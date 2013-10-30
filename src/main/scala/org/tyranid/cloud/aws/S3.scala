/**
 * Copyright (c) 2008-2013 Tyranid <http://tyranid.org>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.tyranid.cloud.aws

import scala.collection.JavaConversions._

import java.util.Date
import java.io.{ ByteArrayInputStream, FileOutputStream, FileInputStream, InputStream, File }
import scala.collection.mutable
import scala.collection.mutable.Buffer
import org.jets3t.service.CloudFrontService
import org.jets3t.service.utils.ServiceUtils

import com.amazonaws.services.s3.AmazonS3Client
import com.amazonaws.services.s3.model.{ AmazonS3Exception, GeneratePresignedUrlRequest, DeleteObjectRequest, GroupGrantee, ObjectMetadata, Permission, S3Object, GetObjectRequest, ListObjectsRequest, DeleteObjectsRequest, CopyObjectRequest }

import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.net.Uri
import org.apache.commons.io.IOUtils

case class S3StoreResult( url:String, mimeType:String )

case class S3Bucket( prefix:String, cfDistributionId:String = "", cfDomain:String = "", keyPairId:String= "", fullName:String = null ) {
  val name = fullName.isBlank ? ( prefix + B.envSuffix + B.bucketSuffix ) | fullName

  def url( path:String, forceS3:Boolean = false ) =
    if ( forceS3 || ( cfDomain.isBlank || B.envSuffix.notBlank ) )
      "https://s3.amazonaws.com/" + name + "/" + path
    else
      "https://" + cfDomain + ".cloudfront.net/" + path
      
  def file( path:String ) = S3.getFile( this, path )  
}

object S3 {
  private val derPrivateKey = {
    val derFile = new File( "/etc/cf-default.der" )
    
    if ( !derFile.exists ) 
      throw new RuntimeException( derFile.getAbsolutePath() + " does not exist!" );
    
    ServiceUtils.readInputStreamToBytes( new FileInputStream( derFile ) )
  }
  
  val s3 = new AmazonS3Client( B.awsCredentials )

  def signedUrl( bucket:S3Bucket, key:String, expireMinutes:Int = 60 ) = {
    val expiration = new Date().add( java.util.Calendar.MINUTE, expireMinutes )
      
    if ( bucket.cfDomain != null ) {
      CloudFrontService.signUrlCanned(
          "https://" + bucket.cfDomain + ".cloudfront.net/" + key,
          bucket.keyPairId,
          derPrivateKey,
          expiration
      )
    } else {
      val req = new GeneratePresignedUrlRequest( bucket.name, key )
      req.setExpiration( expiration )
      s3.generatePresignedUrl( req )._s
    }
  }
  
  def write( bucket:S3Bucket, key:String, file:java.io.File, public:Boolean = false, authorized:Boolean = false, acceptRanges:Boolean = false, encrypt:Boolean = false ) = {
    val mimeType = new FileInputStream( file ).detectMimeType( file.getName )
    
    if ( mimeType.notBlank ) {
      val md = new ObjectMetadata
      md.setContentLength( file.length )
      md.setContentType( mimeType )
      md.setLastModified( new Date )
      
      if ( encrypt )
        md.setServerSideEncryption( ObjectMetadata.AES_256_SERVER_SIDE_ENCRYPTION )

      if ( acceptRanges )
        md.setHeader( "Accept-Ranges", "bytes" ) 

      val in = new FileInputStream( file )
      
      try {
        s3.putObject( bucket.name, key, in, md )
        
        if ( public || authorized )
          access( bucket, key, public, authorized )
      } finally {
        in.close
      }
    } else {
      s3.putObject( bucket.name, key, file )
    }
  }
  
  def write( bucket:S3Bucket, key:String, mimeType:String, data:Array[Byte], encrypt:Boolean ) = {
    val md = new ObjectMetadata
    md.setContentLength( data.length )
    md.setContentType( mimeType )
    
    if ( encrypt )
      md.setServerSideEncryption( ObjectMetadata.AES_256_SERVER_SIDE_ENCRYPTION )

    val is = new ByteArrayInputStream( data )

    try {
      /* val putObjectResult = */ s3.putObject( bucket.name, key, is, md )
    } finally {
      is.close
    }
  }
  
  def write( bucket:S3Bucket, key:String, contentLength:Long, contentType:String, in:InputStream, public:Boolean, acceptRanges:Boolean, encrypt:Boolean ) = {
    val md = new ObjectMetadata
    
    if ( contentLength != -1 )
      md.setContentLength( contentLength )
      
    md.setContentType( contentType )
    
    if ( acceptRanges )
      md.setHeader( "Accept-Ranges", "bytes" ) 

    if ( encrypt )
      md.setServerSideEncryption( ObjectMetadata.AES_256_SERVER_SIDE_ENCRYPTION )
      
    try {
      s3.putObject( bucket.name, key, in, md )
      
        if ( public )
          access( bucket, key, public, false )
      
    } finally {
      in.close
    }
    
    md
  }
  
  def write( bucket:S3Bucket, key:String, file:org.apache.commons.fileupload.FileItem, public:Boolean, acceptRanges:Boolean, encrypt:Boolean ):Unit =
    write( bucket, key, file.getSize, file.getContentType, file.getInputStream, public, acceptRanges, encrypt )

  def delete( bucket:S3Bucket, key:String ) = try {
    s3.deleteObject( bucket.name, key )
  } catch {
    case e:Throwable =>
      println( "Error deleting [" + key + "]" )
      throw e
  }
  
  def findKey( bucket:S3Bucket, name:String ):String = {
    var objectListing = s3.listObjects( new ListObjectsRequest().withBucketName( bucket.name ) )
    var cnt = 0
    val n = name.toLowerCase
    var more = false
    
    do {    
      for ( objectSummary <- objectListing.getObjectSummaries() ) {
        val key = objectSummary.getKey()
        
        println( key )
        
        cnt = cnt + 1
        
        if ( key.toLowerCase.endsWith( n ) )
          return key
      }
      
      more = objectListing.isTruncated
      
      if ( more )
        objectListing = s3.listNextBatchOfObjects( objectListing )
        
    } while ( more )
      
    null
  }
    
        
  def deleteAll( bucket:S3Bucket, keys:Seq[String], keyPrefix:String = null ) = {
    for ( key <- keys )
      delete( bucket, keyPrefix.isBlank ? key | ( keyPrefix + key ) )
  }
  
  def copy( sourceBucket:S3Bucket, sourceKey:String, targetBucket:S3Bucket, targetKey:String, encrypt:Boolean = false ) = try {
    val copyObjRequest = new CopyObjectRequest( sourceBucket.name, sourceKey, targetBucket.name, targetKey )
            
    if ( encrypt ) {
      val md = new ObjectMetadata()
      md.setServerSideEncryption( ObjectMetadata.AES_256_SERVER_SIDE_ENCRYPTION ) 
              
      copyObjRequest.setNewObjectMetadata(md)
    }
    
    s3.copyObject( copyObjRequest )
  } catch {
    case e:Throwable =>
      println( "Error copying from [" + sourceKey + "] to [" + targetKey + "]" )
      throw e
  }

  def move( bucketFrom:S3Bucket, fromPath:String, bucketTo:S3Bucket, toPath:String ) = {
    copy( bucketFrom, fromPath, bucketTo, toPath )
    delete( bucketFrom, fromPath )
  }

  def access( bucket:S3Bucket, key:String, public:Boolean = false, authenticated:Boolean = false ) = {
    try {
      val acl = s3.getObjectAcl( bucket.name, key )

      acl.revokeAllPermissions( GroupGrantee.AllUsers )
      
      if ( public )
        acl.grantPermission( GroupGrantee.AllUsers, Permission.Read )
        
      if ( authenticated )
        acl.grantPermission( GroupGrantee.AuthenticatedUsers, Permission.Read )

      s3.setObjectAcl( bucket.name, key, acl )
    } catch {
      case e:AmazonS3Exception =>
        log( Event.StackTrace, "m" -> ( "S3 Exception for bucket " + bucket.name + " and key " + key ), "ex" -> e )
    }
  }
  
  def getInputStream( bucket:S3Bucket, key:String ):InputStream = {
	  try {
      val obj = s3.getObject( new GetObjectRequest( bucket.name, key ) )
      
      if ( obj != null )
        return obj.getObjectContent()
	  } catch {
      case e: AmazonS3Exception =>
        e.log
    }
	  
	  null
  }
  
  def getFile( bucket:S3Bucket, key:String, ext:String = ".tmp" ): File = {
     val tmpFile = File.createTempFile( "tmp", ext )
     val in = getInputStream( bucket, key )
     
     if ( in != null )
       in.transferTo( new FileOutputStream( tmpFile ) )
     
     tmpFile
  }

  def getFilenames( bucket:S3Bucket, prefix:String = null, suffix:String = null, olderThan:Date = null ) = {
    val listing = prefix.isBlank ? s3.listObjects( new ListObjectsRequest().withBucketName( bucket.name ) ) | s3.listObjects( new ListObjectsRequest().withBucketName( bucket.name ).withPrefix( prefix ) )
    val summaries = listing.getObjectSummaries
    val iterator = summaries.iterator
    val filenames = mutable.ArrayBuffer[String]()
    
    while ( iterator.hasNext ) {
      val o = iterator.next
      val s = o.getKey
      
      if ( ( suffix.isBlank || s.endsWith( suffix ) ) && ( olderThan == null || o.getLastModified.getTime < olderThan.getTime ) )
        filenames += s
    }
    
    filenames
  }

  def exists( bucket:S3Bucket, key:String ) = {
    try {
      val obj = s3.getObject( bucket.name, key )
      
      if ( obj != null )
        obj.getObjectContent().close()
      
      true
    } catch {
      case e:com.amazonaws.AmazonClientException =>
        false
    }
  }

  def getObject( bucket:S3Bucket, key:String ) = s3.getObject( new GetObjectRequest( bucket.name, key ) )
  def getObjectMetadata( bucket:S3Bucket, key:String ) = s3.getObjectMetadata( bucket.name, key )

  def storeUrl( bucket:S3Bucket, urlStr:String, path:String, isPublic:Boolean = true, acceptRanges:Boolean = false, encrypt:Boolean = false ):S3StoreResult = {
    val url = new java.net.URL( urlStr )
    val conn = url.openConnection
    val in = conn.getInputStream
    val mimeType = conn.getContentType
    
    S3.write( bucket, path, conn.getContentLength, mimeType, in, isPublic, acceptRanges, encrypt )
    in.close
      
    S3StoreResult( bucket.url( path ), mimeType )
  }

  def storeUnsecureUrl( bucket:S3Bucket, urlStr:String, path:String, isPublic:Boolean = true, acceptRanges:Boolean = false, encrypt:Boolean = false ):String = {
    if ( !Uri.isSecure( urlStr ) )
      storeUrl( bucket, urlStr, path, isPublic, acceptRanges, encrypt ).url
    else
      urlStr
  }
  
  def listObjects( bucket:S3Bucket, path:String ):Seq[String] = s3.listObjects( bucket.name, path ).getObjectSummaries.map( _.getKey )
  def listRawObjects( bucket:S3Bucket, path:String ) = s3.listObjects( bucket.name, path )
}


