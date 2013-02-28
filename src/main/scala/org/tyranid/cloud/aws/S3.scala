/**
 * Copyright (c) 2008-2012 Tyranid <http://tyranid.org>
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
import com.amazonaws.services.s3.model.{ AmazonS3Exception, GeneratePresignedUrlRequest, GroupGrantee, ObjectMetadata, Permission, S3Object, GetObjectRequest, ListObjectsRequest, DeleteObjectsRequest }
import com.mongodb.DBObject
import org.tyranid.Imp._
import org.tyranid.net.Uri
import org.apache.commons.io.IOUtils
import com.amazonaws.services.s3.model.DeleteObjectRequest

case class S3StoreResult( url:String, mimeType:String )

case class S3Bucket( prefix:String, cfDistributionId:String = "", cfDomain:String = "", keyPairId:String= "" ) {
  val name = prefix + B.envSuffix + B.bucketSuffix

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
  
  private val s3 = new AmazonS3Client( B.awsCredentials )

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
  
  def write( bucket:S3Bucket, key:String, file:java.io.File, public:Boolean = false, authorized:Boolean = false ) = {
    val mimeType = new FileInputStream( file ).detectMimeType( file.getName )
    
    if ( mimeType.notBlank ) {
      val md = new ObjectMetadata
      md.setContentLength( file.length )
      md.setContentType( mimeType )
      md.setLastModified( new java.util.Date() )

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
  
  def write( bucket:S3Bucket, key:String, mimeType:String, data:Array[Byte] ) = {
    val md = new ObjectMetadata
    md.setContentLength( data.length )
    md.setContentType( mimeType )

    val is = new ByteArrayInputStream( data )

    try {
      /* val putObjectResult = */ s3.putObject( bucket.name, key, is, md )
    } finally {
      is.close
    }
  }
  
  def write( bucket:S3Bucket, key:String, contentLength:Long, contentType:String, in:InputStream ) = {
    val md = new ObjectMetadata
    
    if ( contentLength != -1 )
      md.setContentLength( contentLength )
      
    md.setContentType( contentType )

    try {
      s3.putObject( bucket.name, key, in, md )
    } finally {
      in.close
    }
    
    md
  }
  
  def write( bucket:S3Bucket, key:String, file:org.apache.commons.fileupload.FileItem ):Unit =
    write( bucket, key, file.getSize, file.getContentType, file.getInputStream )

  def delete( bucket:S3Bucket, key:String ) = s3.deleteObject( bucket.name, key )
  
  def deleteAll( bucket:S3Bucket, keys:Seq[String], keyPrefix:String = null ) = {
    for ( key <- keys )
      delete( bucket, keyPrefix.isBlank ? key | ( keyPrefix + key ) )
  }
  
  def copy( bucket:S3Bucket, key:String, bucket2:S3Bucket, key2:String ) = s3.copyObject( bucket.name, key, bucket2.name, key2 )

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
    val listing = s3.listObjects( new ListObjectsRequest().withBucketName( bucket.name ).withPrefix( prefix ) )
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
      s3.getObject( bucket.name, key ) != null
    } catch {
      case e:com.amazonaws.AmazonClientException =>
        false
    }
  }
  
  def getObject( bucket:S3Bucket, key:String ) = s3.getObject( new GetObjectRequest( bucket.name, key ) )
  def getObjectMetadata( bucket:S3Bucket, key:String ) = s3.getObjectMetadata( bucket.name, key )

  def storeUrl( bucket:S3Bucket, urlStr:String, path:String, isPublic:Boolean = true ):S3StoreResult = {
    val url = new java.net.URL( urlStr )
    val conn = url.openConnection
    val in = conn.getInputStream
    val mimeType = conn.getContentType
    
    S3.write( bucket, path, conn.getContentLength, mimeType, in )
    in.close
      
    S3.access( bucket, path, public = isPublic )
    
    S3StoreResult( bucket.url( path ), mimeType )
  }

  def storeUnsecureUrl( bucket:S3Bucket, urlStr:String, path:String, isPublic:Boolean = true ):String = {
    if ( !Uri.isSecure( urlStr ) )
      storeUrl( bucket, urlStr, path, isPublic ).url
    else
      urlStr
  }
  
  def listObjects( bucket:S3Bucket, path:String ):Seq[String] = s3.listObjects( bucket.name, path ).getObjectSummaries.map( _.getKey )
}


