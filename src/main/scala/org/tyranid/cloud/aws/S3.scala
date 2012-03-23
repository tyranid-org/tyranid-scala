/**
 * Copyright (c) 2008-2011 Tyranid <http://tyranid.org>
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

import java.io.{ ByteArrayInputStream, FileOutputStream, InputStream }

import com.amazonaws.services.s3.AmazonS3Client
import com.amazonaws.services.s3.model.{ AmazonS3Exception, GroupGrantee, ObjectMetadata, Permission, S3Object, GetObjectRequest }

import com.mongodb.DBObject

import org.tyranid.Imp._


case class S3Bucket( prefix:String, cfDistributionId:String = "", cfDomain:String = "" ) {
  val name = prefix + B.envSuffix + B.bucketSuffix

  def url( path:String ) =
    if ( cfDomain.isBlank || B.envSuffix.notBlank )
      "https://s3.amazonaws.com/" + name + "/" + path
    else
      "https://" + cfDomain + ".cloudfront.net/" + path
}

object S3 {
  private val s3 = new AmazonS3Client( B.awsCredentials )

  def write( bucket:S3Bucket, key:String, file:java.io.File ) = s3.putObject( bucket.name, key, file )
  
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
      /* val putObjectResult = */ s3.putObject( bucket.name, key, in, md )
    } finally {
      in.close
    }
  }
  
  def delete( bucket:S3Bucket, key:String ) = s3.deleteObject( bucket.name, key )
  def copy( bucket:S3Bucket, key:String, bucket2:S3Bucket, key2:String ) = s3.copyObject( bucket.name, key, bucket2.name, key2 )

  def access( bucket:S3Bucket, key:String, public:Boolean ) = {
    try {
      val acl = s3.getObjectAcl( bucket.name, key )

      acl.revokeAllPermissions( GroupGrantee.AllUsers )
      
      if ( public )
        acl.grantPermission( GroupGrantee.AllUsers, Permission.Read )

      s3.setObjectAcl( bucket.name, key, acl )
    } catch {
      case e:AmazonS3Exception =>
        log( Event.StackTrace, "m" -> "S3 Exception", "ex" -> e )
    }
  }
  
  def download( bucket:S3Bucket, key:String ) = {
	var obj:S3Object = null
	
	try {
      obj = s3.getObject( new GetObjectRequest( bucket.name, key ) )

	  //println( "Content-Type: "  + object.getObjectMetadata().getContentType() );
		
      if ( obj != null ) {
		  val tmpName = "/tmp/" + System.currentTimeMillis + "_" + key
		  val f = new java.io.File( tmpName )
			
		  //f.split( "/" )
		  //f.mkdirs()
			
		  var fops = new FileOutputStream( f )
		  obj.getObjectContent.transferTo( fops )
		  fops.close
		  f
      }
	} catch {
    case e: AmazonS3Exception =>
      e.log
      null
    }
  }
  
  def getInputStream( bucket:S3Bucket, key:String ) = {
	try {
      val obj = s3.getObject( new GetObjectRequest( bucket.name, key ) )
      
      if ( obj != null )
        obj.getObjectContent()

	} catch {
    case e: AmazonS3Exception =>
      e.log
      null
    }
  }
  
  def getObject( bucket:S3Bucket, key:String ) = {
    s3.getObject( new GetObjectRequest( bucket.name, key ) )
  }  

  def storeUrl( bucket:S3Bucket, urlStr:String, path:String ) = {
    val url = new java.net.URL( urlStr )
    val conn = url.openConnection
    val in = conn.getInputStream
    
    S3.write( bucket, path, conn.getContentLength, conn.getContentType(), in )
    in.close
      
    S3.access( bucket, path, public = true )
    
    bucket.url( path )
  }
}
