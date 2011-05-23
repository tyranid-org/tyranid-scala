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

import java.io.ByteArrayInputStream

import com.amazonaws.services.s3.AmazonS3Client
import com.amazonaws.services.s3.model.{ AmazonS3Exception, GroupGrantee, ObjectMetadata, Permission, S3Object }

import org.tyranid.Bind
import org.tyranid.Imp._

case class S3Bucket( prefix:String, cfDistributionId:String = "", cfDomain:String = "" ) {
  val name = prefix + Bind.EnvSuffix + Bind.BucketSuffix

  def url( path:String ) =
    if ( cfDomain.isBlank || Bind.EnvSuffix.notBlank )
      "https://s3.amazonaws.com/" + name + "/" + path
    else
      "https://" + cfDomain + ".cloudfront.net/" + path
}

object S3 {
  private val s3 = new AmazonS3Client( Bind.AwsCredentials )

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

  def delete( bucket:S3Bucket, key:String ) = s3.deleteObject( bucket.name, key )

  def access( bucket:S3Bucket, key:String, public:Boolean ) = {

    try {
      val acl = s3.getObjectAcl( bucket.name, key )

      acl.revokeAllPermissions( GroupGrantee.AllUsers )
      if ( public )
        acl.grantPermission( GroupGrantee.AllUsers, Permission.Read )

      s3.setObjectAcl( bucket.name, key, acl )
    } catch {
      case e:AmazonS3Exception =>
        log( "S3 Exception", e )
    }
  }
}

