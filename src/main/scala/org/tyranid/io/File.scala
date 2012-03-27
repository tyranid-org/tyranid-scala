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

package org.tyranid.io

import java.io.{ IOException, FileOutputStream, InputStream, OutputStream }
import java.net.URL

import scala.collection.mutable
import scala.collection.JavaConversions._
import scala.xml.NodeSeq

import org.tyranid.Imp._
import org.tyranid.cloud.aws.{ S3, S3Bucket }
import org.tyranid.db.{ Domain, Record, Scope }
import org.tyranid.ui.PathField

import org.tyranid.web.{ FileUploadSupport, FileMultiParams }
import org.tyranid.web.FileUploadSupport.BodyParams

object DbFile {

  def apply( bucketPrefix:String ):DbFile = new DbFile( B.getS3Bucket( bucketPrefix ) )
}

class DbFile( bucket:S3Bucket ) extends CommonFile {
  val sqlName = "TEXT"  // TODO

  override def extract( s:Scope, f:PathField ) {
    val bodyParams:BodyParams = T.web.req.getAttribute( FileUploadSupport.BodyParamsKey ).asInstanceOf[BodyParams]

    val fileItem = 
      if ( bodyParams != null ) {
        val fileItems = bodyParams.fileParams.get( f.va.name )
        
        if ( fileItems.size > 0 )
          fileItems.get(0)
        else 
          null
      } else 
        null

    if ( fileItem != null && fileItem.getName().notBlank ) {
      val r = s.rec
      r.recordTid match {
      case null | "null" | "-invalid" => r.save
      case _ =>
      }
      
      val path = File.pathFor( r.entityTid, r.recordTid, f.va.att.name, fileItem.getName() )
      var in = fileItem.getInputStream()
      
      S3.write( bucket, path, fileItem.getSize(), fileItem.getContentType(), in )
      in.close
      
      S3.access( bucket, path, public = true )
      
      r( f.va ) = bucket.url( path )
    }
  }
  
  def url( path:String ) = {
    if ( path.startsWith( "http" ) ) {
      path
    } else {
      bucket.url( path )
    }
  }
}

object DbLocalFile extends CommonFile {
  val sqlName = "TEXT"

  override def extract( s:Scope, f:PathField ) {
    val bodyParams:BodyParams = T.web.req.getAttribute( FileUploadSupport.BodyParamsKey ).asInstanceOf[BodyParams]

    val fileItem = 
      if ( bodyParams != null ) {
        val fileItems = bodyParams.fileParams.get( f.va.name )
        
        if ( fileItems.size > 0 )
          fileItems.get(0)
        else 
          null
      } else 
        null

    if ( fileItem != null ) {
      val tmpName = "/tmp/" + System.currentTimeMillis + "_" + fileItem.getName()
      var fops = new FileOutputStream( new java.io.File( tmpName ) )
      var in = fileItem.getInputStream
      in.transferTo( fops )
      in.close
      fops.close
      
      s.rec( f.va ) = tmpName
    }
  }
}

object File {
  def pathFor( entityTid:String, recordTid:String, fieldName:String, url:String ) = {
    val max = scala.math.max( url.lastIndexOf( '/' ), url.lastIndexOf( '.' ) ) 
    val suffix = if ( max != -1 ) url.substring( max+1 ) else "" 
    val extension = suffix.replace( " ", "_" ).replace( "\\\\", "" ).replace( "\\", "/" )
    
    ( entityTid + "/" + recordTid + "/" + fieldName + "." + extension )
  }
}

trait CommonFile extends Domain {
  override def ui( s:Scope, f:PathField ): NodeSeq =
    <div class='thumbnail'>
      { 
//        if ( ( s.rec s f.va ).isBlank ) { // TODO:  Replace this with a blank/default image for ALL images
//        } else {
//        }
        
        <div><input name={ f.id } type="file"/></div>
      }
    </div>
}

