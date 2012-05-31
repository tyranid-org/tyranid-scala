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

package org.tyranid.io

import java.io.{ IOException, FileOutputStream, InputStream, OutputStream }
import java.net.URL

import scala.collection.mutable
import scala.collection.JavaConversions._
import scala.xml.{ NodeSeq, Unparsed }

import org.tyranid.Imp._
import org.tyranid.cloud.aws.{ S3, S3Bucket }
import org.tyranid.db.{ Domain, Record, Scope }
import org.tyranid.ui.PathField
import org.tyranid.web.WebContext


object DbFile {

  def apply( bucketPrefix:String ):DbFile = new DbFile( B.getS3Bucket( bucketPrefix ) )
}

class DbFile( bucket:S3Bucket ) extends CommonFile {
  val sqlName = "TEXT"  // TODO

  override def extract( s:Scope, f:PathField ) {
    val file = T.web.file( f.va.name )

    if ( file != null && file.getName.notBlank ) {

      val rootRec     = s.root.rec
      val embeddedRec = s.rec

      rootRec.ensureId

      val pathName = s.fullPath.aidName_( rootRec )

      val path = File.pathFor( rootRec.entityTid, rootRec.recordTid, pathName, file.getName )
      S3.write( bucket, path, file )
      S3.access( bucket, path, public = true )
        
      embeddedRec( f.va ) = bucket.url( path )
    }
  }
  
  def url( path:String ) =
    if ( path.startsWith( "http" ) )
      path
    else
      bucket.url( path )
}

object DbLocalFile extends CommonFile {
  val sqlName = "TEXT"

  override def extract( s:Scope, f:PathField ) {
    val file = T.web.file( f.va.name )

    if ( file != null ) {
      val tmpName = "/tmp/" + System.currentTimeMillis + "_" + file.getName()
      var fops = new FileOutputStream( new java.io.File( tmpName ) )
      var in = file.getInputStream
      in.transferTo( fops )
      in.close
      fops.close
      
      s.rec( f.va ) = tmpName
    }
  }
}

object File {

  def extension( fileName:String ) = {
    val max = scala.math.max( fileName.lastIndexOf( '/' ), fileName.lastIndexOf( '.' ) ) 
    val suffix = if ( max != -1 ) fileName.substring( max+1 ) else "" 
    suffix.replace( " ", "_" ).replace( "\\\\", "" ).replace( "\\", "/" )
  }

  def pathFor( entityTid:String, recordTid:String, pathName:String, url:String ) =
    entityTid + "/" + recordTid + "/" + pathName + "." + extension( url )

  def mimeTypeFor( fileName:String ) = {
    val mimeType = T.web.ctx.getMimeType( fileName )

    if ( mimeType != null )
      mimeType
    else
      extension( fileName ) match {
      case "mp4" => "video/mp4"
      case _     => null
      }
  }

  def download( web:WebContext, bucket:S3Bucket, key:String, fileName:String ) {
    spam( "bucket:" + bucket + " key:" + key )
    val obj = S3.getObject( bucket, key )
  
    if ( obj != null ) {
      var length = 0 
      val mimetype = mimeTypeFor( fileName )
      val res = web.res

//sp-am( "mimeType:" + mimetype )
//web.req.dump
      
      res.setContentType( ( if ( mimetype != null ) mimetype else "application/octet-stream" ) )
      res.setContentLength( obj.getObjectMetadata.getContentLength.asInstanceOf[Int] )
      res.setHeader( "Content-Disposition", "attachment; filename=\"" + fileName + "\"" )
        
      val out = res.getOutputStream
      val in = obj.getObjectContent
     
      try {
        in.transferTo( out )
      
        in.close
        out.flush
        out.close
      } catch {
      //case e if e.getClass.getSimpleName == "EofException" =>
        //println( "*** Broken pipe" )
      case e =>
        e.log
      }
    } else {
      throw new RuntimeException( "File not found." )
    }
  }
}

trait CommonFile extends Domain {
  override def ui( s:Scope, f:PathField ): NodeSeq = {
    <div class='thumbnail'>
      { 
        val url = s.s( f )
        
        if ( url notBlank )
          <a href="#" onclick={ Unparsed( "downloadFile( '" + url + "', event, 'vFileWindow'); return false;")}>Download File</a>
      }
        
      <div><input id={ f.id } name={ f.id } type="file"/></div>
    </div>
  }
}

