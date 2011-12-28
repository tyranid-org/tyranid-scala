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

import net.liftweb.http.{ FileParamHolder, SHtml }

import org.tyranid.Bind
import org.tyranid.Imp._
import org.tyranid.cloud.aws.{ S3, S3Bucket }
import org.tyranid.db.{ Domain, Record, Scope }
import org.tyranid.ui.Field

object DbFile {

  def apply( bucketPrefix:String ):DbFile = new DbFile( Bind.S3Buckets( bucketPrefix ) )
}

class DbFile( bucket:S3Bucket ) extends Domain {
  val sqlName = "TEXT"  // TODO

  protected def save( r:Record, f:Field )( fp:FileParamHolder ) =
    
//    val fileObj = r.o( f.va )
//    fileObj( 'name ) = dsf
//    fileObj( 'descriptipon )
    
    fp.file match {
    case null =>
    case x if x.length == 0 =>
    case x =>
      val extension = fp.fileName.suffix( '.' ).replace( " ", "_" ).replace( "\\\\", "" ).replace( "\\", "/" )
      r.recordTid match {
      case null | "null" | "-invalid" => r.save
      case _ =>
      }
      val path = r.entityTid + "/" + r.recordTid + "/" + f.va.att.name + "." + extension
      
      println( "PATH: " + path );
      S3.write( bucket, path, fp.mimeType, x )
      S3.access( bucket, path, public = true )

      r( f.va ) = bucket.url( path )
    }

  def url( path:String ) = {
    if ( path.startsWith( "http" ) ) {
      path
    } else {
      bucket.url( path )
    }
  }

  override def ui( s:Scope, f:Field, opts:(String,String)* ): NodeSeq =
    /*SHtml.text( s.rec s f.va, v => s.rec( f.va ) = v, "class" -> "textInput" ) ++ */
    <div class='thumbnail'>
      { 
      if ( ( s.rec s f.va ).isBlank ) { // TODO:  Replace this with a blank/default image for ALL images
//        <img src='http://d33lorp9dhlilu.cloudfront.net/generic-image.png' style='float:left;'/>
      } else {
//        <img src={ url( s.rec s f.va ) } style='float:left;'/>
      }
      }
      <div> { SHtml.fileUpload( save( s.rec, f ) _ ) }</div>
    </div>
}

object DbLocalFile extends Domain {
  val sqlName = "TEXT"

  protected def save( r:Record, f:Field )( fp:FileParamHolder ) =
    fp.file match {
    case null =>
    case x if x.length == 0 =>
    case x =>
      val tmpName = "/tmp/" + System.currentTimeMillis + "_" + fp.fileName
      var fops = new FileOutputStream( new java.io.File( tmpName ) )
      IOUtils.transfer( fp.fileStream, fops )
      fops.close()
      r( f.va ) = tmpName
    }

  override def ui( s:Scope, f:Field, opts:(String,String)* ): NodeSeq =
    <div class='thumbnail'><div> { SHtml.fileUpload( save( s.rec, f ) _ ) }</div></div>
}

object File {
}


