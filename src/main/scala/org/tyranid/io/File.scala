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

import org.apache.tika.detect.{ DefaultDetector, Detector }
import org.apache.tika.io.TikaInputStream
import org.apache.tika.metadata.Metadata
import org.apache.tika.parser.{ AutoDetectParser, Parser, ParseContext }
import org.apache.tika.parser.html.HtmlParser
import org.apache.tika.sax.BodyContentHandler

import java.io.{ IOException, FileOutputStream, FileInputStream, InputStream, OutputStream, File => SysFile, ByteArrayOutputStream }
import java.net.URL

import scala.collection.mutable
import scala.collection.JavaConversions._
import scala.xml.{ NodeSeq, Unparsed }

import org.tyranid.Imp._
import org.tyranid.cloud.aws.{ S3, S3Bucket }
import org.tyranid.db.{ Domain, Record, Scope }
import org.tyranid.document.OpenOfficeParser
import org.tyranid.ui.PathField
import org.tyranid.web.WebContext


trait HasText {
  def text:String
}

abstract class TextExtractor {
  val mimeTypes:Seq[String]
  val extTypes:Seq[String]  
  def extract( file:SysFile ):String
}

object TextExtractors {
  val extractors = Seq[TextExtractor]( 
      new HtmlTextExtractor() )
                                     
  def findByFilename( filename:String ) = {
    val ext = filename.suffix( '.' ).toLowerCase
    extractors.find( e => e.extTypes.contains( ext ) ).getOrElse( null )
  }
  
  def extract( file:SysFile ):String = {
    val filename = file.getName
    val extractor = findByFilename( filename )
    ( extractor != null ) ? extractor.extract( file ) | new TikaExtractor().extract( file )
  } 
}

class TikaExtractor extends TextExtractor {
  override val mimeTypes = null
  override val extTypes = null
  
  override def extract( file:SysFile ) = {
    val parser = new AutoDetectParser( new DefaultDetector() )
    val context =  new ParseContext()
    
    context.set( classOf[Parser], parser )
  
    val out = new ByteArrayOutputStream()
    var is:TikaInputStream = null
    
    try {
      is = TikaInputStream.get( file )
      parser.parse( is, new BodyContentHandler( out ), new Metadata(), context ) 
      out.toString
    } finally {
      is.close()
    } 
  }
}

class HtmlTextExtractor extends TextExtractor {
  override val mimeTypes = Seq( "text/html" )
  override val extTypes = Seq( "htm", "html" )
  
  override def extract( file:SysFile ) = {
    var is:InputStream = null
   
    try {
      is = new FileInputStream( file )
      val handler = new BodyContentHandler()
      new HtmlParser().parse( is, handler, new Metadata(), new ParseContext() )
      handler.toString()    
    } finally {
      is.close
    }
  }
}

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
  
  def url( path:String ) = path.startsWith( "http" ) ? path | bucket.url( path )
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
  val tempBucket = new S3Bucket( "temp" )

  
  def getExtension( mimetype:String ) = {
    MimeType.byExtension.entrySet().find( _.getValue() == mimetype ).flatten( _.getKey(), null )
  }
  
  def mimeTypeFor( filename:String ) = {
    val fsave = filename.safeString.toLowerCase
    val ext = filename.suffix( '.' )
    
    MimeType.byExtension.get( ext ).getOrElse( null )
  }
  
  def safeExtension( filename:String ) = {
    val max = scala.math.max( filename.lastIndexOf( '/' ), filename.lastIndexOf( '.' ) ) 
    val suffix = if ( max != -1 ) filename.substring( max+1 ) else "" 
    suffix.replace( " ", "_" ).replace( "\\\\", "" ).replace( "\\", "/" )
  }

  def isImage( filename:String ) = {
    filename.toLowerCase.suffix( '.' ) match {
    case "jpe" | "jpg" | "jpeg" | "png" | "gif" => true
    case _                                      => false
    }
  }

  def isVideo( filename:String ) = {
    filename.toLowerCase.suffix( '.' ) match {
    case "mp4" => true
    case _     => false
    }
  }

  def pathFor( entityTid:String, recordTid:String, pathName:String, url:String ) =
    entityTid + "/" + recordTid + "/" + pathName + "." + safeExtension( url )

  def download( web:WebContext, bucket:S3Bucket, key:String, fileName:String ) {
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
      case e if e.getClass.getSimpleName == "EofException" || e.getMessage == "Broken pipe" =>
        println( "*** Broken pipe" )
      case ex =>
        ex.log
      } finally {
        in.close
      }
    } else {
      throw new RuntimeException( "File not found." )
    }
  }
}

trait CommonFile extends Domain {
  override def ui( s:Scope, f:PathField ): NodeSeq = {
    <div class="thumbnailImg">

      { 
        val url = s.s( f )
        
        if ( url notBlank )
          <a href="#" onclick={ Unparsed( "downloadFile( '" + url + "', event, 'vFileWindow'); return false;")}>Download File</a>
      }
        
      <div><input id={ f.id } name={ f.id } type="file"/></div>
    </div>
  }
}

