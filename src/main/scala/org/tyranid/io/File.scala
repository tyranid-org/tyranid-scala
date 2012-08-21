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

import org.jsoup.Jsoup

import java.io.{ IOException, FileOutputStream, FileInputStream, InputStream, OutputStream, File => SysFile }
import java.net.URL

import javax.swing.text._
import javax.swing.text.rtf.RTFEditorKit

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
                                        new TxtTextExtractor(),
                                        new HtmlTextExtractor(),
                                        new RtfTextExtractor(),
                                        new OpenOfficeTextExtractor()
                                     )
                                     
  def findByFilename( filename:String ) = {
    val ext = filename.suffix( '.' ).toLowerCase
    
    extractors.find( e => e.extTypes.contains( ext ) ).getOrElse( null )
  }
  
  def extract( file:SysFile ):String = {
    val filename = file.getName
    val extractor = findByFilename( filename )
    ( extractor == null ) ? "" | extractor.extract( file )
  } 
}

class TxtTextExtractor extends TextExtractor {
  override val mimeTypes = Seq( "text/plain" )
  override val extTypes = Seq( "txt", "text" )
  
  override def extract( file:SysFile ) = 
    ( file != null && file.exists ) ? new FileInputStream( file ).asString | ""
}

class HtmlTextExtractor extends TextExtractor {
  override val mimeTypes = Seq( "text/html" )
  override val extTypes = Seq( "htm", "html" )
  
  override def extract( file:SysFile ) = 
    ( file != null && file.exists ) ? Jsoup.parse( new FileInputStream( file ).asString ).text() | ""
}

class RtfTextExtractor extends TextExtractor {
  override val mimeTypes = Seq( "application/rtf", "text/richtext" )
  override val extTypes = Seq( "rtf", "rtx" )
  
  override def extract( file:SysFile ) = {
    val styledDoc = new DefaultStyledDocument()
    var is:InputStream = null 
    
    try {
      is = new FileInputStream( file )
      new RTFEditorKit().read( is, styledDoc, 0 )
      styledDoc.getText( 0, styledDoc.getLength() )
    } catch {
      case e =>
        e.printStackTrace
        ""
    } finally {
      if ( is != null )
        is.close()
    }
  }
}

// Open Office - Should cover all Open Office products
class OpenOfficeTextExtractor extends TextExtractor {
  override val mimeTypes = Seq(
    "application/oda",
    "application/vnd.oasis.opendocument.database",
    "application/vnd.oasis.opendocument.image",
    "application/vnd.oasis.opendocument.chart",  
    "application/vnd.oasis.opendocument.formula",  
    "application/vnd.oasis.opendocument.graphics",
    "application/vnd.oasis.opendocument.text-master",  
    "application/vnd.oasis.opendocument.presentation",
    "application/vnd.oasis.opendocument.spreadsheet",
    "application/vnd.oasis.opendocument.text", 
    "application/vnd.oasis.opendocument.graphics-template",   
    "application/vnd.oasis.opendocument.text-web",
    "application/vnd.oasis.opendocument.text-template",   
    "application/vnd.oasis.opendocument.presentation-template",
    "application/vnd.oasis.opendocument.spreadsheet-template", 
    "application/vnd.openofficeorg.extension" )
  override val extTypes = Seq(     "oda", "odb", "odi", "odc", "odf", "odg", "odm", "odp", "ods", "odt", "otg", "oth", "ott", "otp",  "ots", "oxt" )
  
  override def extract( file:SysFile ) = OpenOfficeParser.getText( file.getPath() )
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
  val mimeTypeMap = Map( 
    "aif"      -> "audio/x-aiff",
    "aifc"     -> "audio/x-aiff",
    "aiff"     -> "audio/x-aiff",
    "au"       -> "audio/basic",
    "avi"      -> "video/x-msvideo",
      
    "bcpio"    -> "application/x-bcpio",
    "bin"      -> "application/octet-stream",

    "cdf"      -> "application/x-netcdf",
    "cpio"     -> "application/x-cpio",
    "csh"      -> "application/x-csh",

    "doc"      -> "application/msword",
    "docm"     -> "application/vnd.ms-word.document.macroEnabled.12",
    "docx"     -> "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    "dot"      -> "application/msword",
    "dotx"     -> "application/vnd.openxmlformats-officedocument.wordprocessingml.template",
    "dotm"     -> "application/vnd.ms-word.template.macroEnabled.12",
    "dvi"      -> "application/x-dvi",
    
    "eps"      -> "application/postscript",

    "gif"      -> "image/gif",
    "gtar"     -> "application/x-gtar",
    
    "hdf"      -> "application/x-hdf",
    "htm"      -> "text/html",
    "html"     -> "text/html",

    "jpe"      -> "image/jpeg",
    "jpeg"     -> "image/jpeg",
    "jpg"      -> "image/jpeg",
    
    "latex"    -> "application/x-latex",

    "man"      -> "application/x-troff-man",
    "me"       -> "application/x-troff-me",
    "mif"      -> "application/x-mif",
    "mov"      -> "video/quicktime",
    "movie"    -> "video/x-sgi-movie",

    "mp4"      -> "video/mp4",
    "mpe"      -> "video/mpeg",
    "mpeg"     -> "video/mpeg",
    "mpg"      -> "video/mpeg",
    "mpp"      -> "application/vnd.ms-project",
    "ms"       -> "application/x-troff-ms",
    
    "nc"       -> "application/x-netcdf",
    
    "oda"      -> "application/oda",
    "odb"      -> "application/vnd.oasis.opendocument.database",
    "odi"      -> "application/vnd.oasis.opendocument.image",
    "odc"      -> "application/vnd.oasis.opendocument.chart",  
    "odf"      -> "application/vnd.oasis.opendocument.formula",  
    "odg"      -> "application/vnd.oasis.opendocument.graphics",
    "odm"      -> "application/vnd.oasis.opendocument.text-master",  
    "odp"      -> "application/vnd.oasis.opendocument.presentation",
    "ods"      -> "application/vnd.oasis.opendocument.spreadsheet",
    "odt"      -> "application/vnd.oasis.opendocument.text", 
    "otg"      -> "application/vnd.oasis.opendocument.graphics-template",   
    "oth"      -> "application/vnd.oasis.opendocument.text-web",
    "ott"      -> "application/vnd.oasis.opendocument.text-template",   
    "otp"      -> "application/vnd.oasis.opendocument.presentation-template",
    "ots"      -> "application/vnd.oasis.opendocument.spreadsheet-template", 
    "oxt"      -> "application/vnd.openofficeorg.extension",
    
    "pbm"      -> "image/x-portable-bitmap",
    "pdf"      -> "application/pdf",
    "pgm"      -> "image/x-portable-graymap",
    "png"      -> "image/png",
    "pnm"      -> "image/x-portable-anymap",
    "pot"      -> "application/vnd.ms-powerpoint",
    "potm"     -> "application/vnd.ms-powerpoint.template.macroEnabled.12",
    "potx"     -> "application/vnd.openxmlformats-officedocument.presentationml.template",
    "ppa"      -> "application/vnd.ms-powerpoint",
    "ppam"     -> "application/vnd.ms-powerpoint.addin.macroEnabled.12",
    "ppm"      -> "image/x-portable-pixmap",
    "ppt"      -> "application/vnd.ms-powerpoint",
    "pps"      -> "application/vnd.ms-powerpoint",
    "ppsm"     -> "application/vnd.ms-powerpoint.slideshow.macroEnabled.12",
    "pptm"     -> "application/vnd.ms-powerpoint.presentation.macroEnabled.12",
    "ppsx"     -> "application/vnd.openxmlformats-officedocument.presentationml.slideshow",
    "pptx"     -> "application/vnd.openxmlformats-officedocument.presentationml.presentation",
    "ps"       -> "application/postscript",
    
    "qt"       -> "video/quicktime",
    
    "ras"      -> "image/x-cmu-raster",
    "rgb"      -> "image/x-rgb",
    "roff"     -> "application/x-troff",
    "rtf"      -> "application/rtf",
    "rtx"      -> "text/richtext",
    
    "sgm"      -> "text/x-sgml",
    "sgml"     -> "text/x-sgml",
    "sh"       -> "application/x-sh",
    "shar"     -> "application/x-shar",
    "snd"      -> "audio/basic",
    "src"      -> "application/x-wais-source",

    "t"        -> "application/x-troff",
    "tar"      -> "application/x-tar",
    "tcl"      -> "application/x-tcl",
    "tex"      -> "application/x-tex",
    "texinfo"  -> "application/x-texinfo",
    "texi"     -> "application/x-texinfo",
    "text"     -> "text/plain",
    "tif"      -> "image/tiff",
    "tiff"     -> "image/tiff",
    "tr"       -> "application/x-troff",
    "tsv"      -> "text/tab-separated-values",
    "txt"      -> "text/plain",
    
    "wav"      -> "audio/x-wav",
    
    "xbm"      -> "image/x-xbitmap",
    "xla"      -> "application/vnd.ms-excel",
    "xlam"     -> "application/vnd.ms-excel.addin.macroEnabled.12",
    "xlsm"     -> "application/vnd.ms-excel.sheet.macroEnabled.12",
    "xltm"     -> "application/vnd.ms-excel.template.macroEnabled.12",
    "xlsb"     -> "application/vnd.ms-excel.sheet.binary.macroEnabled.12",
    "xls"      -> "application/vnd.ms-excel",
    "xlsx"     -> "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    "xlt"      -> "application/vnd.ms-excel",
    "xltx"     -> "application/vnd.openxmlformats-officedocument.spreadsheetml.template",
    "xml"      -> "text/xml",
    "xpm"      -> "image/x-xpixmap",
    "xsl"      -> "text/xml",
    "xwd"      -> "image/x-xwindowdump",
    
    "zip"      -> "application/x-zip-compressed"
  )
  
  def getExtension( mimetype:String ) = {
    mimeTypeMap.entrySet().find( _.getValue() == mimetype ).flatten( _.getKey(), null )
  }
  
  def mimeTypeFor( filename:String ) = {
    val fsave = filename.safeString.toLowerCase
    val ext = filename.suffix( '.' )
    
    mimeTypeMap.get( ext ).getOrElse( null )
  }
  
  def safeExtension( filename:String ) = {
    val max = scala.math.max( filename.lastIndexOf( '/' ), filename.lastIndexOf( '.' ) ) 
    val suffix = if ( max != -1 ) filename.substring( max+1 ) else "" 
    suffix.replace( " ", "_" ).replace( "\\\\", "" ).replace( "\\", "/" )
  }

  def isImage( filename:String ) = {
    filename.toLowerCase.suffix( '.' ) match {
    case "jpg" | "jpeg" | "png" | "gif" => true
    case _     => false
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
      case e if e.getClass.getSimpleName == "EofException" =>
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

