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
    "doc"      -> "application/msword",
    "xls"      -> "application/vnd.ms-excel",
    "ppt"      -> "application/vnd.ms-powerpoint",
    "mpp"      -> "application/vnd.ms-project",
    "bin"      -> "application/octet-stream",
    "oda"      -> "application/oda",
    "pdf"      -> "application/pdf",
    "eps"      -> "application/postscript",
    "ps"       -> "application/postscript",
    "rtf"      -> "application/rtf",
    "odt"      -> "application/vnd.oasis.opendocument.text", 
    "ott"      -> "application/vnd.oasis.opendocument.text-template",   
    "oth"      -> "application/vnd.oasis.opendocument.text-web",
    "odm"      -> "application/vnd.oasis.opendocument.text-master",  
    "odg"      -> "application/vnd.oasis.opendocument.graphics",
    "otg"      -> "application/vnd.oasis.opendocument.graphics-template",   
    "odp"      -> "application/vnd.oasis.opendocument.presentation",
    "otp"      -> "application/vnd.oasis.opendocument.presentation-template",
    "ods"      -> "application/vnd.oasis.opendocument.spreadsheet",
    "ots"      -> "application/vnd.oasis.opendocument.spreadsheet-template", 
    "odc"      -> "application/vnd.oasis.opendocument.chart",  
    "odf"      -> "application/vnd.oasis.opendocument.formula",  
    "odb"      -> "application/vnd.oasis.opendocument.database",
    "odi"      -> "application/vnd.oasis.opendocument.image",
    "oxt"      -> "application/vnd.openofficeorg.extension", 
    "mif"      -> "application/x-mif",
    "csh"      -> "application/x-csh",
    "dvi"      -> "application/x-dvi",
    "hdf"      -> "application/x-hdf",
    "latex"    -> "application/x-latex",
    "nc"       -> "application/x-netcdf",
    "cdf"      -> "application/x-netcdf",
    "sh"       -> "application/x-sh",
    "tcl"      -> "application/x-tcl",
    "tex"      -> "application/x-tex",
    "texinfo"  -> "application/x-texinfo",
    "texi"     -> "application/x-texinfo",
    "t"        -> "application/x-troff",
    "tr"       -> "application/x-troff",
    "roff"     -> "application/x-troff",
    "man"      -> "application/x-troff-man",
    "me"       -> "application/x-troff-me",
    "ms"       -> "application/x-troff-ms",
    "src"      -> "application/x-wais-source",
    "zip"      -> "application/x-zip-compressed",
    "bcpio"    -> "application/x-bcpio",
    "cpio"     -> "application/x-cpio",
    "gtar"     -> "application/x-gtar",
    "shar"     -> "application/x-shar",
    "tar"      -> "application/x-tar",
    "au"       -> "audio/basic",
    "snd"      -> "audio/basic",
    "aif"      -> "audio/x-aiff",
    "aiff"     -> "audio/x-aiff",
    "aifc"     -> "audio/x-aiff",
    "wav"      -> "audio/x-wav",
    "gif"      -> "image/gif",
    "png"      -> "image/png",
    "jpg"      -> "image/jpeg",
    "jpe"      -> "image/jpeg",
    "jpeg"     -> "image/jpeg",
    "tif"      -> "image/tiff",
    "tiff"     -> "image/tiff",
    "ras"      -> "image/x-cmu-raster",
    "pnm"      -> "image/x-portable-anymap",
    "pbm"      -> "image/x-portable-bitmap",
    "pgm"      -> "image/x-portable-graymap",
    "ppm"      -> "image/x-portable-pixmap",
    "rgb"      -> "image/x-rgb",
    "xbm"      -> "image/x-xbitmap",
    "xpm"      -> "image/x-xpixmap",
    "xwd"      -> "image/x-xwindowdump",
    "htm"      -> "text/html",
    "html"     -> "text/html",
    "xml"      -> "text/xml",
    "xsl"      -> "text/xml",
    "sgml"     -> "text/x-sgml",
    "sgm"      -> "text/x-sgml",
    "txt"      -> "text/plain",
    "text"     -> "text/plain",
    "rtx"      -> "text/richtext",
    "tsv"      -> "text/tab-separated-values",
    "mp4"      -> "video/mp4",
    "mpg"      -> "video/mpeg",
    "mpe"      -> "video/mpeg",
    "mpeg"     -> "video/mpeg",
    "qt"       -> "video/quicktime",
    "mov"      -> "video/quicktime",
    "avi"      -> "video/x-msvideo",
    "movie"    -> "video/x-sgi-movie"
  )
  

  def extension( filename:String ) = {
    val max = scala.math.max( filename.lastIndexOf( '/' ), filename.lastIndexOf( '.' ) ) 
    val suffix = if ( max != -1 ) filename.substring( max+1 ) else "" 
    suffix.replace( " ", "_" ).replace( "\\\\", "" ).replace( "\\", "/" )
  }

  def pathFor( entityTid:String, recordTid:String, pathName:String, url:String ) =
    entityTid + "/" + recordTid + "/" + pathName + "." + extension( url )

  def mimeTypeFor( filename:String ) =
    mimeTypeMap.get( extension( filename.safeString.toLowerCase ) ).getOrElse( null )

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

