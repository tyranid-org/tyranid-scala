/**
 * Copyright (c) 2008-2014 Tyranid <http://tyranid.org>
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

import scala.annotation.tailrec

import org.apache.commons.fileupload.disk.DiskFileItem

import java.io.{ IOException, FileOutputStream, InputStream, InputStreamReader, OutputStream, File => JFile }
import java.util.HashMap

import com.amazonaws.AmazonClientException
import com.amazonaws.services.s3.model.{ AmazonS3Exception }

import org.xml.sax.helpers.DefaultHandler

import org.apache.tika.metadata.{ Metadata, TikaMetadataKeys, HttpHeaders }
import org.apache.tika.mime.MediaType
import org.apache.tika.parser.{ AutoDetectParser, Parser, ParseContext }

import org.tyranid.Imp._
import org.tyranid.cloud.aws.S3
import org.tyranid.content.{ Content, ContentMeta }
import org.tyranid.db.{ Entity, Record }
import org.tyranid.db.meta.{ TidItem }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ MongoEntity }
import org.tyranid.web.{ WebContext, Weblet, WebHandledException, FileUploadSupport }
import org.tyranid.web.FileUploadSupport.BodyParams

class InputStreamImp( is:InputStream ) {

  def asString = {
    val buffer = new Array[Char]( 0x10000 )
    val sb = new StringBuilder
    val in = new InputStreamReader( is, "UTF-8" )
    var read = 0

    try {
      do {
        read = in.read( buffer, 0, buffer.length )
        
        if ( read > 0 )
          sb.appendAll( buffer, 0, read )
        
      } while ( read >= 0 )
    } finally {
      is.close
    }

    sb.toString
  }

  def transferTo( to:OutputStream, closeInput:Boolean = false ) {
  	try {
      val buffer = new Array[Byte]( 8192 )
  
      @tailrec def transfer {
        val read = is.read( buffer )
  
        if ( read >= 0 ) {
          to.write( buffer, 0, read )
          transfer
        }
      }
        
      transfer
  	} finally {
  	  to.flush
  	  
  	  if ( closeInput )
  	    is.close
  	}
  }
  
  def detectMimeType( filename:String ) = {
    val parser = new AutoDetectParser()
    parser.setParsers( new HashMap[MediaType, Parser]() )
    val metadata = new Metadata()
    metadata.add( TikaMetadataKeys.RESOURCE_NAME_KEY, filename )
    parser.parse( is, new DefaultHandler(), metadata, new ParseContext() )
    is.close()
    metadata.get( HttpHeaders.CONTENT_TYPE )
  }
}

object Iolet extends Weblet {
  lazy val notFoundUrl = B.getS3Bucket( "public" ).url( "icons/na.png" )

  def handle( web:WebContext ) {
    rpath match {
     // <img src="/io/thumb/a09vUCwNUOSweddROKEl/l|m|s|t"/>
    case s if s.startsWith( "/thumb/" ) =>
      val parts = s.substring( 7 ).split( "/" )
      
      if ( parts.size < 2 ) {
        web.res.setStatus( 302 )
        web.res.setHeader( "Location", notFoundUrl )
        web.res.setHeader( "Connection", "close" )
        return
      }
        
      val tid = parts(0)
      val size = parts(1)
      val pathParts = tid.splitAt( 4 )
      val urlPath = pathParts._1 + "/" + pathParts._2 + "/" + size
      var tries = 0
      
      while ( tries < 3 ) {
        tries += 1
        
        try {
          web.res.s3( Content.thumbsBucket, urlPath, web.req )
          return
        } catch {
          case e:AmazonS3Exception if e.getStatusCode == 404 =>
            def entity = Entity.byTid( tid ).getOrElse( null )
           
            if ( entity == null || !entity.is[ContentMeta] ) {
              web.res.setStatus( 302 )
              web.res.setHeader( "Location", notFoundUrl )
              web.res.setHeader( "Connection", "close" )
              //web.res.setStatus( 404 )
              return
            }
           
            val rec = Record.getByTid( tid ) 
  
            if ( rec == null ) {
              web.res.setStatus( 302 )
              web.res.setHeader( "Location", notFoundUrl )
              web.res.setHeader( "Connection", "close" )
              //web.res.setStatus( 404 )
              return
            }
           
            if ( !rec.as[Content].generateThumbs ) {
              web.res.setStatus( 302 )
              web.res.setHeader( "Location", B.getS3Bucket( "public" ).url( "icons/na.png" ) )
              web.res.setHeader( "Connection", "close" )
              return
            }
          case e2 if e2.getClass.getSimpleName == "EofException" =>
            // Happens with IE a lot
            web.res.setStatus( 200 )
            return
          case e3:AmazonClientException =>
            web.res.setStatus( 302 )
            web.res.setHeader( "Location", notFoundUrl )
            web.res.setHeader( "Connection", "close" )
            log( Event.Eof, "m" -> ( e3.getMessage() + " for thumb path= " + rpath ) )
            return
          case e4:Throwable =>
            web.res.setStatus( 302 )
            web.res.setHeader( "Location", notFoundUrl )
            web.res.setHeader( "Connection", "close" )
            throw e4
        }
      }
      
      web.res.setStatus( 404 )
    case "/tempUpload" =>
      val url = web.s( 'url )

      val filename:String = if ( url.notBlank ) {
        val ext = org.tyranid.io.File.safeExtension( url )
        val randName = math.random * Int.MaxValue + "_" + System.currentTimeMillis + "." + ext 

        S3.storeUrl( org.tyranid.io.File.tempBucket, url, randName, true )
        randName
      } else {      
        val bodyParams = web.req.getAttribute(FileUploadSupport.BodyParamsKey).as[BodyParams]
  
        if ( bodyParams == null ) {
          null
        } else {
          val formParams = bodyParams.formParams
          val fileItem = bodyParams.getFileItems( "files", bodyParams.getFileItems( "thumb" ) )(0) 
          
          val imgFile:JFile = fileItem.as[DiskFileItem].getStoreLocation
  
          if ( !imgFile.exists )
            fileItem.write( imgFile )
  
          val randName = math.random * Int.MaxValue + "_" + System.currentTimeMillis + "_" + fileItem.getName 
          S3.write( org.tyranid.io.File.tempBucket, randName, imgFile, true )
          randName
        }
      }

      // This will save the property off into an existing object if needed
      val saveProp = web.s( 'upp )
      
      if ( saveProp.notBlank ) {
        val tid = web.s( 'c )                
        val en = Entity.byTid( tid )
        
        if ( en.is[MongoEntity] )
          en.as[MongoEntity].db.update( Mobj( "_id" -> en.as[MongoEntity].tidToId( tid ) ), Mobj( $set -> Mobj( saveProp -> url ) ) )
      }
      
      if ( filename.notBlank )
        web.json( Map( "thumbSrc" -> org.tyranid.io.File.tempBucket.url( filename ) ) )
      else
        web.jsRes()
    case _ =>
      web.jsRes()
    }
  }
}


