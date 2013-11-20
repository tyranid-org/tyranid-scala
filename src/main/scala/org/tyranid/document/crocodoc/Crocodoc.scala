/**
 * Copyright (c) 2008-2013 Tyranid <http://tyranid.org>
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

package org.tyranid.document.crocodoc

import scala.collection.mutable
import scala.xml.{ Unparsed, NodeSeq }
import java.io.{ File, FileOutputStream, FileInputStream }
import com.mongodb.DBObject
import org.tyranid.Imp._
import org.tyranid.app.AppStat
import org.tyranid.db.mongo.Imp._
import org.tyranid.json.Json
import org.tyranid.http.Http
import org.tyranid.math.Base64
import org.tyranid.session.Session
import org.tyranid.document.DocApp
import org.tyranid.time.Time
import org.tyranid.ui.Form
import org.tyranid.web.{ Weblet, WebContext }
import java.io.BufferedOutputStream

object Crocodoc {
  val code = "croc"
}

case class CrocApp( apiKey:String, secret:String = null ) extends DocApp {
  val serviceCode = Crocodoc.code
  val serviceName = "Crocodoc"
  val websiteUrl = "http://www.crocodoc.com"
    
  val supportedFormats = List( "DOC", "DOCX", "XLS", "XLSX", "PPT", "PPTX", "PDF" )
  
  def upload( file:File, fileSize:Long, filename:String, obj:DBObject ): Boolean = {
    val suffix = filename.suffix( '.' )
    
    if ( supports( suffix ) ) {
      //println( "filename " + useFilename )
      //println( file.exists )
      //println( file.length )
      
      var error:String = null
      var retryCount = 0
        
      while ( error.isBlank ) {
        retryCount = retryCount + 1
        
        if ( retryCount == 1 )
          AppStat.CrocodocUpload
        
        val result = Http.POST_FILE( "https://crocodoc.com/api/v2/document/upload", file, fileSize, filename, params = Map( "token" -> apiKey ) ).s
      
      //println( "croc: " + result )
      
        val res = Json.parse( result )
        error = res.s( 'error )
        
        if ( error.isBlank ) {
          obj( 'externalId ) = externalDocId( res.s( 'uuid ) )
          AppStat.CrocodocSuccess
          return true
        } else if ( error.containsIgnoreCase( "rate limit exceeded" ) ) {
          AppStat.CrocodocRetry
          Thread.sleep( 2000 )
          error = null
        } else {
          log( Event.Crocodoc, "m" -> ( "Failed to upload document: " + filename + ", error=" + error ) )
          AppStat.CrocodocFailure
          return false
        }
      }
    }
    
    false
  }
  
  def statusFor( extDocId:String ) = {
    val statusJson = Json.parse( Http.GET( "https://crocodoc.com/api/v2/document/status?token=" + apiKey + "&uuids=" + extDocId ).s ).get(0)
    statusJson.s( 'status )
  }
  
  def docPreviewContainer( extDocId:String, height:String="100%", print:Boolean = false, annotatable:Boolean = true ): NodeSeq =
    { print |* <script src={ B.CROCODOC_SCRIPT }></script> } ++
    <div class={ "doc-view doc crocodoc" + ( annotatable |* " annotatableObject" ) } id={ "dv_" + extDocId }></div>

    /* ---

_doc = {"status": 3, "socketioHost": "//socket.crocodoc.com:5555/", "objects": [], "pageStatuses": "", "demo": false, "editable": false, "webserviceUrl": "//crocodoc.com/webservice/", "step": "DONE", "session": "8TkzK5jhqzMVqinRejpBwRPld2ZrZ3AAzwhrv9w9WnScgvpZSUp-H7b9WaMj9K11jSv950vHA9P6HwByZMjqfd-rSddPpq2e49_eCQ", "assetsLocation": "//proxy-v2.crocodoc.com/ehdLsgyVIIbgTCAXV5WnT3UCQYZBCjwgTqzVhbjWZE8d_nSgJMk1QNjaunsVUKjxySR-kGG8DuGhNa6qyRejRgIJNUj5vmKjQNtNvSPz1bPrz8gHOZQ81XfcPKv6aa7mUCCstyg-tExqkWTnP0a8LN-26KKLpC9lQ_tDNrZgJToXvCgkz13NIaq30TgDv2LX8JoBBrP1hNtgnso_QfkIV8R6DPvf9mR-ow8CY8il1fv_/8TkzK5jhqzMVqinRejpBwRPld2ZrZ3AAzwhrv9w9WnScgvpZSUp-H7b9WaMj9K11jSv950vHA9P6HwByZMjqfd-rSddPpq2e49_eCQ/", "metadata": {"numpages": 2, "fonts": [{"last": 2, "id": 94, "first": 1}, {"last": 2, "id": 98, "first": 1}, {"last": 2, "id": 111, "first": 1}, {"last": 1, "id": 121, "first": 1}, {"last": 1, "id": 125, "first": 1}, {"last": 1, "id": 129, "first": 1}, {"last": 2, "id": 4, "first": 2}], "pages": {}, "defaults": {"width": 648.0, "height": 792.0}}};
  if ( proj ) {
    proj.initViewer('a9b32759-98d3-45ce-8ce3-23090684b10f');
  } else {
    var docViewer = new DocViewer({ "id": "dv_a9b32759-98d3-45ce-8ce3-23090684b10f" });
  }

      --- */

  override def previewJsonFor( extDocId:String, print:Boolean = false ) = {
    val sessionJson = Http.POST( "https://crocodoc.com/api/v2/session/create", null, Map( "token" -> apiKey, "uuid" -> extDocId ) ).s
    
    var tries = 0
    var previewJson:Map[String,Any] = null
    
    while ( tries < 3 ) {
      try {
        val session = Json.parse( sessionJson ).s( 'session )
        
        previewJson = session.isBlank ? null | Map( 
          "extDocId" -> extDocId,
          "session" -> session,
          "_doc" -> ( B.CROC_JS_V2 ? Http.GET( "https://crocodoc.com/webservice/document.js?session=" + session )._s.substring( 6 ).parseJson | Map() )
        )
        
        tries = ( previewJson == null ) ? 3 | ( tries + 1 )
      } catch {
        case t:Throwable => 
          log( Event.Crocodoc, "m" -> ( "Failed to get status for extId: " + extDocId + ", str=" + sessionJson ), "ex" -> t )
          Thread.sleep( 1000 ) // 1.25 secs
      }
    }
    
    previewJson
  }
  
  def previewUrlFor( extDocId:String ):String = null
  
  //def previewUrlFor( extDocId:String ):String = { 
  //  val sessionJson = Http.POST( "https://crocodoc.com/api/v2/session/create", null, Map( "token" -> apiKey, "uuid" -> extDocId ) ).s
  //  "https://crocodoc.com/view/" + Json.parse( sessionJson ).s( 'session )
  //}
  
  def getThumbnailFile( extDocId:String, width:Int = 300, height:Int = 300 ):File = {
    var tries = 0
    
    while ( tries < 10 ) {
      tries += 1
      val res = Http.GET( "https://crocodoc.com/api/v2/download/thumbnail?token=" + apiKey + "&uuid=" + extDocId + "&size=" + width + "x" + height )
      val entity = res.response.getEntity
    
      if ( entity != null ) {
        if ( "application/json" == entity.getContentType.getValue ) {
          val json = Json.parse( res._s )
          
          if ( json.s( 'error ) == "thumbnail not available" ) {
            Thread.sleep( 2000 ) // retry -- this is the only time this method does not return out of this while loop
          } else {
            log( Event.Crocodoc, "m" -> ( "Get Thumbnail failed for crocodoc uuid: " + extDocId + ", error is: " + res._s ) )
            return null 
          }
        } else {
          val tmpFile = File.createTempFile( "tmp", ".png" )
          val out = new FileOutputStream( tmpFile )
          
          try {
            entity.getContent.transferTo( out, true )
          } finally {
            out.close
          }
    
          return tmpFile
        }
      } else {
        return null
      }
    }
    
    return null
  }
  
  def getText( extDocId:String ):String = {
    val text = Http.GET( "https://crocodoc.com/api/v2/download/text?token=" + apiKey + "&uuid=" + extDocId )._s
    
    if ( text.startsWith( "{\"error\"" ) ) {
      log( Event.Crocodoc, "m" -> ( "Extract text failed for crocodoc uuid: " + extDocId + ", error is: " + text ) )
      null
    } else {
      text
    }
  }
  
  def delete( extDocId:String ):Boolean = {
    val result = Http.POST( "https://crocodoc.com/api/v2/document/delete", null, Map( "token" -> apiKey, "uuid" -> extDocId ) )._s
    
    if ( result == "true" ) 
      true
    else {
      log( Event.Crocodoc, "m" -> ( "Deletion failed for crocodoc uuid: " + extDocId ) )
      false
    }
  }
}

object Croclet extends Weblet {

  def handle( web:WebContext ) {
    val s = Session()
    val u = s.user

    rpath match {
    case _ => _404
    }
  }
}

