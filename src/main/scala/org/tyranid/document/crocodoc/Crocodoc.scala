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

package org.tyranid.document.crocodoc

import scala.xml.Unparsed

import java.io.{ File, FileOutputStream }

import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.db.mongo.Imp._
import org.tyranid.json.Json
import org.tyranid.http.Http
import org.tyranid.math.Base64
import org.tyranid.session.Session
import org.tyranid.document.DocApp
import org.tyranid.time.Time
import org.tyranid.ui.Form
import org.tyranid.web.{ Weblet, WebContext }

object Crocodoc {
  val code = "croc"
}

case class CrocApp( apiKey:String, secret:String = null ) extends DocApp {
  val serviceCode = Crocodoc.code
  val serviceName = "Crocodoc"
    
  val supportedFormats = List( "DOC", "DOCX", "XLS", "XLSX", "PPT", "PPTX", "PDF" )
  
  def upload( file:File, fileSize:Long, filename:String ):String = {
    if ( supports( filetypeFor( filename ) ) ) {
      val result = Http.POST_S( "https://crocodoc.com/api/v2/document/upload", file, fileSize, params = Map( "token" -> apiKey ), filename = filename )._s
      externalDocId( Json.parse( result ).s( 'uuid ) )
    } else {
      null
    }
  }
  
  def statusFor( extDocId:String ) = {
    val statusJson = Json.parse( Http.GET( "https://crocodoc.com/api/v2/document/status?token=" + apiKey + "&uuids=" + extDocId ).s ).get(0)
    statusJson.s( 'status )
  }
  
  def previewUrlFor( extDocId:String ) = {
    val viewingSessionId = Json.parse( Http.POST( "https://crocodoc.com/api/v2/session/create", null, Map( "token" -> apiKey, "uuid" -> extDocId ) ).s ).s( 'session )
    "https://crocodoc.com/view/" + viewingSessionId
  }
  
  def getThumbnailFile( extDocId:String ) = {
    val res = Http.GET( "https://crocodoc.com/api/v2/download/thumbnail?token=" + apiKey + "&uuids=" + extDocId )
    val entity = res.response.getEntity
    
    if ( entity != null ) {
      val instream = entity.getContent
      val tmpFile = File.createTempFile( extDocId, ".png" )
      val out = new FileOutputStream( tmpFile )
       
      instream.transferTo( out, true )

      tmpFile
    } else {
      null
    }
  }
  
  def previewParams( extDocId:String, width:String, height:String ):Map[String,AnyRef] = {
    statusFor( extDocId ) match {
      case "DONE" =>
        val iframeSrc = previewUrlFor( extDocId )
        
        Map( "width" -> width, 
             "height" -> height,
             "cssClass" -> "no-scroll",
             "html" -> ( { org.tyranid.session.Notification.box } ++
                         <iframe style="width:100%;height:100%;" src={ iframeSrc }/> ) )
      case "ERROR" =>
        Map( "status" -> ( "Error occured!: " ) ) //+ statusJson.s( 'error ) ) ) )
      case s =>
        Map( "status" -> ( "Error, unknown status: " + s._s ) )
    }
  }
  
  def delete( extDocId:String ):Boolean = {
    val result = Http.POST_S( "https://crocodoc.com/api/v2/document/delete", null, 0, params = Map( "token" -> apiKey, "uuid" -> extDocId ) )._s
    
    ( result == "true" )
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

