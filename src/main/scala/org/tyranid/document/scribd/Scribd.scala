package org.tyranid.document.scribd

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

import scala.xml.Unparsed

import java.io.File

import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.db.mongo.Imp._
import org.tyranid.json.Json
import org.tyranid.http.Http
import org.tyranid.session.Session
import org.tyranid.document.DocApp
import org.tyranid.web.{ Weblet, WebContext }

object Scribd {
  val code = "scribd"
}

case class ScribdApp( apiKey:String, secret:String = null, publisher:String = null ) extends DocApp {
  val serviceCode = Scribd.code
  val serviceName = "Scribd"

  // scribd: 
  val supportedFormats = List( "DOC", "DOCX", "XLS", "XLSX", "PPS", "PPT", "PPTX", "PDF", "PS", "ODT", "FODT", "SXW", "ODP", "FODP", "SXI", "ODS", "FODS", "SXC", "TXT", "RTF", "ODB", "ODG", "FODG", "ODF"  )
    
  def upload( file:File, fileSize:Long, filename:String ):String = {
    val result = Http.POST_S( "http://api.scribd.com/api?", file, fileSize, params = Map( "method" -> "docs.upload", "api_key" -> apiKey ), filename = filename )._s
    
    //result.
    externalDocId( Json.parse( result ).s( 'uuid ) )
  }
  
  def statusFor( extDocId:String ) = {
    val statusJson = Json.parse( Http.GET( "https://crocodoc.com/api/v2/document/status?token=" + apiKey + "&uuids=" + extDocId ).s ).get(0)
    statusJson.s( 'status )
  }
  
  def previewUrlFor( extDocId:String ) = {
    val viewingSessionId = Json.parse( Http.POST( "https://crocodoc.com/api/v2/session/create", null, Map( "token" -> apiKey, "uuid" -> extDocId ) ).s ).s( 'session )
    "https://crocodoc.com/view/" + viewingSessionId
  }
}

object Scribdlet extends Weblet {

  def handle( web:WebContext ) {
    val s = Session()
    val u = s.user

    rpath match {
    case _ => _404
    }
  }
}

