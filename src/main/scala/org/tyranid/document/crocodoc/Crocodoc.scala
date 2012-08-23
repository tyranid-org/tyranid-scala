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
import java.io.FileInputStream

object Crocodoc {
  val code = "croc"
}

case class CrocApp( apiKey:String, secret:String = null ) extends DocApp {
  val serviceCode = Crocodoc.code
  val serviceName = "Crocodoc"
  val websiteUrl = "http://www.crocodoc.com"
    
  val supportedFormats = List( "DOC", "DOCX", "XLS", "XLSX", "PPT", "PPTX", "PDF" )
  
  def upload( file:File, fileSize:Long, filename:String ):String = {
    if ( supports( filename.suffix( '.' ) ) ) {
      //println( "filename " + filename )
      //println( file.exists )
      //println( file.length )
      
      var error:String = null
        
      while ( error.isBlank ) {
        val result = Http.POST_FILE( "https://crocodoc.com/api/v2/document/upload", file, fileSize, filename, params = Map( "token" -> apiKey ) ).s
      
      //println( "croc: " + result )
      
        val res = Json.parse( result )
        error = res.s( 'error )
        
        if ( error.isBlank )
          return externalDocId( res.s( 'uuid ) )
          
        if ( error.containsIgnoreCase( "rate limit exceeded" ) ) {
          Thread.sleep( 2000 )
          error = null
        } else {
          log( Event.Crocodoc, "m" -> ( "Failed to upload document: " + filename + ", error=" + error ) )
        }
      }
    }
    
    null
  }
  
  def statusFor( extDocId:String ) = {
    val statusJson = Json.parse( Http.GET( "https://crocodoc.com/api/v2/document/status?token=" + apiKey + "&uuids=" + extDocId ).s ).get(0)
    statusJson.s( 'status )
  }
  
  def previewUrlFor( extDocId:String ):String = { 
    val sessionJson = Http.POST( "https://crocodoc.com/api/v2/session/create", null, Map( "token" -> apiKey, "uuid" -> extDocId ) ).s
    "https://crocodoc.com/view/" + Json.parse( sessionJson ).s( 'session )
  }
  
  def getThumbnailFile( extDocId:String, width:Int = 300, height:Int = 300 ) = {
    Http.GET_File( "https://crocodoc.com/api/v2/download/thumbnail?token=" + apiKey + "&uuid=" + extDocId + "&size=" + width + "x" + height, ext = ".png" )
  }
  
  def getText( extDocId:String ):String = {
    val text = Http.GET( "https://crocodoc.com/api/v2/download/text?token=" + apiKey + "&uuid=" + extDocId )._s
    
    if ( text.startsWith( "{\"error\"" ) ) {
      log( Event.Crocodoc, "m" -> ( "Extract text failed for crocodoc uuid: " + extDocId + ", error is: " + text ) )
      null
    } else 
      text
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

