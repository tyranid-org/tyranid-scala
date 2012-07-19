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

package org.tyranid.document.issuu

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

object Issuu {
  val code = "issuu"
}

case class IssuuApp( apiKey:String, secret:String = null ) extends DocApp {
  val serviceCode = Issuu.code
  val serviceName = "Issuu"
  val websiteUrl = "http://www.issuu.com"
    
  override val active = false
    
  val supportedFormats = List( "DOC", "DOCX", "XLS", "XLSX", "PPT", "PPTX", "PDF", "ODT", "ODP", "WPD", "RTF", "SXI" )
  
  def upload( file:File, fileSize:Long, filename:String ):String = {
    var externalId:String = null
    
    if ( supports( filename.suffix( '.' ) ) ) {
      println( "filename " + filename )
      println( file.exists )
      println( file.length )
      
      val params = scala.collection.mutable.Map( 
          "apiKey" -> apiKey, 
          "action" -> "issuu.document.upload", 
          "format" -> "json", 
          "access" -> "private" )
          
      val result = Http.POST_FILE( "http://upload.issuu.com/1_0", file, fileSize, filename, params = ( params += "signature" -> MD5( params ) ) )._s
      
      println( "issuu: " + result )
      
      val res = Json.parse( result ).get( 'rsp )
      val stat = res.s( 'stat )
      
      if ( stat == "ok" ) {
        val doc = res.get( '_content ).get( 'document )
        externalId = externalDocId( res.s( 'documentId ) )
      } else { 
        log( Event.Issuu, "m" -> ( "Failed to upload document: " + filename ) )
      }
    }
    
    externalId
  }
  
  def getText( extDocId:String ):String = {
    null  
  }
  
  def MD5( params:scala.collection.mutable.Map[String,String] ):String = {
    val str = new StringBuilder
    params.foreach( p => { str ++= ( p._1 + p._2 ) } )
    MD5( str.toString )
  }

  def statusFor( extDocId:String ) = {
    "DONE"
  }
  
// TODO
  def previewUrlFor( extDocId:String ) =  
    //"https://crocodoc.com/view/" + Json.parse( Http.POST( "https://crocodoc.com/api/v2/session/create", null, Map( "token" -> apiKey, "uuid" -> extDocId ) ).s ).s( 'session )
    "TODO"
  
// TODO
    def getThumbnailFile( extDocId:String, width:Int = 300, height:Int = 300 ) = {
    val res = Http.GET( "https://crocodoc.com/api/v2/download/thumbnail?token=" + apiKey + "&uuid=" + extDocId + "&size=" + width + "x" + height )
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
  
  // TODO
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
  
  // TODO
  def delete( extDocId:String ):Boolean = {
    //val result = Http.POST( "https://crocodoc.com/api/v2/document/delete", "", Map( "token" -> apiKey, "uuid" -> extDocId ) )._s
    
    //( result == "true" )
    false
  }
}

object Issuulet extends Weblet {

  def handle( web:WebContext ) {
    val s = Session()
    val u = s.user

    rpath match {
    case _ => _404
    }
  }
}

