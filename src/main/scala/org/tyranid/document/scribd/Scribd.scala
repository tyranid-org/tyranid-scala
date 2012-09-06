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

import scala.xml.{ Unparsed, NodeSeq }

import java.io.{ File, FileOutputStream }

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
  val websiteUrl = "http://www.scribd.com"

  val supportedFormats = List( "DOC", "DOCX", "XLS", "XLSX", "PPS", "PPT", "PPTX", "PDF", "PS", "ODT", "FODT", "SXW", "ODP", "FODP", "SXI", "ODS", "FODS", "SXC", "TXT", "RTF", "ODB", "ODG", "FODG", "ODF"  )
    
  /*
  <rsp stat="ok">
    <doc_id>123456</doc_id>
    <access_key>key-rvfa2c82sq5bf9q8t6v</access_key>
    <secret_password>2jzwhplozu43cyqfky1m</secret_password>
  </rsp>
  */
    
  def upload( file:File, fileSize:Long, filename:String ):String = {
    if ( supports( filename.suffix( '.' ) ) ) {
      val resultStr = Http.POST_FILE( "http://api.scribd.com/api?", file, fileSize, filename, params = Map( "method" -> "docs.upload", "access" -> "private", "api_key" -> apiKey, "secure" -> "1" ) )._s

      //println( "scribd: " + resultStr )
    
      val response = resultStr.toXml \\ "rsp"
      val stat = ( response \ "@stat" ).text

      if ( stat == "ok" ) {
        val docId = ( response \\ "doc_id" ).text
        val accessKey = ( response \\ "access_key" ).text
        val secretPassword = ( response \\ "secret_password" ).text
      
        externalDocId( docId + "," + accessKey + ( secretPassword.isBlank ? "" | ( "," + secretPassword ) ) )
      } else {
        log( Event.Scribd, "m" -> ( "Failed to upload document: " + filename + ", error=" + ( response \\ "error" \ "@message" ).text ) )
        null
      }
    } else {
      null
    }
  }
  
  def statusFor( extDocId:String ) = {
    "DONE"
    //val statusJson = Json.parse( Http.GET( "https://crocodoc.com/api/v2/document/status?token=" + apiKey + "&uuids=" + extDocId ).s ).get(0)
    //statusJson.s( 'status )
  }
  
  def getText( extDocId:String ):String = {
    null
  }
  
  def previewUrlFor( extDocId:String ):String = null
  
  def docPreviewContainer( extDocId:String, height:String="100%" ): NodeSeq = {
    <div class="scribd" id={ "scrib_doc_" + extDocId.split( "," )(0) }></div>    
  }

  override def previewJsFor( extDocId:String ) = {
    val parts = extDocId.split( "," )
    """
    var scribd_doc = scribd.Document.getDoc(""" + parts(0) + ", '" + parts(1) + """');
    var onDocReady = function(e) {
      scribd_doc.api.setPage(1); 
      //console.log( scribd_doc.api.getPage() );
    };
    
    scribd_doc.addParam('jsapi_version', 2);
    scribd_doc.addParam('default_embed_format', 'html5');
    scribd_doc.addParam('use_ssl', true); 
    scribd_doc.addEventListener( 'docReady', onDocReady );
    
//    scribd_doc.addEventListener( 'pageChanged', function(e) { 
//      console.log( 'page changed to: ' + scribd_doc.api.getPage() );
//    } ); 
    
    scribd_doc.write('scrib_doc_""" + parts(0) + """');
    scribd_doc.grantAccess('""" + T.user.tid + """','""" + Session().id + """','""" + MD5( parts(0), Session().id, T.user.tid ) + """');"""
  }
  
  def previewParams( extDocId:String, width:String, height:String ):Map[String,AnyRef] = {
    val previewJs = previewJsFor( extDocId )
    
    Map( "width" -> width, 
         "height" -> height,
         "cssClass" -> "no-scroll",
         "extraJS" -> previewJs,
         "html" -> docPreviewContainer( extDocId ) )
  }
  
  def getThumbnailFile( extDocId:String, width:Int = 300, height:Int = 300 ) = {
    val parts = extDocId.split( "," )
    val resultStr = Http.GET( "http://api.scribd.com/api?method=thumbnail.get&api_key=" + apiKey + "&doc_id=" + parts(0) + "&api_sig=" + MD5( parts(0), Session().id, T.user.tid ) + "&width=" + width + "&height=" + height )._s
    
    // <?xml version="1.0" encoding="UTF-8"?><rsp stat="ok"><thumbnail_url>http://imgv2-1.scribdassets.com/img/word_document/97855850/111x142/65d2b8d54c/1342103296</thumbnail_url></rsp>
    
    val response = ( resultStr.toXml \\ "rsp" )
     
    val stat = ( response \ "@stat" ).text
    
    if ( stat == "ok" )
      Http.GET_File( ( response \\ "thumbnail_url" ).text, ".png" )
    else {
      log( Event.Scribd, "m" -> ( "Get Thumbnail failed for scribd uuid: " + extDocId + ", error is: " + ( response \\ "error" \ "@message" ).text ) )
      null
    }
  }
  
  def delete( extDocId:String ):Boolean = {
    val parts = extDocId.split( "," )
    val resultStr = Http.GET( "http://api.scribd.com/api?method=docs.delete&api_key=" + apiKey + "&doc_id=" + parts(0) + "&api_sig=" + MD5( parts(0), Session().id, T.user.tid ) )._s
    false
  }
  
  //MD5([Your API Secret Key]document_id[The Document ID]session_id[Session ID]user_identifier[User Identifier])
  def MD5( docId:String, sessionId:String, userId:String ):String = MD5( secret + "document_id" + docId + "session_id" + sessionId + "user_identifier" + userId )
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

