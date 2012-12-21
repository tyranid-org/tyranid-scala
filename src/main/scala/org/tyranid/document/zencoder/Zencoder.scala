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

package org.tyranid.document.zencoder

import scala.collection.mutable
import scala.collection.mutable.Buffer
import scala.xml.{ Unparsed, NodeSeq }

import java.io.{ File, FileOutputStream, FileInputStream }

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

object Zencoder {
  val mp4Vid = Seq( "mp4", "h264", "aac" )
  val oggVid = Seq( "ogg", "theora", "vorbis" )
  val allVids = Seq( oggVid, mp4Vid )
  
  val otherVidFormats = Map(   
        "mp4" -> Seq( oggVid ),
        "ogg" -> Seq( mp4Vid ),
        "ogv" -> Seq( mp4Vid ),
        "mov" -> allVids,
        "flv" -> allVids,
        "3gp" -> allVids,
        "mov" -> allVids,
        "wmv" -> allVids
      )

  val mp4Aud = Seq( "m4a", "aac" )
  val oggAud = Seq( "oga", "vorbis" )
  
  val allAudio = Seq( oggAud, mp4Aud )
  
  val otherAudioFormats = Map(   
        "m4a" -> Seq( oggAud ),
        "oga" -> Seq( mp4Aud ),
        "mp3" -> allAudio,
        "wav" -> allAudio
      )
      
  def supports( filename:String ) = supportsVideo( filename ) || supportsAudio( filename )
  def supportsVideo( filename:String ) = otherVidFormats.getOrElse( filename.suffix( '.' ).toLowerCase, null ) != null
  def supportsAudio( filename:String ) = otherAudioFormats.getOrElse( filename.suffix( '.' ).toLowerCase, null ) != null
  
  private def mapForFormat( url:String, format:Seq[String], forAudio:Boolean = false ) = {
    if ( forAudio )
      Map( "url" -> ( url + ".TMP." + format(0) ),
         "audio_codec" -> format(1) )
    else 
      Map( "url" -> ( url + ".TMP." + format(0) ),
         "video_codec" -> format(1),
         "audio_codec" -> format(2) )
  }
  
  def outputFormats( url:String, formats:Seq[Seq[String]], forAudio:Boolean = false ) = {
    var maps = new mutable.ArrayBuffer[Map[String,String]]()
    
    for ( format <- formats )
      maps += mapForFormat( url, format, forAudio )
    
    maps.toSeq
  }
}

case class ZencoderApp( apiKey:String ) {
 // val websiteUrl = "http://www.zencoder.com"
    
  def upload( inputUrl:String, filename:String, doc:DBObject ): Boolean = {
    var ext = filename.suffix( '.' ).toLowerCase
    
    var formats = Zencoder.otherVidFormats.getOrElse( ext, null )
    var isAudio = false
    
    if ( formats == null ) {
      isAudio = true
      formats = Zencoder.otherAudioFormats.getOrElse( ext, null )
    }
      
    if ( formats != null ) {
      val outputFormats =  Zencoder.outputFormats( inputUrl, formats, isAudio )
      val jsonReq = Map( 
          "test" -> B.DEV,  
          "input" -> inputUrl,
          "output" -> outputFormats )
          
      println( "request: " + jsonReq.toJsonStr )
      val req = Http.POST( "https://app.zencoder.com/api/v2/jobs", jsonReq.toJsonStr, null, "application/json", Map( "Zencoder-Api-Key" -> apiKey ) )
      val result = req.s            
      
      println( "zc res: " + result )
      if ( req.response.getStatusLine().getStatusCode() != 201 ) {
        log( Event.Zencoder, "m" -> ( "Failed to upload video: " + filename + ", error=" + result ) )
      } else {
        //{"outputs":[{"label":null,"url":"https://s3.amazonaws.com/files.volerro.com/5069a80ad748dff278930a82/50d2180fd748c9c33a8e8b0f.ogg","id":66443523},{"label":null,"url":"https://s3.amazonaws.com/files.volerro.com/5069a80ad748dff278930a82/50d2180fd748c9c33a8e8b0f.mp4","id":66443525}],"test":true,"id":34152199}
        val res = Json.parse( result )
        doc( 'zid ) = res.i( 'id )
  
        val outputs = res.a( "outputs" )
        val outputlen = outputs.size
        val zoids = doc.a_!( 'zoids )
        val zformats = doc.a_!( 'zfmts )
        
        for ( i <- 0 until outputlen ) {
          val output = outputs.get( i )
          zoids.add( i, output.i( 'id ).as[AnyRef] )
          zformats.add( output.s( 'url ).suffix( '.' ) )
        }
      }
      
      return true
    }
    
    false
  }
  
  // Output states include waiting, queued, assigning, ready, processing, finished, failed, cancelled and no input.
  def status( doc:DBObject ) = {
    val zid = doc.i( 'zid )
    
    if ( zid > 0 ) {
      //GET https://app.zencoder.com/api/v2/jobs/1234.xml?api_key=asdf1234
      val statusJson = Json.parse( Http.GET( "https://app.zencoder.com/api/v2/jobs/" + zid + ".json?api_key=" + apiKey ).s )
      statusJson.s( 'state )
    } else {
      null
    }
  }
  
  /*
  /*  mp4 to ogg
POST /api/v2/jobs HTTP/1.1
Accept: application/json
Content-Type: application/json
Zencoder-Api-Key: e834e2d2e415f7ef2303ecbb81ab54da

{
  "test": true,
  "input": "https://s3.amazonaws.com/files.volerro.com/50c8ba9ee4b0a9fc6e32a1bd/50c8bb39e4b0a9fc6e32a1d9",
  "private": true,
  "output": [
    {
      "url": "https://s3.amazonaws.com/files.volerro.com/50c8ba9ee4b0a9fc6e32a1bd/50c8bb39e4b0a9fc6e32a1d9.ogg",
      "video_codec": "theora",
      "audio_codec": "vorbis"
    }
  ]
}

 "video_codec": "h264",
      "audio_codec": "aac"
      
   */
  def statusFor( extDocId:String ) = {
    val statusJson = Json.parse( Http.GET( "https://Zencoder.com/api/v2/document/status?token=" + apiKey + "&uuids=" + extDocId ).s ).get(0)
    statusJson.s( 'status )
  }
  
  def docPreviewContainer( extDocId:String, height:String="100%", print:Boolean = false ): NodeSeq =
    { print |*
        <script src="//ajax.googleapis.com/ajax/libs/jquery/1.8.3/jquery.min.js"></script> ++
        <script src='//static-v2.Zencoder.com/core/docviewer.js'></script>
    } ++
    <div class="doc-view doc Zencoder annotatableObject" id={ "dv_" + extDocId }/>

    /* ---

_doc = {"status": 3, "socketioHost": "//socket.Zencoder.com:5555/", "objects": [], "pageStatuses": "", "demo": false, "editable": false, "webserviceUrl": "//Zencoder.com/webservice/", "step": "DONE", "session": "8TkzK5jhqzMVqinRejpBwRPld2ZrZ3AAzwhrv9w9WnScgvpZSUp-H7b9WaMj9K11jSv950vHA9P6HwByZMjqfd-rSddPpq2e49_eCQ", "assetsLocation": "//proxy-v2.Zencoder.com/ehdLsgyVIIbgTCAXV5WnT3UCQYZBCjwgTqzVhbjWZE8d_nSgJMk1QNjaunsVUKjxySR-kGG8DuGhNa6qyRejRgIJNUj5vmKjQNtNvSPz1bPrz8gHOZQ81XfcPKv6aa7mUCCstyg-tExqkWTnP0a8LN-26KKLpC9lQ_tDNrZgJToXvCgkz13NIaq30TgDv2LX8JoBBrP1hNtgnso_QfkIV8R6DPvf9mR-ow8CY8il1fv_/8TkzK5jhqzMVqinRejpBwRPld2ZrZ3AAzwhrv9w9WnScgvpZSUp-H7b9WaMj9K11jSv950vHA9P6HwByZMjqfd-rSddPpq2e49_eCQ/", "metadata": {"numpages": 2, "fonts": [{"last": 2, "id": 94, "first": 1}, {"last": 2, "id": 98, "first": 1}, {"last": 2, "id": 111, "first": 1}, {"last": 1, "id": 121, "first": 1}, {"last": 1, "id": 125, "first": 1}, {"last": 1, "id": 129, "first": 1}, {"last": 2, "id": 4, "first": 2}], "pages": {}, "defaults": {"width": 648.0, "height": 792.0}}};
  if ( proj ) {
    proj.initViewer('a9b32759-98d3-45ce-8ce3-23090684b10f');
  } else {
    var docViewer = new DocViewer({ "id": "dv_a9b32759-98d3-45ce-8ce3-23090684b10f" });
  }

      --- */

  override def previewJsFor( extDocId:String, print:Boolean = false ) = {
    val sessionJson = Http.POST( "https://Zencoder.com/api/v2/session/create", null, Map( "token" -> apiKey, "uuid" -> extDocId ) ).s
    val session = Json.parse( sessionJson ).s( 'session )

    Http.GET( "https://Zencoder.com/webservice/document.js?session=" + session )._s + ";" +
    ( if ( print )
        "var docViewer = new DocViewer({ id:'dv_" + extDocId + "', zoom:'fitWidth' });"
      else
        """if ( proj ) { proj.initViewer('""" + extDocId + """'); } else { var docViewer = new DocViewer({ "id": "dv_""" + extDocId + """" }); }""" )
  }
  
  def previewUrlFor( extDocId:String ):String = null
  
  //def previewUrlFor( extDocId:String ):String = { 
  //  val sessionJson = Http.POST( "https://Zencoder.com/api/v2/session/create", null, Map( "token" -> apiKey, "uuid" -> extDocId ) ).s
  //  "https://Zencoder.com/view/" + Json.parse( sessionJson ).s( 'session )
  //}
  
  def getThumbnailFile( extDocId:String, width:Int = 300, height:Int = 300 ):File = {
    var tries = 0
    
    while ( tries < 8 ) {
      tries += 1
      val res = Http.GET( "https://Zencoder.com/api/v2/download/thumbnail?token=" + apiKey + "&uuid=" + extDocId + "&size=" + width + "x" + height )
      val entity = res.response.getEntity
    
      if ( entity != null ) {
        if ( "application/json" == entity.getContentType.getValue ) {
          val json = Json.parse( res._s )
          
          if ( json.s( 'error ) == "thumbnail not available" ) {
            Thread.sleep( 2000 ) // retry -- this is the only time this method does not return out of this while loop
          } else {
            log( Event.Zencoder, "m" -> ( "Get Thumbnail failed for Zencoder uuid: " + extDocId + ", error is: " + res._s ) )
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
    val text = Http.GET( "https://Zencoder.com/api/v2/download/text?token=" + apiKey + "&uuid=" + extDocId )._s
    
    if ( text.startsWith( "{\"error\"" ) ) {
      log( Event.Zencoder, "m" -> ( "Extract text failed for Zencoder uuid: " + extDocId + ", error is: " + text ) )
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
    val result = Http.POST( "https://Zencoder.com/api/v2/document/delete", null, Map( "token" -> apiKey, "uuid" -> extDocId ) )._s
    
    if ( result == "true" ) 
      true
    else {
      log( Event.Zencoder, "m" -> ( "Deletion failed for Zencoder uuid: " + extDocId ) )
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
  */
}

