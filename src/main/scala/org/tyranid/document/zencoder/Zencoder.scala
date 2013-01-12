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

import java.io.{ File, FileInputStream }

import com.mongodb.{ DBObject, DBCollection }

import org.tyranid.Imp._
import org.tyranid.cloud.aws.{ S3, S3Bucket }
import org.tyranid.db.mongo.Imp._
import org.tyranid.json.Json
import org.tyranid.http.Http
import org.tyranid.log.Event
import org.tyranid.time.Time

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
*/
  
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
          
      //println( "request: " + jsonReq.toJsonStr )
      val req = Http.POST( "https://app.zencoder.com/api/v2/jobs", jsonReq.toJsonStr, null, "application/json", Map( "Zencoder-Api-Key" -> apiKey ) )
      val result = req.s            
      
      //println( "zc res: " + result )
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
  
  // Need to copy these over because they are not solely owned by us:
  // See: https://forums.aws.amazon.com/message.jspa?messageID=371475
        
  def checkStatus( doc:DBObject, db:DBCollection, bkt:S3Bucket, key:String, s3Url:String ) = {
    val zformats = doc.a( 'zfmts )
    
    if ( zformats != null ) {
      var outerTries = 0
      var complete = false
                         
      while ( outerTries < 3 && !complete ) {
        status( doc ) match {
          case "finished" | "ready" =>
            db.update( Mobj( "_id" -> doc.id ), Mobj( $unset -> Mobj( "zfmts" -> 1, "zid" -> 1, "zoids" -> 1, "ztries" -> 1 ) ) )
            doc.removeField( "zfmts" )
            doc.removeField( "zid" )
            doc.removeField( "zoids" )
            doc.removeField( "ztries" )
            
            for ( fmt <- zformats ) {
              var ok = false
              var tries = 0
              
              while ( !ok && tries < 3 ) {
                try {
                  tries += 1
                  S3.move( bkt, key + ".TMP." + fmt, bkt, key + "." + fmt )
                  ok = true
                } catch {
                  case e =>
                    Thread.sleep( 3300 ) // sleep for 3 secs-- after a bit it SHOULD be available
                }
              }
            }
            
            complete = true
          case "failed" =>
            if ( !retryUpload( db, doc, s3Url ) ) {
              val zid = doc.i( 'zid )
              log( Event.Zencoder, "m" -> ( "Status came back as failed for job id " + zid + " on doc " + doc.s( '_id ) ) )
              complete = true
            }
          case "waiting" | "queued" | "assigning" | "processing" =>
            Thread.sleep( 5000 )
          case status =>
            val zid = doc.i( 'zid )
            log( Event.Zencoder, "m" -> ( "Status came back as: " + status + "for job id " + zid + " on doc " + doc.s( '_id ) ) )
            complete = true
        }
        
        outerTries += 1
      }
    }
  }
  
  private def retryUpload( db:DBCollection, doc:DBObject, s3Url:String ):Boolean = {
    var numTries = doc.i( 'ztries )
    
    if ( numTries < 3 ) {
      numTries += 1
      doc( 'ztries ) = numTries
      db.update( Mobj( "_id" -> doc.id ), Mobj( $set -> Mobj( "ztries" -> numTries ) ) )
      return upload( s3Url, doc.s( 'filename ), doc )
    }
    
    false
  }
  
  // Output states include waiting, queued, assigning, ready, processing, finished, failed, cancelled and no input.
  def status( doc:DBObject ) = {
    val zid = doc.i( 'zid )
    
    if ( zid > 0 ) {
      //GET https://app.zencoder.com/api/v2/jobs/1234.xml?api_key=asdf1234
      val statusJson = Json.parse( Http.GET( "https://app.zencoder.com/api/v2/jobs/" + zid + ".json?api_key=" + apiKey ).s )
      
      val job = statusJson.get( "job" )
      val state = job.s( 'state )
      state
    } else {
      null
    }
  }  
}

