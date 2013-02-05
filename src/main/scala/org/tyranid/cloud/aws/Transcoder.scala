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

// TODO!!!

// This cannot be used until it supports ogg or webm output formats (for Firefox)

package org.tyranid.cloud.aws

import java.util.Date

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

import com.mongodb.{ DBObject, DBCollection }

import org.tyranid.Imp._
import org.tyranid.db.mongo.Imp._
import org.tyranid.http.Http
import org.tyranid.json.Json
import org.tyranid.time.Time

object Transcoder {
  private val supportedFormats = List( "3gp", "asf", "avi", "divx", "flv", "mkv", "mov", "mp4", "mpeg", "mpeg-ps", "mpeg-ts", "mxf", "ogg", "vob", "wav", "webm" )
  def supports( filename:String ) = supportedFormats.contains( filename.suffix( '.' ).toLowerCase )
  
  val PRESET_WEB = "1351620000000-100070" // Facebook, SmugMug, Vimeo, YouTube 
  val pipeline_standardId = "1360088105484-015cff"
  val pipeline_priorityId = "1360088128950-3550cc"
    
  val endpoint = "elastictranscoder.us-east-1.amazonaws.com"
  //val west_endpoint = "elastictranscoder.us-west-1.amazonaws.com"
  //val west_2_endpoint = "elastictranscoder.us-west-2.amazonaws.com"

  def request( method:String, uri:String, queryString:String, payload:String = "" ) = {
    val cal = Time.createUtcNowCalendar
    val date = cal.getTime

    val headers = Map[String,String](
        "x-amz-date" -> date.toIso8601,
        "host" -> endpoint,
        "content-type" -> "application/x-amz-json-1.0"
    )
    
    val authHeader = new Signature4( method, uri, queryString, headers, payload, service = "elastictranscoder", cal = cal ).authHeader
    
    if ( method == "GET" )
      Http.GET( "https://" + endpoint + uri + ( queryString.notBlank |* ( "?" + queryString ) ), null, headers = ( headers + authHeader ) )
    else
      Http.POST( "https://" + endpoint + uri + ( queryString.notBlank |* ( "?" + queryString ) ), payload, null, null, headers = ( headers + authHeader ) )
  }
  
  // http://docs.aws.amazon.com/elastictranscoder/latest/developerguide/create-job.html
  def upload( doc:DBObject ):Boolean = {
    val inputKey = doc.s( 'parentFolder ) + "/" + doc.id._s
/*
{
   "Input":{
      "Key":"name of the file to transcode",
      "FrameRate":"auto"|"10"|"15"|"23.97"|"24"|"25"|"29.97"|"30"|"60",
      "Resolution":"auto"|"width in pixelsxheight in pixels",
      "AspectRatio":"auto"|"1:1"|"4:3"|"3:2"|"16:9",
      "Interlaced":"auto"|"true"|"false",
      "Container":"auto"|"3gp"|"asf"|"avi"|"divx"|"flv"|"mkv"|"mov"|"mp4"|"mpeg"|"mpeg-ps"|"mpeg-ts"|"mxf"|"ogg"|"vob"|"wav"|"webm"
   },
   "Output":{
      "Key":"name of the transcoded file",
      "ThumbnailPattern":""|"pattern",
      "Rotate":"auto"|"0"|"90"|"180"|"270",
      "PresetId":"preset to use for the job"
   },
   "PipelineId":"pipeline to add the job to"
}
*/    
    val payload = Map(
         "Input" -> Map(
             "Key" -> inputKey,
             "FrameRate" -> "auto",
             "Resolution" -> "auto",
             "AspectRatio" -> "auto",
             "Interlaced" -> "auto",
             "Container" -> "auto"
         ),
         "Output" -> Map(
            "Key" -> ( inputKey + ".mp4" ),
            "ThumbnailPattern" -> "",
            "Rotate" -> "0",
            "PresetId" -> PRESET_WEB
         ),
         "PipelineId" -> pipeline_standardId
        )
        
    val req = request( "POST", "/2012-09-25/jobs", null, payload.toJsonStr )
    val result = req.s            
      
    if ( req.response.getStatusLine().getStatusCode() != 200 ) {
      log( Event.Transcoder, "m" -> ( "Failed to upload video: " + inputKey + ", error=" + result ) )
    } else {
      //{"outputs":[{"label":null,"url":"https://s3.amazonaws.com/files.volerro.com/5069a80ad748dff278930a82/50d2180fd748c9c33a8e8b0f.ogg","id":66443523},{"label":null,"url":"https://s3.amazonaws.com/files.volerro.com/5069a80ad748dff278930a82/50d2180fd748c9c33a8e8b0f.mp4","id":66443525}],"test":true,"id":34152199}
      val res = Json.parse( result )
      val job = res.get( "Job" )
      
      doc( 'trid ) = job.s( 'Id )
    }
    
/*
{
   "Job":{
      "Id":"3333333333333-abcde3"
      "Input":{
         "AspectRatio":"auto",
         "Container":"mp4",
         "FrameRate":"auto",
         "Interlaced":"auto",
         "Key":"cooking/lasagna.mp4",
         "Resolution":"auto"
      },
      "Output":{
         "Key":"",
         "PresetId":"5555555555555-abcde5",
         "Rotate":"0",
         "Status":"Submitted",
         "StatusDetail":"Job has been received.",
         "ThumbnailPattern":"cooking/lasagna-{count}"
      },
      "PipelineId":"1111111111111-abcde1"
   }
} 
*/
    return true
  }
  
  def checkStatus( doc:DBObject, db:DBCollection, bkt:S3Bucket, key:String, s3Url:String ) = {
    val jobId = doc.s( 'trid )
    
    if ( jobId.notBlank ) {
      var outerTries = 0
      var complete = false
                         
      while ( outerTries < 3 && !complete ) {
        val state = status( doc )
        
        state match {
          case "Completed" | "ready" =>
            db.update( Mobj( "_id" -> doc.id ), Mobj( $unset -> Mobj( "trid" -> 1, "ztries" -> 1 ) ) )
            complete = true
          case "Error" | "Canceled" =>
            if ( !retryUpload( db, doc, s3Url ) ) {
              val zid = doc.i( 'zid )
              log( Event.Zencoder, "m" -> ( "Status came back as " + state + " for job id " + zid + " on doc " + doc.s( '_id ) ) )
              complete = true
            }
          case "Submitted" | "In Progess" =>
            Thread.sleep( 5000 )
          case _ =>
            log( Event.Zencoder, "m" -> ( "Status came back as: " + state + "for job id " + jobId + " on doc " + doc.s( '_id ) ) )
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
      return upload( doc )
    }
    
    false
  }
  
  // Output states include waiting, queued, assigning, ready, processing, finished, failed, cancelled and no input.
  def status( doc:DBObject ) = {
    val jobId = doc.s( 'trid )
    
    if ( jobId.notBlank ) {
      val inputKey = doc.s( 'parentFolder ) + "/" + doc.id._s
      val req = request( "GET", "/2012-09-25/jobs/" + jobId, null, null )
      val result = req.s            
        
      if ( req.response.getStatusLine().getStatusCode() != 200 ) {
        log( Event.Transcoder, "m" -> ( "Failed to get status for video: " + inputKey + ", error=" + result ) )
        null
      } else {
        val res = Json.parse( result )
        val job = res.get( "Job" )
        val output = job.get( "Output" )
        val status = output.s( "Status" )
        
        status
      }
    } else {
      null
    }
  }    
}