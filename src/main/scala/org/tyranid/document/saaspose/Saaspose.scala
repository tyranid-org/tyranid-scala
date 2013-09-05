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

package org.tyranid.document.saaspose

import java.io.InputStream

import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;

import scala.collection.mutable
import scala.collection.mutable.Buffer

import java.io.{ File, FileInputStream, FileOutputStream }
import java.net.{ HttpURLConnection, URL, URLEncoder }

import com.mongodb.{ DBObject, DBCollection }

import org.tyranid.Imp._
import org.tyranid.cloud.aws.{ S3, S3Bucket }
import org.tyranid.db.mongo.Imp._
import org.tyranid.json.Json
import org.tyranid.http.Http
import org.tyranid.log.Event
import org.tyranid.math.Base64
import org.tyranid.time.Time


object Saaspose {
  
}

case class SaasposeApp( appSid:String, appKey:String ) {
  val fileUploadUri = "http://api.saaspose.com/v1.0/storage/file/"
  val slidesUri = "http://api.aspose.com/v1.1/slides/"
  
  def signUrl( uri:String ) = {
    val url = uri.replace( " ","%20" ) + "&appSID=" + appSid

    // get an hmac_sha1 key from the raw key bytes
    val signingKey = new SecretKeySpec( appKey.getBytes(), "HmacSHA1" )//HMAC_SHA1_ALGORITHM

    // get an hmac_sha1 Mac instance and initialize with the signing key
    val mac = Mac.getInstance("HmacSHA1")//HMAC_SHA1_ALGORITHM
    mac.init(signingKey)

    // compute the hmac on input data bytes
    val rawHmac = mac.doFinal( url.getBytes() )
    val newresult = Base64.toString( rawHmac )
    println( "base 64: " + newresult )
    
    // Remove invalid symbols.
    val result = newresult.endsWith( "/" ) ? newresult.prefix( '/' ) | newresult 
    
//    val result = newresult.substring( 0, newresult.length - 1 )
    val encodedUrl = URLEncoder.encode( result,"UTF-8" ) 
    
    url + "&signature=" + encodedUrl
  }
    
  def getBytesFromFile( file:File) = {
    val source = scala.io.Source.fromFile(file)(scala.io.Codec.ISO8859)
    val byteArray = source.map(_.toByte).toArray
    source.close()
    byteArray
  }
  
  def streamToString( stream:InputStream ) = {
    val br = new java.io.BufferedReader( new java.io.InputStreamReader( stream ) )
    val sb = new StringBuilder()

    var line = ""
    var notDone = true
    
    while ( notDone ) {
      val line = br.readLine()
      
      if ( line.isBlank ) {
        notDone = false
      } else {
        sb.append( line )
      }
    }

    br.close()
    sb._s
  }
  
  def uploadBinaryFile( localFile:File, uploadUrl:String, strHttpCommand:String ) = {
    val url = new URL( uploadUrl )
    val buf = getBytesFromFile(localFile)
    val m_connection = url.openConnection().as[HttpURLConnection]  
    
    //String parameters = "data=some_post_data"  
    m_connection.setDoOutput(true)
    m_connection.setRequestMethod( strHttpCommand )
    m_connection.setRequestProperty("Accept", "text/json")
    m_connection.setRequestProperty("Content-Type", "MultiPart/Form-Data")  
    //byte bytes[] = parameters.getBytes();  
    m_connection.setRequestProperty("Content-length", "" + buf.length)  
    m_connection.connect()
    
    val out  = m_connection.getOutputStream()  
    out.write(buf)  
    out.flush()

    try {
      val response = m_connection.getInputStream()
    
      streamToString( response )
    } catch {
      case e:Exception =>
        e.printStackTrace()
        throw e
    }
  }

  def uploadBinaryFile2( localFile:File, uploadUrl:String, strHttpCommand:String, format:String = "PPTX" ) = {
    val url = new URL( uploadUrl )
    //val buf = getBytesFromFile(localFile)
    val connection = url.openConnection().as[HttpURLConnection]  
    
    //String parameters = "data=some_post_data"  
    connection.setRequestMethod( strHttpCommand )
    //connection.setRequestProperty("Accept", "text/json")
    connection.setRequestProperty("Content-Type", "MultiPart/Form-Data")  
    //byte bytes[] = parameters.getBytes();  
    connection.setRequestProperty("Content-length", "" + localFile.length)
    connection.setUseCaches( false )
    connection.setDoOutput(true)
    connection.connect()
    
    val out  = connection.getOutputStream()  
    
    val connIn = new FileInputStream( localFile )
    val connBufIn = new java.io.BufferedInputStream( connIn )
    
    connBufIn.transferTo( out, true )

    val tmpFile = File.createTempFile( localFile.getName, "." + format.toLowerCase )
    val fileOut = new FileOutputStream( tmpFile )
    connection.getInputStream().transferTo( fileOut, true )
  
    tmpFile
  }
  
  def processCommand( strURI:String, strHttpCommand:String ) = {
    val address = new URL( strURI )
    val httpCon = address.openConnection().as[HttpURLConnection]
    httpCon.setDoOutput(true)
    httpCon.setRequestProperty("Content-Type", "application/json" )
    httpCon.setRequestProperty("Accept", "text/json") 
    httpCon.setRequestMethod(strHttpCommand)
    
    if ( strHttpCommand == "PUT" || strHttpCommand == "POST" )
      httpCon.setFixedLengthStreamingMode(0)
    
    //val d = httpCon.getResponseMessage()
    //System.out.println(d);
    httpCon.getInputStream
  }
  
  def convert2( file:File, format:String = "PPTX" ): File = {
    val url = B.saaspose.slidesUri + "convert?format=" + format
    println(url)
    val uploadUrl = signUrl( url )
    println( uploadUrl )
    
    uploadBinaryFile2( file, uploadUrl, "PUT", format )
  }
  
  def convert( file:File, format:String = "PPTX" ): File = {
    val filename = file.getName
    val uploadUrl = signUrl( B.saaspose.fileUploadUri + filename )
    
    val responseStr = uploadBinaryFile( file, uploadUrl, "PUT" )
    
    println( "Upload: " + Json.parse( responseStr ).toJsonStr( true ) )
    
    
    val slidesUri = B.saaspose.slidesUri + filename + "?format=" + format
    val slidesUrl = signUrl( slidesUri )

    //return Http.GET_File( slidesUrl, ext = ".pptx", headers = Map(
    //    "Content-Type" -> "application/json",
    //    "Accept" -> "text/json"
    //    ) )
    
    val responseStream = processCommand( slidesUrl, "GET" )

    val tmpFile = File.createTempFile( file.getName, "." + format.toLowerCase )
    val out = new FileOutputStream( tmpFile )
    responseStream.transferTo( out, true )

    /*
    val deleteUri = "http://api.saaspose.com/v1.0/slides/" + filename + "/slides"
    val deleteUrl = signUrl( deleteUri ) 
    
    val resStream = processCommand( deleteUrl, "DELETE" )
    
    val json = Json.parse( streamToString( resStream ) )
          
    println( json.toJsonStr( true ) )
    */    
    
    tmpFile
  }
  
  // Need to copy these over because they are not solely owned by us:
  // See: https://forums.aws.amazon.com/message.jspa?messageID=371475
        
  def checkStatus( doc:DBObject, db:DBCollection, bkt:S3Bucket, key:String, s3Url:String ) = {
    null
  }
  
  /*
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
  */
  
  // Output states include waiting, queued, assigning, ready, processing, finished, failed, cancelled and no input.
  def status( doc:DBObject ) = {
    val zid = doc.i( 'zid )
    
    if ( zid > 0 ) {
      //GET https://app.zencoder.com/api/v2/jobs/1234.xml?api_key=asdf1234
      val statusJson = Json.parse( Http.GET( "https://app.zencoder.com/api/v2/jobs/" + zid + ".json?api_key=" + appKey ).s )
      
      val job = statusJson.get( "job" )
      val state = job.s( 'state )
      state
    } else {
      null
    }
  }  
}

