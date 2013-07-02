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

package org.tyranid.cloud.aws

import java.security.MessageDigest
import java.util.{ Date, Calendar }

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

import scala.collection.mutable
import scala.collection.mutable.Buffer

import org.tyranid.Imp._
import org.tyranid.time.Time

object Aws {
  // http://docs.aws.amazon.com/general/latest/gr/sigv4-create-canonical-request.html
  def canonicalRequestWithSignedHeaders( method:String, canonicalURI:String, queryString:String, headers:Map[String,String], payload:String ) = {
    val qsParts = queryString.isBlank ? null | queryString.split( "&" ) 
    val queryStringParams = mutable.ArrayBuffer[String]()

    if ( qsParts != null )
      for ( part <- qsParts )
        queryStringParams += part
       
    val sortedQueryStringParams = queryStringParams.sortBy( _.charAt( 0 ) )
    
    // METHOD
    val canonicalRequest = new StringBuilder( method ) += '\n'
      
    // URI
    canonicalRequest ++= canonicalURI += '\n'
      
    // QUERY STRING
    canonicalRequest ++= sortedQueryStringParams.mkString( "&" )
    canonicalRequest += '\n'
    
    // HEADERS
    val sortedHeaders = headers.toSeq.sortBy( _._1.toLowerCase.charAt( 0 ) )
    var hostFound = false
    
    for ( header <- sortedHeaders ) {
      val name = header._1.toLowerCase.trim
      canonicalRequest ++= name += ':' ++= header._2.trim += '\n'
      hostFound = hostFound || name == "host"
    }

    if ( !hostFound )
      throw new RuntimeException( "Need to provide 'Host' header!" )

    canonicalRequest += '\n' 
      
    // SIGNED HEADERS
    val signedHeaders = sortedHeaders.map( sh => sh._1.toLowerCase ).mkString( ";" )
    canonicalRequest ++= signedHeaders += '\n'
      
    // PAYLOAD
    canonicalRequest ++= SHA256( payload ).toHexString
    
    ( canonicalRequest._s, signedHeaders )
  }
  
  def canonicalRequest( method:String, canonicalURI:String, queryString:String, headers:Map[String,String], payload:String ) =
    canonicalRequestWithSignedHeaders( method, canonicalURI, queryString, headers, payload )._1
  
  def stringToSign( algorithm:String, requestDate:Date, credentialScope:String, canonicalRequest:String ) = {
    algorithm + '\n' + 
    requestDate.toAmzFormat + '\n' +
    credentialScope + '\n' +
    SHA256( canonicalRequest ).toHexString  
  }
  
  def HmacSHA256( data:String, key:Array[Byte] ) = {
     val algorithm = "HmacSHA256"
     val mac = Mac.getInstance( algorithm )
     mac.init( new SecretKeySpec( key, algorithm ) )
     mac.update( data.isBlank ? "".getBytes() | data.getBytes( "UTF8" ) )
     mac.doFinal()
  }
  
  def SHA256( data:String ) = MessageDigest.getInstance("SHA-256").digest( data.isBlank ? "".getBytes() | data.getBytes( "UTF8" ) )

  def getSignatureKey( key:String, dateStamp:String, regionName:String, serviceName:String ) = {
     val kSecret = ( "AWS4" + key ).getBytes( "UTF8" )
     val kDate   = HmacSHA256( dateStamp, kSecret )
     val kRegion  = HmacSHA256( regionName, kDate )
     val kService = HmacSHA256( serviceName, kRegion )
     
     HmacSHA256( "aws4_request", kService )
  }  
}

class Signature4( method:String, uri:String, queryString:String, headers:Map[String,String], payload:String, region:String = "us-east-1", service:String = "elastictranscoder", cal:Calendar = Time.createUtcNowCalendar ) {
  val date = cal.getTime
  val dateRfc1123 = date.toRfc1123GMT
  val dateAmz = date.toAmzFormat.substring( 0, 8 ) 
  
  val host = service match {
    case "elastictranscoder" =>
      "elastictranscoder.us-east-1.amazonaws.com"
    case _ =>
      "UNKNOWN"
  }
    
  val credentialScope = dateAmz + "/" + region + "/" + service + "/aws4_request"
  
  val ( canonicalRequest, signedHeaders ) = Aws.canonicalRequestWithSignedHeaders( method, uri, queryString, headers, payload )
  
  val stringToSign = Aws.stringToSign( "AWS4-HMAC-SHA256", date, credentialScope, canonicalRequest )
  
  val signedString = Aws.getSignatureKey( B.awsCredentials.getAWSSecretKey, dateAmz, region, service )
  
  val signature = Aws.HmacSHA256( stringToSign, signedString ).toHexString

  //Authorization: AWS4-HMAC-SHA256 Credential=AccessKeyID/20120116/us-east-1/ets/aws4_request,SignedHeaders=host;x-amz-date;x-amz-target,Signature=145b1567ab3c50d929412f28f52c45dbf1e63ec5c66023d232a539a4afd11fd9  
  val authHeader = ( "Authorization", "AWS4-HMAC-SHA256 Credential=" + B.awsCredentials.getAWSAccessKeyId + "/" + credentialScope + ", SignedHeaders=" + signedHeaders + ", Signature=" + signature )
}