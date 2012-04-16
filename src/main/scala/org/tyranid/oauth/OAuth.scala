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

package org.tyranid.oauth

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

import scala.collection.mutable

import org.tyranid.Imp._
import org.tyranid.http.{ Http, HttpResult }
import org.tyranid.math.Base64


trait Grant {
  val key:String
  val secret:String
}

case class Token( key:String, secret:String ) extends Grant

object OAuth {

  val signatureMethod = "HMAC-SHA1"
  val version = "1.0"

  val signedHeaders = Seq(
    // required
    "oauth_consumer_key",
    "oauth_nonce",
    "oauth_timestamp",
    "oauth_signature_method",
    // optional
    "oauth_version",
    "oauth_token",
    "oauth_signature"
  )

	def encOAuthUrl( s:String ) = s.encUrl.replace("+", "%20").replace("*", "%2A").replace("%7E", "~")

  def decOAuthUrl( s:String ) = {

    var ch:Char = 0
    var utfch = 0

    val sb = new StringBuilder

    var byte = 0
    var more = -1

    var i = 0
    while ( i < s.length ) {
      ch = s.charAt( i )

      ch match {
      case '%' =>
        i += 1
        val hb = s.charAt( i ).hexDigit
        i += 1
        val lb = s.charAt( i ).hexDigit
        byte = (hb << 4) | lb

      case '+' =>
        byte = ' '

      case _ =>
        byte = ch
      }

      // UTF-8 decode
      if ( (byte & 0xc0) == 0x80 ) {        // 10xxxxxx
        utfch = (utfch << 6) | (byte & 0x3f)
        more -= 1
        if ( more == 0 )
          sb += utfch.toChar
      } else if ( (byte & 0x80) == 0x00 ) { // 0xxxxxxx
        sb += byte.toChar
      } else if ( (byte & 0xe0) == 0xc0 ) { // 110xxxxx
        utfch = byte & 0x1f
        more = 1
      } else if ( (byte & 0xf0) == 0xe0 ) { // 1110xxxx
        utfch = byte & 0x0f
        more = 2
      } else if ( (byte & 0xf8) == 0xf0 ) { // 11110xxx
        utfch = byte & 0x07
        more = 3
      } else if ( (byte & 0xfc) == 0xf8 ) { // 111110xx
        utfch = byte & 0x03
        more = 4
      } else {                              // 1111110x
        utfch = byte & 0x01
        more = 5
      }

      i += 1
    }

    sb.toString
  }


  def hmacSha1( text:String, key:String ) = {
    val mac = Mac.getInstance( "HmacSHA1" )
    val keySpec = new SecretKeySpec( key.getBytes, "HmacSHA1" )
    mac.init( keySpec )
    Base64.toStringNormal( mac.doFinal( text.getBytes ) ).trim
  }

  def encParams( params:collection.Map[String,String] ) =
    params.toSeq.filter( _._2.notBlank ).map( pair => pair._1 + '=' + pair._2.encOAuthUrl ).sorted.mkString( "&" )
}

case class OAuth( key:String, secret:String ) extends Grant {

  def putOAuthParams( params:mutable.Map[String, String] ) {
    params( "oauth_consumer_key" )     = key
    params( "oauth_signature_method" ) = OAuth.signatureMethod
    params( "oauth_version" )          = OAuth.version

    val ts = System.currentTimeMillis
    params( "oauth_timestamp" )        = String.valueOf( ts / 1000 )
    params( "oauth_nonce" )            = String.valueOf( ts )
  }

  def exchange( accessTokenUrl:String, requestToken:String ) = {

    val params = mutable.Map[String,String]()

    // TODO:  oauth_verifier

    params.put( "oauth_token", requestToken )
    putOAuthParams( params )

    val sig = sign( "POST", accessTokenUrl, params, requestToken )

    accessTokenUrl.POST( params )
  }

  def sign( method:String, url:String, params:mutable.Map[String, String], tokenSecret:String = null ) =
    params( "oauth_signature" ) = signature( method, url, params, tokenSecret )

  def signature( method:String, url:String, params:collection.Map[String, String], tokenSecret:String = null ) = {

    var key = secret.encOAuthUrl + '&'

    if ( tokenSecret.notBlank )
      key += tokenSecret.encOAuthUrl
  
    val text = method + "&" + url.encOAuthUrl + "&" + OAuth.encParams( params ).encOAuthUrl

    OAuth.hmacSha1( text, key )
  }

  def GET( url:String,
           token:Token,
           params:mutable.Map[String,String] = null,
           headers:collection.Map[String,String] = null ) =
    request( "GET", url, params, headers, token )

  def POST( url:String,
            token:Token,
            params:mutable.Map[String,String] = null,
            headers:collection.Map[String,String] = null ) =
    request( "POST", url, params, headers, token )

  def request( method:String,
               url:String,
               params:mutable.Map[String,String],
               headers:collection.Map[String,String],
               token:Token ):HttpResult = {
    var eParams =
      if ( params != null ) params
      else                  mutable.Map[String,String]()

    eParams( "oauth_token" ) = token.key
    putOAuthParams( eParams )
    var eUrl = Http.extractQueryString( url, eParams, oauth = true )
    sign( method, eUrl, eParams, token.secret )

    val fullHeaders = this.headers( eParams, headers )

    method match {
    case "GET"  => eUrl.GET ( eParams, headers = fullHeaders )
    case "POST" => eUrl.POST( eParams, headers = fullHeaders )
    }
  }

  def headers( params:mutable.Map[String, String], headers:collection.Map[String,String] ) = {
    val fullHeaders:mutable.Map[String,String] = mutable.Map[String,String]()

    fullHeaders.put( "Authorization", authHeader( params ) )

    if ( headers != null )
      for ( key <- headers.keys )
        fullHeaders( key ) = headers( key )

    fullHeaders
  }

  /*

  OAuth oauth_consumer_key="0685bd9184jfhq22",oauth_token="ad180jjd733klru7",oauth_signature_method="HMAC-SHA1",oauth_signature="wOJIO9A2W5mFwDgiDvZbTSMK%2FPY%3D",oauth_timestamp="137131200", oauth_nonce="4572616e48616d6d65724c61686176",oauth_version="1.0"


  OAuth oauth_consumer_key="akzb237fsren",oauth_nonce="1328991634803",oauth_timestamp="1328991634",oauth_signature_method="HMAC-SHA1",oauth_version="1.0",oauth_token="50141d2d-2784-40d5-8455-9fe32882784c",oauth_signature="8q%2FBUvB756%2BBGEEfz8vhrd5cOoE%3D"

   */
  def authHeader( params:mutable.Map[String, String] ) = {
    val sb = new StringBuilder
    sb ++= "OAuth "
    var first = true

    for ( header <- OAuth.signedHeaders;
          value <- params.get( header ) ) {
      if ( first )
        first = false
      else
        sb += ','

      sb ++= header ++= "=\"" ++= value.encOAuthUrl += '"'
      params.remove( header )
    }

    sb.toString
  }
}

