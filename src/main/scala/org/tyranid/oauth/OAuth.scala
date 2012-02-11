
package org.tyranid.oauth

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

import scala.collection.mutable

import org.tyranid.Imp._
import org.tyranid.math.Base64


object OAuth {

  val signatureMethod = "HMAC-SHA1"
  val version = "1.0"

	def encOAuthUrl( base:String ) = base.encUrl.replace("+", "%20").replace("*", "%2A").replace("%7E", "~")

  def decOAuthUrl( encoded:String ) = {

    var ch:Char = 0

    val sb = new StringBuilder

    var bytePattern = 0
    var sumb = 0
    var more = -1

    var i = 0
    while ( i < encoded.length ) {
      ch = encoded.charAt( i )

      ch match {
      case '%' =>
        i += 1
        ch = encoded.charAt( i )
        val hb = ch.hexDigit
        i += 1
        ch = encoded.charAt( i )
        val lb = ch.hexDigit
        bytePattern = (hb << 4) | lb

      case '+' =>
        bytePattern = ' '

      case _ =>
        bytePattern = ch
      }

      if ( (bytePattern & 0xc0) == 0x80 ) { // 10xxxxxx
        sumb = (sumb << 6) | (bytePattern & 0x3f)
        more -= 1
        if ( more == 0 )
          sb += sumb.toChar
      } else if ( (bytePattern & 0x80) == 0x00 ) { // 0xxxxxxx
        sb += bytePattern.toChar
      } else if ( (bytePattern & 0xe0) == 0xc0 ) { // 110xxxxx
        sumb = bytePattern & 0x1f
        more = 1
      } else if ( (bytePattern & 0xf0) == 0xe0 ) { // 1110xxxx
        sumb = bytePattern & 0x0f
        more = 2
      } else if ( (bytePattern & 0xf8) == 0xf0 ) { // 11110xxx
        sumb = bytePattern & 0x07
        more = 3
      } else if ( (bytePattern & 0xfc) == 0xf8 ) { // 111110xx
        sumb = bytePattern & 0x03
        more = 4
      } else { // 1111110x
        sumb = bytePattern & 0x01
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

case class OAuth( consumerKey:String, consumerSecret:String ) {

  def putOAuthParams( params:mutable.Map[String, String] ) {
    params( "oauth_consumer_key" )     = consumerKey
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

    val rslt = accessTokenUrl.POST( params )

    spam( "rslt=" + rslt )
  }

  def sign( method:String, url:String, params:mutable.Map[String, String], tokenSecret:String = null ) =
    params( "oauth_signature" ) = signature( method, url, params, tokenSecret )

  def signature( method:String, url:String, params:collection.Map[String, String], tokenSecret:String = null ) = {

    var key = consumerSecret.encOAuthUrl + '&'

    if ( tokenSecret.notBlank )
      key += tokenSecret.encOAuthUrl

    val text = method + "&" + url.encOAuthUrl + "&" + OAuth.encParams( params ).encOAuthUrl

    OAuth.hmacSha1( text, key )
  }

  def send( url:String,
            params:mutable.Map[String, String],
            headerParams:mutable.Map[String, String],
            body:String,
            method:String,
            tokenKey:String,
            tokenSecret:String,
            isHeaderRequired:Boolean ) {
    params( "oauth_token" ) = tokenKey
    putOAuthParams( params )

    var eUrl:String = null

    val idx = url.indexOf( '?' )

    if ( idx != -1 ) {
      eUrl = url.substring( 0, idx )

      val pairs = url.substring( idx+1 ).splitAmp
      for ( pair <- pairs ) {
        val eq = pair.indexOf( '=' )
        if ( eq == -1 ) {
          params( pair ) = ""
        } else {
          params( pair.substring(0, eq) ) = pair.substring(eq + 1).decOAuthUrl
        }
      }
    } else {
      eUrl = url
    }

    sign( method, url, params, tokenSecret )
/*
    Map<String, String> headerMap = null;
    if (isHeaderRequired) {
      String headerVal = getAuthHeaderValue(params);
      headerMap = new HashMap<String, String>();
      headerMap.put("Authorization", headerVal);
      if (headerParams != null) {
        for (String key : headerParams.keySet()) {
          headerMap.put(key, headerParams.get(key));
        }
      }
      url = reqURL;
    } else {
      url += "?" + OAuth.encParams( params )
    }

    return HttpUtil.doHttpRequest(url, methodName, body, headerMap);
*/
  }
}

