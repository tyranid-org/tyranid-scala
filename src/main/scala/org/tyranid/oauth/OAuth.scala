
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

    params.put( "oauth_signature", sig)

    val rslt = accessTokenUrl.POST( params )

    spam( "rslt=" + rslt )
  }

  def sign( method:String, url:String, params:collection.Map[String, String], tokenSecret:String ) = {

    var key = consumerSecret.encOAuthUrl + '&'

    if ( tokenSecret.notBlank )
      key += tokenSecret.encOAuthUrl

    val text = method + "&" + url.encOAuthUrl + "&" + OAuth.encParams( params ).encOAuthUrl
spam( "text=\n\n" + text + "\n\n" )

    OAuth.hmacSha1( text, key )
  }
}

