
package org.tyranid.linkedIn

import javax.servlet.http.Cookie

import scala.collection.mutable
import scala.xml.Unparsed

import org.tyranid.Imp._
import org.tyranid.db.mongo.Imp._
import org.tyranid.oauth.{ OAuth, Token }
import org.tyranid.profile.User
import org.tyranid.session.Session
import org.tyranid.web.{ WebContext, Weblet }


object LinkedIn {

  lazy val oauth = OAuth( key = B.linkedInApiKey, secret = B.linkedInSecretKey )

  def exchangeToken( cookie:Cookie ) {

    val json = cookie.getValue.decUrl.parseJson.as[mutable.Map[String,Any]]

    val memberId    = json( 'member_id ).as[String]
    val accessToken = json( 'access_token ).as[String]
    val signature = json( 'signature ).as[String]

    val text = new StringBuilder
    for ( fieldName <- json( 'signature_order ).as[Array[String]] )
      text ++= json( fieldName ).toString

    val calcSignature = OAuth.hmacSha1( text.toString, B.linkedInSecretKey )

    if ( calcSignature != signature )
      throw new RuntimeException( "Failed signature match." )

    val exchangeUrl = "https://api.linkedin.com/uas/oauth/accessToken"

    val params = mutable.Map(
      "oauth_consumer_key"         -> oauth.key,
      "xoauth_oauth2_access_token" -> accessToken,
      "oauth_signature_method"     -> OAuth.signatureMethod )

    oauth.sign( "POST", exchangeUrl, params )

    val str = exchangeUrl.POST( content = OAuth.encParams( params ), contentType = "application/x-www-form-urlencoded" )

    val update = Mobj()

    update( 'liid ) = memberId
    for ( rslt <- str.splitAmp;
          ( key, value ) = rslt.splitFirst( '=' ) ) {
      key match {
      case "oauth_token"                    => update( 'lit )  = value
      case "oauth_token_secret"             => update( 'lits ) = value
      case "oauth_expires_in"               => // should be 0
      case "oauth_authorization_expires_in" => // should be 0
      case _ =>
      }
    }

    val session = Session()
    val user = session.user

    val usersdb = Mongo.connect.db( B.profileDbName )( "users" )
    val existing = usersdb.findOne( Mobj( "liid" -> memberId ) )
    if ( existing != null && existing.id != user.id )
      usersdb.update( Mobj( "_id" -> existing.id ), Mobj( $unset -> Mobj( "liid" -> 1, "lit" -> 1, "lits" -> 1 ) ) )

    // this way of getting the users db is a hack, need to move more knowledge of user schema into tyranid
    usersdb.update( Mobj( "_id" -> user.id ), Mobj( $set -> update ) )
    user.copy( update )
  }

  def tokenFor( user:User ) = Token( key = user.s( 'lit ), secret = user.s( 'lits ) )

  def GET( url:String, user:User ) = oauth.GET( url, tokenFor( user ), headers = Map( "x-li-format" -> "json" ) )
}

object LinkedInlet extends Weblet {

  def handle( ctx:WebContext ) {
    val s = Session()
    val u = s.user

    ctx.path match {
    case "/linkedin/exchange" =>

      val cookieName = "linkedin_oauth_" + B.linkedInApiKey
      ctx.req.getCookies.find( _.getName == cookieName ) match {
      case Some( cookie ) =>
        LinkedIn.exchangeToken( cookie )

      case None =>
        log( Log.LinkedIn, "m" -> ( "/linkedin/exchange missing " + cookieName + " cookie.  Cannot exchange linked in bearer token for a server token." ) )
      }

      ctx.res.ok

    case _ =>
      ctx.res.ok
    }
  }
}

