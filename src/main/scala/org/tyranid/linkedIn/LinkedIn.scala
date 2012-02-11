
package org.tyranid.linkedIn

import javax.servlet.http.Cookie

import scala.collection.mutable
import scala.xml.Unparsed

import org.tyranid.Imp._
import org.tyranid.db.mongo.Imp._
import org.tyranid.oauth.OAuth
import org.tyranid.session.Session
import org.tyranid.web.{ WebContext, Weblet }


object LinkedIn {

  def makeOAuth = OAuth( consumerKey = Tyr.linkedInApiKey, consumerSecret = Tyr.linkedInSecretKey )

  def exchangeToken( cookie:Cookie ) {

    val json = cookie.getValue.decUrl.parseJson.as[mutable.Map[String,Any]]
spam( "cookie contained: " + json )

    val memberId    = json( 'member_id ).as[String]
    val accessToken = json( 'access_token ).as[String]
    val signature = json( 'signature ).as[String]

    val text = new StringBuilder
    for ( fieldName <- json( 'signature_order ).as[Array[String]] )
      text ++= json( fieldName ).toString

    val calcSignature = OAuth.hmacSha1( text.toString, Tyr.linkedInSecretKey )

    if ( calcSignature != signature )
      throw new RuntimeException( "Failed signature match." )

    val oauth = makeOAuth

    val exchangeUrl = "https://api.linkedin.com/uas/oauth/accessToken"

    val params = mutable.Map(
      "oauth_consumer_key"         -> oauth.consumerKey,
      "xoauth_oauth2_access_token" -> accessToken,
      "oauth_signature_method"     -> OAuth.signatureMethod )

    params( "oauth_signature" ) = oauth.sign( "POST", exchangeUrl, params, tokenSecret = null )

    /*

com.linkedin.security.auth.pub.LoginDeniedInvalidAuthTokenException
while obtaining request token for :

POST&https%3A%2F%2Fapi.linkedin.com%2Fuas%2Foauth%2FaccessToken&oauth_consumer_key%3Dakzb237fsren%26oauth_signature_method%3DHMAC-SHA1%26xoauth_oauth2_access_token%3DwzuOPVhwCay9aYm37vtXSyJduv3M859bAqxO
POST&https%3A%2F%2Fapi.linkedin.com%2Fuas%2Foauth%2Faccesstoken&oauth_consumer_key%3Dakzb237fsren%26oauth_signature_method%3DHMAC-SHA1%26xoauth_oauth2_access_token%3DwzuOPVhwCay9aYm37vtXSyJduv3M859bAqxO


CONN:O|164844|65429|62339144|173100|*02:1328940064:Lp4cLI3aE3GKmLb8QgQAKSP/hEE=

    */

    val str = exchangeUrl.POST( content = OAuth.encParams( params ), contentType = "application/x-www-form-urlencoded" )

    val update = Mobj()

    update( 'liid ) = memberId
    for ( rslt <- str.split( "&" );
          ( key, value ) = rslt.splitFirst( '=' ) ) {
      key match {
      case "oauth_token"                    => update( 'lit )  = value
      case "oauth_token_secret"             => update( 'lits ) = value
      case "oauth_expires_in"               => // should be 0
      case "oauth_authorization_expires_in" => // should be 0
      case _ =>
      }
    }

    // this way of getting the users db is a hack, need to move more knowledge of user schema into tyranid
    Mongo.connect.db( Tyr.profileDbName )( "users" ).update( Mobj( "_id" -> Session().user.id ), Mobj( $set -> update ) )
  }
}

object LinkedInlet extends Weblet {

  def handle( ctx:WebContext ) {
    val s = Session()
    val u = s.user

    ctx.path match {
    case "/linkedin/exchange" =>

      val cookieName = "linkedin_oauth_" + Tyr.linkedInApiKey
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

