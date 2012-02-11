
package org.tyranid.linkedIn

import javax.servlet.http.Cookie

import scala.collection.mutable
import scala.xml.Unparsed

import org.tyranid.Imp._
import org.tyranid.math.Base64
import org.tyranid.session.Session
import org.tyranid.web.{ WebContext, Weblet }


object LinkedIn {

  def exchangeToken( cookie:Cookie ) {

    val json = cookie.getValue.decUrl.parseJson.as[mutable.Map[String,Any]]

    val memberId = json( 'member_id ).as[String]
    spam( "memberId=" + memberId )

    val sb = new StringBuilder
    for ( fieldName <- json( 'signature_order ).as[Array[String]] )
      sb ++= json( fieldName ).toString

    val secret = Tyr.linkedInSecretKey
    val data = sb.toString
    val signature = json( 'signature ).as[String]

    val mac = javax.crypto.Mac.getInstance( "HmacSHA1" )
    val secretSpec = new javax.crypto.spec.SecretKeySpec( secret.getBytes, "HmacSHA1" )
    mac.init( secretSpec )
    val digest = mac.doFinal( data.getBytes )
    val enc = Base64.toStringNormal( digest )

    if ( enc != signature )
      throw new RuntimeException( "Failed signature match." )


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

