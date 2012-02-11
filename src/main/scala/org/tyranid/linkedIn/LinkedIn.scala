
package org.tyranid.linkedIn

import scala.collection.mutable
import scala.xml.Unparsed

import org.tyranid.Imp._
import org.tyranid.session.Session
import org.tyranid.web.{ WebContext, Weblet }


object LinkedIn {

  def parseExchangeToken( jsonStr:String ) {

    val json = jsonStr.parseJson.as[mutable.Map[String,Any]]

    val memberId = json( 'member_id ).as[String]
    spam( "memberId=" + memberId )

    for ( fieldName <- json( 'signature_order ).as[Array[String]] )
      spam( "fieldName:" + fieldName )

    //spam( "signatureOrder: " + signatureOrder )
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
        spam( "value:" + cookie.getValue.decUrl )
        LinkedIn.parseExchangeToken( cookie.getValue.decUrl )

      case None =>
        log( Log.LinkedIn, "m" -> ( "/linkedin/exchange missing " + cookieName + " cookie.  Cannot exchange linked in bearer token for a server token." ) )
      }

      ctx.res.ok

    case _ =>
      ctx.res.ok
    }
  }
}

