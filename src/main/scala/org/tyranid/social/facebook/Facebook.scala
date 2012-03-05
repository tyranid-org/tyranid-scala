
package org.tyranid.social.facebook

import org.tyranid.Imp._
import org.tyranid.http.Http
import org.tyranid.session.Session
import org.tyranid.time.Time
import org.tyranid.web.{ Weblet, WebContext }


object Facebook {
}

object Facebooklet extends Weblet {

  def handle( web:WebContext ) {
    val s = Session()
    val u = s.user

    rpath match {
    case "/channel" =>
      web.res.html(
        <script src="//connect.facebook.net/en_US/all.js"/>,
        headers = Http.expireCacheControlHeaders( ageMs = Time.OneYearMs )
      )

    case _ =>
      _404
    }
  }
}

