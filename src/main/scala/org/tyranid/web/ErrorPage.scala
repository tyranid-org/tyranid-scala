
package org.tyranid.web

import org.tyranid.Imp._


object Errorlet extends Weblet {

  def handle( web:WebContext ) {

    rpath match {
    case "/404" =>

      val originalUrl = web.req.getAttribute( "javax.servlet.forward.request_uri" )

      if ( originalUrl != null )
        log( Event.Error404, "m" -> originalUrl )

      web.template( <tyr:404/> )

    case "/throw" =>
      throw new RuntimeException( "test exception" )

    case _ =>
      _404
    }
  }
}

