
package org.tyranid.web

import org.tyranid.Imp._


object Errorlet extends Weblet {

  def handle( web:WebContext ) {

    rpath match {
    case "/404" =>

      log( Log.Error404, "m" -> web.req.getAttribute( "javax.servlet.forward.request_uri" ) )

      web.template( <tyr:404/> )

    case "/throw" =>
      throw new RuntimeException( "test exception" )
    }
  }
}

