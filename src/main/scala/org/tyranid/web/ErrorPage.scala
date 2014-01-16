/**
 * Copyright (c) 2008-2013 Tyranid <http://tyranid.org>
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

package org.tyranid.web

import org.tyranid.Imp._

object Errorlet extends Weblet {

  def handle( web:WebContext ) {

    rpath match {
    case "/404" =>
      
      val originalUrl = web.req.getAttribute( "javax.servlet.forward.request_uri" )._s

      if ( T.session.ua( web ).isIE && ( originalUrl.startsWith( "/\"" ) || originalUrl.startsWith( "/%22/" ) ) ) {
        web.res.ok
        return
      }
      
      println( originalUrl )
      
      if ( originalUrl != null )
        log( Event.Error404, "p" -> originalUrl )
        
      if ( WebFilter.notAsset( originalUrl ) )
        web.template( <tyr:404/> )
    case "/throw" =>
      throw new RuntimeException( "test exception" )

    case _ =>
      _404
    }
  }
}

