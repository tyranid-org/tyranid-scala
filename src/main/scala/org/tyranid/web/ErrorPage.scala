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

import scala.xml.{ NodeSeq }

import java.io.{ BufferedInputStream, ByteArrayInputStream, BufferedOutputStream, IOException }

import org.tyranid.Imp._
import org.tyranid.json.JqHtml

object Errorlet extends Weblet {

  val stopCometStr = """[
  {
     "channel": "/meta/handshake",
     "version": "1.0",
     "minimumVersion": "0.9",
     "supportedConnectionTypes": ["long-polling","callback-polling"],
     "successful": false,
     "error": "Authentication failed",
     "advice": { "reconnect": "none" }
   }
]""".getBytes

  def stopComet( web:WebContext ) = {
    web.res.setContentType( "application/json" )

    try {
      new BufferedInputStream( new ByteArrayInputStream( stopCometStr ) ).transferTo( new BufferedOutputStream( web.res.getOutputStream() ), true )
    } catch {
      case e:IOException => ; // bury
      case e if e.getClass.getSimpleName == "EofException" =>
      case e2:Throwable => throw e2
    }

    web.res.ok
  }

  def handle( web:WebContext ) {

    rpath match {
    case "/" =>
      if ( web.xhr ) {
        if ( web.b( 'full ) )
          web.jsRes( JqHtml( "#main",
            <div class="warning" style="margin:80px 0 0 200px; width:500px;">
             <img src="/images/warning.png" style="height:120px; width:120px; float:right;"/>
             <p><strong>We're sorry!</strong></p>
             <p>There seems to be some difficulties with this page.  Our technical staff have been notified that you have encountered this issue.</p>
             <p>Please visit <a href="http://rbsupport.volerro.com/">http://rbsupport.volerro.com/</a> if this is an urgent issue.</p>
             <p>Your time is important to us, so thank you for your patience!</p>
             <p><em>- The { B.applicationName } Team</em></p>
           </div>
          ) )
      } else {
        web.forward( path = "/error/?full=1" )
      }

    case "/404" =>

      val originalUrl = web.req.getAttribute( "javax.servlet.forward.request_uri" )._s
      if ( originalUrl.isBlank )
        originalUrl = web.s( 'url ) or ""

      if ( T.session.ua( web ).isIE && ( originalUrl.startsWith( "/\"" ) || originalUrl.startsWith( "/%22/" ) ) ) {
        web.res.ok
        return
      }

      println( originalUrl )

      if ( originalUrl.startsWith( "/cometd" ) ) {
        stopComet( web )
        return
      }

      if ( originalUrl.notBlank )
        log( Event.Error404, "p" -> originalUrl )

      if ( WebFilter.notAsset( originalUrl ) ) {
        if ( web.xhr ) {
          if ( web.b( 'full ) ) {
            web.jsRes( JqHtml( "#main",
      <div class="warning" style="margin:80px 0 0 200px; width:500px;">
       <img src="/images/warning.png" style="height:120px; width:120px; float:right;"/>
       <p style="font-size:32px;"><strong>404</strong></p>
       <p>We're sorry, but the requested URL <pre>{ originalUrl }</pre> was not found on our servers.</p>
       <p>Please visit <a href={ "http://rbsupport.volerro.com/" }>{ "http://rbsupport.volerro.com/" }</a> if this is an urgent issue.</p>
       <p>Thank you.</p>
       <p><em>- The { B.applicationName } Team</em></p>
      </div> ) )
          } else {
            T.session.error(
             "<p>We're sorry, but the requested URL <pre>" + originalUrl  + "</pre> was not found on our servers.</p>" +
             "<p>Please visit <a href='http://rbsupport.volerro.com'>http://rbsupport.volerro.com</a> if this is an urgent issue.</p>" +
             "<p>Thank you.</p>" +
             "<p><em>- The " + B.applicationName + " Team</em></p>".encJson
            )
          }
        } else {
          T.web.forward( js = "V.app.load('/error/404?full=1&url=" + originalUrl.encUrl + "')" )
        }
      }
    case "/throw" =>
      throw new RuntimeException( "test exception" )

    case _ =>
      _404
    }
  }
}

