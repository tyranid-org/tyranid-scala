package org.tyranid.sso

/**
 * Copyright (c) 2008-2012 Tyranid <http://tyranid.org>
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

import java.net.URLEncoder
import java.util.Date

import org.apache.http.{ HttpRequest, HttpHost }
import org.apache.http.auth.{ AuthState, AuthScope, AuthScheme }
import org.apache.http.HttpRequestInterceptor
import org.apache.http.client.CredentialsProvider
import org.apache.http.client.protocol.ClientContext
import org.apache.http.protocol.{ ExecutionContext, HttpContext }

import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity }
import org.tyranid.http.Http
import org.tyranid.Imp._
import org.tyranid.json.Json
import org.tyranid.profile.User
import org.tyranid.web.{ Weblet, WebContext, WebTemplate }

object Ssolet extends Weblet {
  // REST username: 6b46e886-434d-478a-b6ae-de5faece0ea6
  // REST password: vuW1jQKFS
  lazy val SAAS_ID = URLEncoder.encode( B.saasId, "UTF-8" )
  lazy val TOKEN_URL = URLEncoder.encode( T.website + "/sso/token", "UTF-8" ) 
  lazy val ERROR_URL = URLEncoder.encode( T.website + "/sso/error", "UTF-8" ) 

  // PingOne Documentation for this
  // https://connect.pingidentity.com/web-portal/appintegration?x=tyGMaRMgiMSYAHNoa21b84ce4ZKmtJ88
  
  def handle(web: WebContext) {
    val sess = T.session
    
    rpath match {
    case "/" =>
      val idpId = URLEncoder.encode( getIdpIdFromRequest( web ), "UTF-8" ) 
      
      println( "https://sso.connect.pingidentity.com/sso/sp/initsso?saasid=" + SAAS_ID + "&idpid=" + idpId + "&appurl=" + TOKEN_URL + "&appurl=" + ERROR_URL )
      web.res.sendRedirect( "https://sso.connect.pingidentity.com/sso/sp/initsso?saasid=" + SAAS_ID + "&idpid=" + idpId + "&appurl=" + TOKEN_URL + "&appurl=" + ERROR_URL )
    case "/token" =>
      val token = web.s( 'tokenid )
      
      println( "Token: " + token )
      val str = Http.GET( "https://sso.connect.pingidentity.com/sso/TXS/2.0/1/" + token, authScope = AuthScope.ANY, username = "6b46e886-434d-478a-b6ae-de5faece0ea6", password = "vuW1jQKFS", interceptor = new PreemptiveAuthInterceptor() ).s
      println( str )
      
      val json = Json.parse( str ) // .get(0)
      
      println( json.s( "pingone.subject" ) )
      //statusJson.s( 'status )

      //https://sso.connect.pingidentity.com/sso/TXS/2.0/<format>/<tokenid>
      //redirectIfNotLoggedIn( web )
      //web.forward( "/sms/verify?id=" + web.s( "id" ) or "" )
    case "/error" =>
      
    }  
  }
  
  // TODO:  This needs to be a URL provided by the IdP 
  def getIdpIdFromRequest( web:WebContext ): String = {
     return "testidp.connect.pingidentity.com";           
  }
}

class PreemptiveAuthInterceptor extends HttpRequestInterceptor {
  def process( request:HttpRequest, context:HttpContext) {//} throws HttpException, IOException {
    val authState = context.getAttribute(ClientContext.TARGET_AUTH_STATE).as[AuthState]

    // If no auth scheme avaialble yet, try to initialize it preemptively
    if ( authState.getAuthScheme() == null ) {
      val authScheme = context.getAttribute( "preemptive-auth" ).as[AuthScheme]
      val credsProvider = context.getAttribute( ClientContext.CREDS_PROVIDER ).as[CredentialsProvider]
      val targetHost = context.getAttribute( ExecutionContext.HTTP_TARGET_HOST ).as[HttpHost]
     
      if ( authScheme != null ) {
        val creds = credsProvider.getCredentials( new AuthScope( targetHost.getHostName(), targetHost.getPort() ) )
    
        if ( creds != null ) { 
          println ( "creds set!" )
          authState.update( authScheme, creds )
          //authState.setAuthScheme(authScheme)
          //authState.setCredentials(creds)
        }
      }
    }
  }
}


