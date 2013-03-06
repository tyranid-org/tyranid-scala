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

package org.tyranid.social.basecamp

import org.apache.http.auth.AuthScope

import scala.xml.{ Unparsed, NodeSeq }

import java.net.URI

import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.db.mongo.Imp._
import org.tyranid.json.Json
import org.tyranid.http.Http
import org.tyranid.locale.{ Country, Language }
import org.tyranid.math.Base64
import org.tyranid.profile.{ Gender, User }
import org.tyranid.session.Session
import org.tyranid.social.SoApp
import org.tyranid.time.Time
import org.tyranid.ui.Form
import org.tyranid.web.{ Weblet, WebContext }

object Basecamp {
  def getProjects( user:DBObject ) = {
    val bcUsername = user.s( 'bcUsername )
    val bcPassword = user.s( 'bcPassword )
    val bcAccount = user.s( 'bcAccount )
    
    if ( bcUsername.isBlank || bcPassword.isBlank || bcAccount.isBlank ) {
      null
    } else {
      //1901937
      val res = Http.GET( 
                  "https://basecamp.com/" + bcAccount + "/api/v1/projects.json",
                  headers = Map( "User-Agent" -> "TestApp (mrkcbradley@gmail.com)" ),
                  authScope = new AuthScope( "basecamp.com", AuthScope.ANY_PORT ), 
                  username = bcUsername, password = bcPassword )
                  
      val entity = res.response.getEntity
    
      if ( entity != null ) {
        if ( "application/json" == entity.getContentType.getValue ) {
          val json = Json.parse( res._s )
          println( json.toJsonStr( true ) )
        }
      }
    }
      
    null
  }  
}

case class BcApp( apiKey:String, secret:String ) extends SoApp {

  val networkCode = "bc"
  val networkName = "Basecamp"

  val logo = "/images/basecamp_logo.png"

  def copyAttributes( from:User, to:User ) = {
    to( 'bcid ) = from.s( 'bcid )
    to( 'bct )  = from.s( 'bct )
    to( 'bcte ) = from.l( 'bcte )
  }

  def saveAttributes( user:User ) {
    B.User.db.update(
      Mobj( "_id" -> user.id ),
      Mobj( $set -> Mobj(
        "bcid" -> user.s( 'bcid ),
        "bct"  -> user.s( 'bct ),
        "bcte" -> user.l( 'bcte ) )
      )
    )
  }

  def removeAttributes( user:DBObject ) {
    user.remove( 'bcid )
    user.remove( 'bct )
    user.remove( 'bcte )
    B.User.db.update( Mobj( "_id" -> user.id ), Mobj( $unset -> Mobj( "bcid" -> 1, "bct" -> 1, "bcte" -> 1 ) ) )
  }

  private lazy val loadFacebookApi = """
  // Load the SDK Asynchronously
  (function(d){
     var js, id = 'facebook-jssdk', ref = d.getElementsByTagName('script')[0];
     if (d.getElementById(id)) {return;}
     js = d.createElement('script'); js.id = id; js.async = true;
     js.src = "//connect.facebook.net/en_US/all.js";
     ref.parentNode.insertBefore(js, ref);
   }(document));
"""

  private lazy val facebookInit = """
    FB.init({
      appId      : '""" + apiKey + """',
      channelUrl : '//""" + B.domain + """/facebook/channel',
      status     : true, // check login status
      cookie     : true, // enable cookies to allow the server to access the session
      xfbml      : true  // parse XFBML
    });
"""

  def loginButton( weblet:Weblet ) = {
    val loggingOut = T.web.req.s( 'lo ).notBlank
    
    if ( false && !B.PRODUCTION )
     <a href={ "https://launchpad.37signals.com/authorization/new?type=web_server&client_id=" + apiKey + "&redirect_uri=" + T.website + "/basecamp" }>Basecamp Login</a>
    else 
      NodeSeq.Empty
  }

  def logoutScript = {
    <top>
     <div id="bc-root"></div>
     <script>{ Unparsed( """
  window.fbAsyncInit = function() {
    """ + facebookInit + """

    FB.getLoginStatus( function( resp ) {
      if ( resp.status == 'connected' )
        FB.logout();
    });
  };

""" + loadFacebookApi ) }</script>
    </top>
  }

  def removeCookies {}

  def linkButton = {
    if ( !B.PRODUCTION )
      <a href={ "https://launchpad.37signals.com/authorization/new?type=web_server&client_id=" + apiKey + "&redirect_uri=" + T.website + "/basecamp/link" }>Authenticate Basecamp</a>
    else 
      NodeSeq.Empty
  }

  def linkPreview( user:User ) = {
    val uid = user.s( 'fbid )
    val profile = "https://graph.facebook.com/me".GET( Map( "access_token" -> user.s( 'fbt ) ) ).s.parseJsonObject

    Form.text( "First Name", profile.s( 'first_name ) ) ++
    Form.text( "Last Name", profile.s( 'last_name ) ) ++
    Form.thumbnail( "Profile Image", "https://graph.facebook.com/" + uid + "/picture?type=square", href = "/facebook/useThumbnail", hrefLabel = "Use Picture" )
  }


  private val cookieName = "bc_" + apiKey

  def isActive = T.web.req.cookieValue( cookieName ).notBlank

  def exchangeToken:Boolean = {
    val code = T.web.s( 'code )
    val responseStr =
     "https://launchpad.37signals.com/authorization/token".POST( Map( "type" -> "web_server", "client_id" -> apiKey, "client_secret" -> secret, "redirect_uri" -> ( T.website + "/basecamp" ), "code" -> code ) ).s
     
    if ( responseStr.startsWith( "error" ) ) {
      println( responseStr )
      false
    } else {
      val responseJson = responseStr.parseJsonObject
      val u = T.user
      
      u( 'bct ) = responseJson.s( 'access_token )
      u( 'bcte ) = System.currentTimeMillis + responseJson.l( 'expires_in ) * 1000
      u( 'bcid ) = responseJson.s( 'refresh_token )
//    https://launchpad.37signals.com/authorization.json
      true
    }
  }

  /*
   * * *   People
   */

  def importUser( user:User, uid:String ) = {
    val profile = "https://graph.facebook.com/me".GET( Map( "access_token" -> user.s( 'fbt ) ) ).s.parseJsonObject

    user( 'firstName ) = profile.s( 'first_name )
    user( 'lastName )  = profile.s( 'last_name )

    val gender = Gender.by( profile.s( 'gender ) )
    
    if ( gender != null )
      user( 'gender ) = gender.id

    if ( profile.s( 'timezone ).notBlank )
      user( 'tzOff ) = profile.i( 'timezone )

    if ( profile.s( 'locale ).notBlank ) {
      val ( language, country ) = profile.s( 'locale ).splitFirst( '_' )

      user( 'lang )    = Language.idByIso630_1( language )
      user( 'country ) = Country.idByIso3166_2( country )
    }

    user( 'thumbnail ) = "https://graph.facebook.com/" + uid + "/picture?type=square"
  }
}

object Basecamplet extends Weblet {

  def handle( web:WebContext ) {
    val s = Session()
    val u = s.user

    println( "*** BASECAMP ****" )
    web.req.dump
    println( "*** BASECAMP ****" )
    
    val accessCode = web.b( 'code ) // 1058314c
    
    if ( accessCode ) {
      B.basecamp.exchangeToken
    }
    
    rpath match {
    case "/link" =>
      T.web.redirect( "/user/edit" )
      
    case "/channel" =>
      web.res.html(
        <script src="//connect.facebook.net/en_US/all.js"/>,
        headers = Http.expireCacheControlHeaders( ageMs = Time.OneYearMs )
      )

    case "/exchange" =>
      B.facebook.exchangeToken
      web.res.ok

    case "/useThumbnail" =>
      u( 'thumbnail ) = "https://graph.facebook.com/" + u.s( 'fbid ) + "/picture?type=square"
      B.User.db.update( Mobj( "_id" -> u.id ), Mobj( $set -> Mobj( "thumbnail" -> u.s( 'thumbnail ) ) ) )
      s.notice( "Your " + B.applicationName + " profile image has been set to your Facebook profile image." )
      T.web.redirect( "/user/edit" )

    case _ => _404
    }
  }
}
