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

package org.tyranid.social.facebook

import scala.xml.Unparsed

import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.db.mongo.Imp._
import org.tyranid.http.Http
import org.tyranid.locale.{ Country, Language }
import org.tyranid.math.Base64
import org.tyranid.profile.{ Gender, User }
import org.tyranid.session.Session
import org.tyranid.social.SoApp
import org.tyranid.time.Time
import org.tyranid.ui.Form
import org.tyranid.web.{ Weblet, WebContext }


case class FbApp( apiKey:String, secret:String ) extends SoApp {

  val networkCode = "fb"
  val networkName = "Facebook"

  val logo = "/images/facebook_logo.png"

  def copyAttributes( from:User, to:User ) = {
    to( 'fbid ) = from.s( 'fbid )
    to( 'fbt )  = from.s( 'fbt )
    to( 'fbte ) = from.l( 'fbte )
  }

  def saveAttributes( user:User ) {
    B.User.db.update(
      Mobj( "_id" -> user.id ),
      Mobj( $set -> Mobj(
        "fbid" -> user.s( 'fbid ),
        "fbt"  -> user.s( 'fbt ),
        "fbte" -> user.l( 'fbte ) )
      )
    )
  }

  def removeAttributes( user:DBObject ) {
    user.remove( 'fbid )
    user.remove( 'fbt )
    user.remove( 'fbte )
    B.User.db.update( Mobj( "_id" -> user.id ), Mobj( $unset -> Mobj( "fbid" -> 1, "fbt" -> 1, "fbte" -> 1 ) ) )
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

    <top>
     <div id="fb-root"></div>
     <script>{ Unparsed( """
""" + ( loggingOut |* "window.fbLogOut = true;" ) + """

  window.fbAsyncInit = function() {
    """ + facebookInit + """

    FB.Event.subscribe('auth.login', function () {
      window.location = '""" + weblet.wpath + """/infb';
    });

    FB.getLoginStatus( function( resp ) {
      if ( resp.status == 'connected' ) {
        if ( !window.fbLogOut ) {
          window.location = '""" + weblet.wpath + """/infb';
        } else {
          FB.logout();
          delete window.fbLogOut;
        }
      }
    });
  };

""" + loadFacebookApi ) }</script>
    </top>
    <fb:login-button>Sign In with Facebook</fb:login-button>
  }

  def logoutScript = {
    <top>
     <div id="fb-root"></div>
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
    <top>
     <div id="fb-root"></div>
     <script>{ Unparsed( """
  window.fbAsyncInit = function() {
    """ + facebookInit + """

    function exchange() {
      $.post('/facebook/exchange', function(data) {
        window.location.reload( true );
      });
    }

    FB.Event.subscribe('auth.login', function() {
      exchange();
    });

    FB.getLoginStatus( function( resp ) {
      if ( resp.status == 'connected' )
        exchange();
    });
  };

""" + loadFacebookApi ) }</script>
    </top>
    <fb:login-button>Sign In with Facebook</fb:login-button>
  }

  def linkPreview( user:User ) = {
    val uid = user.s( 'fbid )
    val profile = "https://graph.facebook.com/me".GET( Map( "access_token" -> user.s( 'fbt ) ) ).s.parseJsonObject

    Form.text( "First Name", profile.s( 'first_name ) ) ++
    Form.text( "Last Name", profile.s( 'last_name ) ) ++
    Form.thumbnail( "Profile Image", "https://graph.facebook.com/" + uid + "/picture?type=square", href = "/facebook/useThumbnail", hrefLabel = "Use Picture" )
  }


  private val cookieName = "fbsr_" + apiKey

  def isActive = T.web.req.cookieValue( cookieName ).notBlank

  def exchangeToken:Boolean = {

    val t = T


    // 1.  extract client-side code from the javascript api's fbsr_ cookie

    val cookieValue = t.web.req.cookieValue( cookieName )

    if ( cookieValue == null ) {
        log( Event.Facebook, "m" -> ( "Facebook exchange missing " + cookieName + " cookie.  Cannot exchange linked in bearer token for a server token." ) )
        return false
    }

    val ( sigStr, payloadStr ) = cookieValue.splitFirst( '.' )

    val sigBytes     = Base64.toBytes( sigStr )
    val payloadBytes = Base64.toBytes( payloadStr )

    val sigDecodedStr = new String( sigBytes )
    val payloadDecodedStr = new String( payloadBytes )

    val json = payloadDecodedStr.parseJsonObject

    val uid = json.s( 'user_id )
    val sig = sigDecodedStr
    val code = json.s( 'code )
    //val issuedAt = json.s( 'issued_at )


    // 2.  exchange client-side code for a short-lived server-side token

    val shortLivedAccessToken = 
      "https://graph.facebook.com/oauth/access_token".POST( Map( "client_id" -> apiKey, "client_secret" -> secret, "redirect_uri" -> "", "code" -> code ) ).s.
      split( "&" ).map( _.splitFirst( '=' ) ).
      find( _._1 == "access_token" ).get._2


    // 3.  exchange short-lived server-side token for a long-term server-side access token

    val params =
      "https://graph.facebook.com/oauth/access_token".POST( Map( "client_id" -> apiKey, "client_secret" -> secret, "grant_type" -> "fb_exchange_token", "fb_exchange_token" -> shortLivedAccessToken ) ).s.
      split( "&" ).map( _.splitFirst( '=' ) )

    params.find( _._1 == "access_token" ).map( _._2 ) match {
    case Some( accessToken ) =>
      val expires   = System.currentTimeMillis + params.find( _._1 == "expires" ).get._2.toLong * 1000

      val session   = t.session
      val user      = session.user

      user( 'fbid ) = uid
      user( 'fbt )  = accessToken
      user( 'fbte ) = expires

      exchangeAttributes( user )
      true

    case _ =>
      false
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
    // "/icon_individual.png"
  }
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

