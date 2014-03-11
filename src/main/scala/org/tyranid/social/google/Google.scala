/**
 * Copyright (c) 2008-2014 Tyranid <http://tyranid.org>
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

package org.tyranid.social.google

import scala.xml.Unparsed

import com.mongodb.DBObject

import com.google.api.client.googleapis.auth.oauth2.{ GoogleAuthorizationCodeTokenRequest, GoogleCredential }
import com.google.api.client.http.javanet.NetHttpTransport
import com.google.api.client.json.jackson.JacksonFactory
import com.google.api.client.auth.oauth2.TokenResponseException
import com.google.api.services.oauth2.Oauth2

import org.tyranid.Imp._
import org.tyranid.db.mongo.Imp._
import org.tyranid.http.Http
import org.tyranid.locale.{ Country, LatLong }
import org.tyranid.math.Base64
import org.tyranid.session.Session
import org.tyranid.web.WebContext


case class GoApp( simpleKey:String, clientId:String, secret:String, signin:Boolean ) { // extends SoApp {

  val networkCode = "go"
  val networkName = "Google+"

  //val logo = "/images/facebook_logo.png"

/*
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
*/

// Another free lookup API 
// http://nominatim.openstreetmap.org/search?q=135+pilkington+avenue,+birmingham&addressdetails=1&format=json
    
  def geocode( address:String ):LatLong = {

    val str = "https://maps.googleapis.com/maps/api/geocode/json".GET( Map( "address" -> address, "sensor" -> "false" ) ).s
    val obj = str.parseJsonObject

    if ( obj.s( 'status ) == "OK" ) {
      val rslts = obj.a_?( 'results )
      if ( rslts.size > 0 ) {
        val loc = rslts( 0 ).as[ObjectMap].o_?( 'geometry ).o_?( 'location )
        return LatLong( lat = loc.d( 'lat ), long = loc.d( 'lng ) )
      }
    }

    log( Event.Google, "m" -> ( "geocode problem:\n\nAddress:" + address + "\n\nResponse:\n\n" + str ) )
    null
  }

  def createCrossSiteAntiForgeryToken = {
    new java.math.BigInteger( 130, new java.security.SecureRandom ).toString( 32 )

    // Read index.html into memory, and set the Client ID,
    // Token State, and Application Name in the HTML before serving it.

    //return new Scanner( new File( "index.html" ), "UTF-8" )
        //.useDelimiter( "\\A" ).next
        //.replaceAll( "[{]{2}\\s*CLIENT_ID\\s*[}]{2}", CLIENT_ID )
        //.replaceAll( "[{]{2}\\s*STATE\\s*[}]{2}", state )
        //.replaceAll( "[{]{2}\\s*APPLICATION_NAME\\s*[}]{2}", APPLICATION_NAME )
  }

  def signinCode = """
(function () {
  var po = document.createElement('script');
  po.type = 'text/javascript';
  po.async = true;
  po.src = 'https://plus.google.com/js/client:plusone.js?onload=start';
  var s = document.getElementsByTagName('script')[0];
  s.parentNode.insertBefore(po, s);
})();
"""

  def login {
    val t = T
    val web = t.web
    val sess = t.session

    val csrf = web.s( 'csrf )
    if ( csrf != t.crossSiteRequestForgeryToken ) {
      sess.error( "Invalid state parameter." )
      return web.jsRes()
    }

    //val gPlusId = web.s( 'gplus_id )
    val code = web.s( 'code )
    //String code = request.body();


    val response = web.res

    try {
      val jsonFactory = new JacksonFactory
      val httpTransport = new NetHttpTransport

      // Upgrade the authorization code into an access and refresh token.
      val tokenResponse = new GoogleAuthorizationCodeTokenRequest( httpTransport, jsonFactory, B.google.clientId, B.google.secret, code, "postmessage" ).execute

      // Create a credential representation of the token data.
      val credential = new GoogleCredential.Builder()
        //.setApplicationName( B.applicationName )
        .setJsonFactory( jsonFactory )
        .setTransport( httpTransport )
        .setClientSecrets( B.google.clientId, B.google.secret ).build
        .setFromTokenResponse( tokenResponse )

      // Check that the token is valid.
      val oauth2 = new Oauth2.Builder( httpTransport, jsonFactory, credential ).build
      val tokenInfo = oauth2.tokeninfo.setAccessToken( credential.getAccessToken ).execute
      //sp am( "access token=" + credential.getAccessToken )

      if ( tokenInfo.containsKey( "error" ) ) {
        sess.error( tokenInfo.get( "error" ).toString )
        return web.jsRes()
      }

      // Make sure the token we got is for the intended user.
      //if ( !tokenInfo.getUserId.equals( gPlusId ) ) {
        //response.setStatus( 401 )
        //sess.error( "Token's user ID doesn't match given user ID." )
        //return web.jsRes()
      //}

      // Make sure the token we got is for our app.
      if ( !tokenInfo.getIssuedTo.equals( B.google.clientId ) ) {
        sess.error( "Token's client ID does not match app's." )
        return web.jsRes()
      }

      val guid = tokenInfo.getUserId
      val email = tokenInfo.getEmail

      //sp am( "plusId: " + guid )
      //sp am( "email: " + email )
      //sp am( "token 1: " + credential.getAccessToken )
      //sp am( "token 2: " + tokenResponse.getClass.getName )
      //sp am( "token 2: " + tokenResponse.toString )

      val tr = tokenResponse.toString.parseJsonObject

      val accessToken = tr.s( 'access_token )
      //sp am( "access_token 2: " + accessToken )
      val expiresIn = tr.i( 'expires_in )
      val idToken = tr.s( 'id_token )
      val refreshToken = tr.s( 'refresh_token )
      val tokenType = tr.s( 'token_type )

      var user:org.tyranid.profile.User = B.User( B.User.db.findOne( Mobj( "goid" -> guid ) ) )
      if ( user == null ) {
        var isNew = false

        if ( email.notBlank ) {
          user = B.User( B.User.db.findOne( Mobj( "email" -> email ) ) )

          if ( user != null ) {
            val pw = web.s( 'pw )

            if ( pw.isBlank ) {
              sess.error( "This user already exists.  Please enter in your Volerro password to link this account to your Google+ account." )
              return web.jsRes()
            }

            if ( !pw.checkShash( user.s( 'password ) ) ) {
              sess.error( "Invalid password." )
              return web.jsRes()
            }
          }
        }

        if ( user == null ) {
          user = B.newUser()
          isNew = true
        }

        val gUser = ( "https://www.googleapis.com/plus/v1/people/" + guid + "?access_token=" + accessToken ).GET().s.parseJsonObject
        //sp am( gUser )

        user( 'email )     = email
        user( 'goid )      = guid
        user( 'got )       = accessToken
        user( 'gote )      = expiresIn
        user( 'goft )      = refreshToken
        user( 'firstName ) = gUser.o_?( 'name ).s( 'givenName )
        user( 'lastName )  = gUser.o_?( 'name ).s( 'familyName )

        sess.user = user

        /*
         
        Map(
          kind -> plus#person,
          etag -> "cNO8fvS5xtpwXS2Hju_Q9HKhleA/fNvrEURk7OQoZxxFEXj8iDEZzA4",
          occupation -> Software Engineer,
          gender -> male,
          objectType -> person,
          id -> 110299965513205554278,
          displayName -> Ted Halmrast,
          url -> https://plus.google.com/110299965513205554278,
          image -> Map(url -> https://lh5.googleusercontent.com/-UMYfS4W0pTk/AAAAAAAAAAI/AAAAAAAAAJI/JPh9gIQdDyM/photo.jpg?sz=50),
          organizations -> [Ljava.lang.Object;@60acd4a0,
          isPlusUser -> true,
          language -> en,
          ageRange -> Map(min -> 21),
          circledByCount -> 0,
          verified -> false
        )

        */

        user.save

        if ( isNew ) {
          val orgs = gUser.a_?( 'organizations )
          //sp am( "orgs.length: " + orgs.length )
          //for ( o <- orgs )
            //sp am( "org: " + o )

          val orgName =
            if ( orgs.length > 0 )
              orgs( 0 ).as[ObjectMap].s( 'name )
            else
              null

          //sp am( "user.tid=" + user.tid )

          if ( orgName.notBlank )
            B.registerUser( user, orgName )

          B.welcomeUserEvent
        }
      }

      sess.login( user, setAuth = true )

      // Store the token in the session for later use.
      //request.session().attribute("token", tokenResponse.toString )
      //return GSON.toJson( "Successfully connected user." )
    } catch {
    case e:TokenResponseException =>
      response.setStatus( 500 )
      sess.error( "Failed to upgrade the authorization code." )
      return web.jsRes()
    case e:java.io.IOException =>
      response.setStatus( 500 )
      sess.error( "Failed to read token data from Google. " + e.getMessage )
      return web.jsRes()
    }

    web.jsRes()

/*

      val companyName = web.s( 'company ).trim

      if ( user.oid( 'org ) == null && companyName.notBlank ) {
        val exists = B.Org.db.exists( Mobj( "name" -> companyName.encRegex.toPatternI ) )

        if ( exists ) {
          sess.error( "Company name is already in use." )
          return web.jsRes()
        }
      }

      val firstName = web.s( 'firstName )

      user( 'email ) = email
      user( 'firstName ) = firstName
      user( 'lastName ) = web.s( 'lastName )
      user( 'password ) = password.shash()
      user( 'createdOn ) = new Date

      if ( user.s( 'activationCode ).isBlank ) {
        sendActivation( user )
        B.registerUser( user, companyName )
        EmailCookie.set( email )
        sess.logout( true )
        return web.jsRes( JsModel( user.toClientCommonMap(), "common" ), JsModel( Map( "email" -> email, "firstName" -> firstName ) ) )
      }

      sess.login( user, setAuth = true )
      user.remove( 'activationCode )

      B.registerUser( user, companyName )
      B.welcomeUserEvent

      return web.jsRes( JsData( user ), JsModel( user.toClientCommonMap( true ), "common" ), JsModel( Map( "dashboard" -> true ) ) )
    }

    web.forward( js = "mainLoad( function() { router.navigate( '#register', { trigger: true } ); } );" )
*/
  }

}

