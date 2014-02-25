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

package org.tyranid.social

import scala.xml.{ NodeSeq, Unparsed }

import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.db.mongo.Imp._
import org.tyranid.oauth.{ OAuth, Token }
import org.tyranid.profile.User
import org.tyranid.web.Weblet



object Twitter {

  /*
    +. twit feed

          twitterStories

            


       +. read twitter feed

       +. filter feed by tweets with links

       +. analyze url to get image
   */

   def splitUrl( tweet:String ):(String,String) = {
    val urlStart = tweet.indexOf( "http" )

    if ( urlStart != -1 ) {
      var urlEnd = tweet.indexOf( " ", urlStart )
      if ( urlEnd == -1 ) urlEnd = tweet.length

      ( tweet.substring( urlStart, urlEnd ),
        tweet.substring( 0, urlStart ) + ( urlEnd < tweet.length |* tweet.substring( urlEnd + 1 ) ) )
    } else {
      ( null, tweet )
    }
  }
}


case class TwApp( apiKey:String, secret:String, systemToken:Token = null ) extends SoApp {
  val networkCode = "tw"
  val networkName = "Twitter"
  val logo = "/images/linkedin_logo.png"

  lazy val oauth = OAuth( key = apiKey, secret = secret )

  def copyAttributes( from:User, to:User ) = {
    to( 'twid ) = from.s( 'twid )
    to( 'twt )  = from.s( 'twt )
    to( 'twts ) = from.s( 'twts )
  }

  def saveAttributes( user:User ) {
    B.User.db.update(
      Mobj( "_id" -> user.id ),
      Mobj( $set -> Mobj(
        "twid" -> user.s( 'twid ),
        "twt"  -> user.s( 'twt ),
        "twts" -> user.s( 'twts ) )
      )
    )
  }

  def removeAttributes( user:DBObject ) {
    user.remove( 'twid )
    user.remove( 'twt )
    user.remove( 'twts )
    B.User.db.update( Mobj( "_id" -> user.id ), Mobj( $unset -> Mobj( "twid" -> 1, "twt" -> 1, "twts" -> 1 ) ) )
  }

  def loginButton( weblet:Weblet ) = {
    val loggingOut = T.web.req.s( 'lo ).notBlank
    
    <head>
     <script src={ "https://platform.twitter.com/anywhere.js?id=" + apiKey + "&v=1" } type="text/javascript"></script>
    </head>
    <span id="twLogin"></span>
    <script type="text/javascript">{ Unparsed("""

""" + ( loggingOut |* "window.twLogOut = true;" ) + """

twttr.anywhere( function(T) {
  T("#twLogin").connectButton({
    size: 'medium',
    authComplete: function(user) {
      console.log(user);
      if ( !window.twLogOut )
        window.location.assign( '""" + weblet.wpath + """/intw' );
      else
        delete window.twLogOut
    },
    signOut: function() {
    }
  });

""" + ( loggingOut |* """
  if ( T.isConnected() ) {
    twttr.anywhere.signOut();
  }
  setTimeout( "delete window.twLogOut;", 1200 );
""" ) + """
});
""") }</script>
   }

  def removeCookies = {
    /*
    // TODO
    val web = T.web
    for ( c <- web.req.cookies;
          if c.getName.startsWith( "linkedin_oauth" ) )
      web.res.deleteCookie( c.getName )
     */
  }

  def logoutScript = {

    <head>
     <script src={ "https://platform.twitter.com/anywhere.js?id=" + apiKey + "&v=1" } type="text/javascript"></script>
    </head>
    <script type="text/javascript">{ Unparsed("""
      twttr.anywhere.signOut();
    """ ) }</script>
  }

  def linkButton = {
    <head>
     <script src={ "https://platform.twitter.com/anywhere.js?id=" + apiKey + "&v=1" } type="text/javascript"></script>
    </head>
     /*

     // TODO
     
     <script>{ Unparsed( """
       function onLinkedInAuth() {
         $.post('/linkedin/exchange', function(data) {
           window.location.reload( true );
         });
       }
     """ ) }</script>
    </head>
    <script type="IN/Login" data-onAuth="onLinkedInAuth"></script>
    */
  }

  def linkPreview( user:User ) = {
    /*

      TODO

    val uid = user.s( 'liid )
    val profile = B.linkedIn.GET( "/people/id=" + uid + ":(id,first-name,last-name,picture-url)", user ).parseJsonObject

    { Form.text( "First Name", profile.s( 'firstName ) ) } ++
    { Form.text( "Last Name", profile.s( 'lastName ) ) } ++
    { profile.contains( 'pictureUrl ) |*
      Form.thumbnail( "Profile Image", profile.s( 'pictureUrl ), href = "/linkedin/useThumbnail", hrefLabel = "Use Picture" ) }
     */
    NodeSeq.Empty
  }


  private val cookieName = "linkedin_oauth_" + apiKey

  def isActive = {
    val cv = T.web.req.cookieValue( cookieName )
    cv.notBlank && cv != "null"
  }

  def exchangeToken:Boolean = {
    T.web.req.dump
/*
    val cookie =
      T.web.req.cookie( cookieName ).getOrElse {
        log( Event.LinkedIn, "m" -> ( "Linkedin exchange missing " + cookieName + " cookie.  Cannot exchange linked in bearer token for a server token." ) )
        return false
      }

    val json = cookie.getValue.decUrl.parseJsonObject

    val uid         = json.s( 'member_id )
    val accessToken = json.s( 'access_token )
    val signature   = json.s( 'signature )

    val text = new StringBuilder
    for ( fieldName <- json( 'signature_order ).as[Array[String]] )
      text ++= json( fieldName ).toString

    val calcSignature = OAuth.hmacSha1( text.toString, secret )

    if ( calcSignature != signature )
      throw new RuntimeException( "Failed signature match." )

    val exchangeUrl = "https://api.linkedin.com/uas/oauth/accessToken"

    val params = mutable.Map(
      "oauth_consumer_key"         -> oauth.key,
      "xoauth_oauth2_access_token" -> accessToken,
      "oauth_signature_method"     -> OAuth.signatureMethod )

    oauth.sign( "POST", exchangeUrl, params )

    val str = exchangeUrl.POST( content = OAuth.encParams( params ), contentType = "application/x-www-form-urlencoded" )

    val session = Session()
    val user = session.user

    user( 'liid ) = uid
    for ( rslt <- str.splitAmp;
          ( key, value ) = rslt.splitFirst( '=' ) ) {
      key match {
      case "oauth_token"                    => user( 'lit )  = value
      case "oauth_token_secret"             => user( 'lits ) = value
      case "oauth_expires_in"               => // should be 0
      case "oauth_authorization_expires_in" => // should be 0
      case _ =>
      }
    }

    exchangeAttributes( user )
  
 */ 
    true
  }

  def tokenFor( user:User ) = Token( key = user.s( 'lit ), secret = user.s( 'lits ) )

  def GET( url:String, user:User ) = oauth.GET( "https://api.linkedin.com/v1" + url, tokenFor( user ), headers = Map( "x-li-format" -> "json" ) )


  /*
   * * *   People
   */

  def importUser( user:User, uid:String ) = {
    /*
    TODO
    val profile = GET( "/people/id=" + uid + ":(id,first-name,last-name,picture-url,headline,location:(country:(code)))", user ).parseJsonObject

    user( 'firstName ) = profile( 'firstName )
    user( 'lastName )  = profile( 'lastName )
    user( 'thumbnail ) =
      if ( profile.contains( 'pictureUrl ) )
        profile( 'pictureUrl )
    user( 'title )     = profile( 'headline )

    val country = profile.o( 'location ).o( 'country ).s( 'code )
    if ( country.notBlank )
      user( 'country ) = Country.idByIso3166_2( country )
    */
  }

}


