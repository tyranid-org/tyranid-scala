
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
import org.tyranid.web.{ Weblet, WebContext }


case class FbApp( apiKey:String, secret:String ) extends SoApp {

  val networkCode = "fb"
  val networkName = "Facebook"

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

  def loginButton( weblet:Weblet ) = {
    <head>
     <script>{ Unparsed( """
  window.fbAsyncInit = function() {
    FB.init({
      appId      : '""" + apiKey + """',
      channelUrl : '//""" + B.domain + """/facebook/channel',
      status     : true, // check login status
      cookie     : true, // enable cookies to allow the server to access the session
      xfbml      : true  // parse XFBML
    });

    FB.Event.subscribe('auth.login', function () {
      window.location = '""" + weblet.wpath + """/infb';
    });
  };

  // Load the SDK Asynchronously
  (function(d){
     var js, id = 'facebook-jssdk', ref = d.getElementsByTagName('script')[0];
     if (d.getElementById(id)) {return;}
     js = d.createElement('script'); js.id = id; js.async = true;
     js.src = "//connect.facebook.net/en_US/all.js";
     ref.parentNode.insertBefore(js, ref);
   }(document));
""" ) }</script>
    </head>
    <tail>
     <div id="fb-root"></div>
    </tail>
    <fb:login-button>Sign In with Facebook</fb:login-button>
  }

  def exchangeToken:Boolean = {

    val t = T

    val cookieName = "fbsr_" + apiKey
    val cookieValue = t.web.req.cookieValue( cookieName )

    if ( cookieValue == null ) {
        log( Log.Facebook, "m" -> ( "Facebook exchange missing " + cookieName + " cookie.  Cannot exchange linked in bearer token for a server token." ) )
        return false
    }

    val ( sigStr, payloadStr ) = cookieValue.splitFirst( '.' )

    val sigBytes     = Base64.toBytes( sigStr )
    val payloadBytes = Base64.toBytes( payloadStr )

    val sigDecodedStr = new String( sigBytes )
    val payloadDecodedStr = new String( payloadBytes )

    val json = payloadDecodedStr.toJson

    val uid = json.s( 'user_id )
    val sig = sigDecodedStr
    val code = json.s( 'code )
    //val issuedAt = json.s( 'issued_at )

    val params =
      "https://graph.facebook.com/oauth/access_token".POST( Map( "client_id" -> apiKey, "client_secret" -> secret, "redirect_uri" -> "", "code" -> code ) ).
      split( "&" ).map( _.splitFirst( '=' ) )

    val accessToken = params.find( _._1 == "access_token" ).get._2
    val expires     = System.currentTimeMillis + params.find( _._1 == "expires" ).get._2.toLong * 1000

    spam( "uid=" + uid )
    spam( "accessToken=" + accessToken )
    spam( "expires=" + expires )

    val session = t.session
    val user = session.user

    user( 'fbid ) = uid
    user( 'fbt )  = accessToken
    user( 'fbte ) = expires

    val existing = B.User.db.findOne( Mobj( "fbid" -> uid ) )
    if ( existing != null && user.id != null && existing.id != user.id )
      removeAttributes( existing )

    if ( !user.isNew )
      saveAttributes( user )

    true
  }


  /*
   * * *   People
   */

  def importUser( user:User, uid:String ) = {
    val profile = "https://graph.facebook.com/me".GET( Map( "access_token" -> user.s( 'fbt ) ) ).parseJsonObject
    spam( "profile:\n\n" + profile )

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

    case _ =>
      _404
    }
  }
}

