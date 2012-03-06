
package org.tyranid.social.facebook

import scala.xml.Unparsed

import org.tyranid.Imp._
import org.tyranid.http.Http
import org.tyranid.session.Session
import org.tyranid.time.Time
import org.tyranid.web.{ Weblet, WebContext }


object Facebook {

}

case class FbApp( apiKey:String, secret:String ) {

  def apiScript( weblet:Weblet ) = {
    <div id="fb-root"></div>
    <script>{ Unparsed( """
  window.fbAsyncInit = function() {
    FB.init({
      appId      : '""" + apiKey + """',
      channelUrl : '//""" + B.domain + """/facebook/channel',
      status     : true, // check login status
      cookie     : true, // enable cookies to allow the server to access the session
      xfbml      : true  // parse XFBML
    });

    // Additional initialization code here
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
  }

  def loginButton = <fb:login-button/>
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

