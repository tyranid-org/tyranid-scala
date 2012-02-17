
package org.tyranid.web

import scala.xml.Unparsed

import org.tyranid.Imp._
import org.tyranid.session.Session
import org.tyranid.ui.Button


object ChromeFrame {

  val sniffBox = Unparsed( """
<!--[if lt IE 8 ]>
<div class="error"><a href="/chrome/install">Welcome to """ + B.applicationName + """!  Please click here for a better web browsing experience with this website.</a></div>
<![endif]-->
<!--[if IE 8 ]>
<div class="warning"><a href="/chrome/install">Welcome to """ + B.applicationName + """!  Please click here for a better web browsing experience with this website.</a></div>
<![endif]-->
""" )

  val installScript = Unparsed( """
<!--[if IE]>
 <script type="text/javascript" src="http://ajax.googleapis.com/ajax/libs/chrome-frame/1/CFInstall.min.js"></script>
 <style>
  .chromeFrameInstallDefaultStyle {
     width: 800px;
     border: 5px solid blue;
  }
 </style>
 <script>
   // The conditional ensures that this code will only execute in IE,
   // Therefore we can use the IE-specific attachEvent without worry
   window.attachEvent("onload", function() {
     CFInstall.check({
       mode: "inline", // the default
       node: "prompt",
       destination: "/chrome/done"
     });
   });
 </script>
<![endif]-->""" )

}


object ChromeFramelet extends Weblet {

  def handle( ctx:WebContext ) {
    val s = Session()
    val u = s.user

    ctx.path match {
    case "/chrome/install" =>
      ctx.res.html(
        WebTemplate(
          <tyr:chromeShell>
          { ChromeFrame.installScript }
          </tyr:chromeShell> ) )

    case "/chrome/done" =>
      ctx.res.html(
        WebTemplate(
          <tyr:chromeShell>
           <p>Your browser is now upgraded.  Thank you!</p>
           { Button.bar(
              Button.link( "Return to " + B.applicationName + ".", "/", color = "green" ) ) }
          </tyr:chromeShell> ) )

    case _ =>
      ctx.res.ok
    }
  }
}
