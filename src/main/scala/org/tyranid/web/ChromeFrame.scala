
package org.tyranid.web

import scala.xml.Unparsed

import org.tyranid.Imp._


object ChromeFrame {

  val sniffBox = Unparsed( """
<!--[if lt IE 8 ]>
<div class="error"><a href="/browser/chrome">Welcome to """ + Tyr.applicationName + """!  Please click here for a better web browsing experience with this website.</a></div>
<![endif]-->
<!--[if IE 8 ]>
<div class="warning"><a href="/browser/chrome">Welcome to """ + Tyr.applicationName + """!  Please click here for a better web browsing experience with this website.</a></div>
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
       destination: "/browser/chrome_installed"
     });
   });
 </script>
<![endif]-->""" )


}

