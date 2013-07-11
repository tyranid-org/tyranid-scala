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

import scala.xml.Unparsed

import org.tyranid.Imp._
import org.tyranid.session.Session
import org.tyranid.ui.Button

object ChromeFrame {
      
  val sniffBox_sc = Unparsed( """
<!--[if lt IE 9 ]>
<div class="chrome error"><a href="http://www.google.com/chromeframe/eula.html?user=true" target="_blank">Welcome to """ + B.applicationName + """!  Please click here for a better web browsing experience with this website.</a></div>
<![endif]-->
""" )

//<div class="chrome error"><a href="javascript:T.chromeFrame()">Welcome to """ + B.applicationName + """!  Please click here for a better web browsing experience with this website.</a></div>
  val sniffBox_rb = Unparsed( """
<!--[if lt IE 9 ]>
<div class="chrome error"><a href="http://www.google.com/chromeframe/eula.html?user=true" target="_blank">Welcome to """ + B.applicationName + """!  Please click here for a better web browsing experience with this website.</a></div>
<![endif]-->
""" )

  val installScript = Unparsed( """
<!--[if IE]>
 <script type="text/javascript" src="https://ajax.googleapis.com/ajax/libs/chrome-frame/1/CFInstall.min.js"></script>
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
          <tyr:shell>
          { ChromeFrame.installScript }
          </tyr:shell> ) )

    case "/chrome/done" =>
      ctx.res.html(
        WebTemplate(
          <tyr:shell>
           <p>Your browser is now upgraded.  Thank you!</p>
           { Button.bar(
              Button.link( "Return to " + B.applicationName + ".", "/", color = "go" ) ) }
          </tyr:shell> ) )

    case _ =>
      ctx.res.ok
    }
  }
}

