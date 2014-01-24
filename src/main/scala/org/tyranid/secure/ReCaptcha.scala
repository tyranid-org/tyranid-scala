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

package org.tyranid.secure

import java.io.IOException
import java.net.URL

import scala.collection.mutable
import scala.collection.JavaConversions._
import scala.xml.{ NodeSeq, Unparsed }

import org.tyranid.Imp._
import org.tyranid.cloud.aws.{ S3, S3Bucket }
import org.tyranid.db.{ Domain, Record, Scope }
import org.tyranid.logic.Invalid
import org.tyranid.ui.PathField


object DbReCaptcha {
  val scriptSrc = "https://www.google.com/recaptcha/api/js/recaptcha_ajax.js"
    
  def showFunction( theme:String ) = """
window.showRecaptcha = function(element) {
  Recaptcha.create( """" + B.reCaptchaPublicKey + """", element, {
  theme: """" + theme + """",
  callback: Recaptcha.focus_response_field});
}"""

  val div = <div id="recaptcha_div"></div>
    
  val callShowFunction = "showRecaptcha( $( '#recaptcha_div' ).get(0) );"
    
  def passed = { 
    val web = T.web
    
    val res = "https://www.google.com/recaptcha/api/verify".POST(
      form = Map(
        "privatekey" -> B.reCaptchaPrivateKey,
        "remoteip"   -> ( web.req.getRemoteAddr or "localhost" ),
        "challenge"  -> web.s( 'recaptcha_challenge_field ),
        "response"   -> web.s( 'recaptcha_response_field )
      ) ).s.trim
      
    //println( res )
    
    res.startsWith( "true" )
  }
}

case class DbReCaptcha( theme:String ) extends Domain {
  val sqlName = "invalid"
    
  override def show( s:Scope ) = !T.session.passedCaptcha

  override def ui( s:Scope, f:PathField ) = {
    <head>
     <script type="text/javascript" src={ DbReCaptcha.scriptSrc }></script>
     <script>{ Unparsed( DbReCaptcha.showFunction( theme ) ) }</script>
    </head> ++
    { DbReCaptcha.div } ++
    <tail>
     <script type="text/javascript">{ Unparsed( DbReCaptcha.callShowFunction ) } </script>
    </tail>
  }

  override def inputcClasses = " recaptcha"

  override val validations =
    ( ( scope:Scope ) => {
      val sess = T.session

      scope.captcha &&
      !sess.passedCaptcha &&
      scope.rec.hasSubmitted && {
        val passedCaptcha = DbReCaptcha.passed

        if ( passedCaptcha )
          sess.passedCaptcha = true

        !passedCaptcha
      } |* Some( Invalid( scope, "Invalid captcha." ) )
    } ) ::
    super.validations
}

