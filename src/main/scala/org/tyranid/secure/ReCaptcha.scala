/**
 * Copyright (c) 2008-2011 Tyranid <http://tyranid.org>
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
import org.tyranid.ui.Field


case class DbReCaptcha( theme:String ) extends Domain {
  val sqlName = "invalid"
    
  override def show( s:Scope ) = !T.session.passedCaptcha

  override def ui( s:Scope, f:Field, opts:(String,String)* ) =
    <head>
     <script type="text/javascript" src="https://www.google.com/recaptcha/api/js/recaptcha_ajax.js"></script>

     <script type="text/javascript">{ Unparsed( """
       function showRecaptcha(element) {
         Recaptcha.create( """" + B.reCaptchaPublicKey + """", element, {
           theme: """" + theme + """",
           callback: Recaptcha.focus_response_field});
       }
      """ ) } </script>
    </head>

    <div id="recaptcha_div"></div>
      
    <tail>
    <script type="text/javascript">{ Unparsed( """
       showRecaptcha('recaptcha_div');      
     """ ) } </script>
    </tail>;

  override def inputcClasses = " recaptcha"

  override val validations =
    ( ( scope:Scope ) => {
      val sess = T.session

      scope.captcha &&
      !sess.passedCaptcha &&
      scope.rec.hasSubmitted && {
        val web = T.web
        val passedCaptcha =
          "https://www.google.com/recaptcha/api/verify".POST(
            form = Map(
              "privatekey" -> B.reCaptchaPrivateKey,
              "remoteip"   -> ( web.req.getRemoteAddr or "localhost" ),
              "challenge"  -> web.req.s( "recaptcha_challenge_field" ),
              "response"   -> web.req.s( "recaptcha_response_field" )
            ) ).trim.startsWith( "true" )

        if ( passedCaptcha )
          sess.passedCaptcha = true

        !passedCaptcha
      } |* Some( Invalid( scope, "Invalid captcha." ) )
    } ) ::
    super.validations
}

