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

package org.tyranid.sms

import scala.xml.{ Unparsed, NodeSeq }

import com.nexmo.messaging.sdk.{ NexmoSmsClient, SmsSubmissionResult }
import com.nexmo.messaging.sdk.messages.TextMessage

import org.tyranid.db.{ DbBoolean, DbUpperChar, DbPhone, DbInt, Scope, Record }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity }
import org.tyranid.Imp._
import org.tyranid.math.Base62
import org.tyranid.profile.User
import org.tyranid.session.Notification
import org.tyranid.web.{ Weblet, WebContext, WebTemplate }
import org.tyranid.ui.{ Grid, Row, Field, Opts }


object SMS extends MongoEntity( tid = "a0Gt" ) {
  "phone"        is DbPhone        as "Mobile Number" is 'required;
  "ok"           is DbBoolean      as "SMS Verified";
  "on"           is DbBoolean      as "SMS Notifications";
  "timeStart"    is DbInt          as "Starting Time";
  "timeEnd"      is DbInt          as "Ending Time";
  "vCode"        is DbUpperChar(6) as "Verfication Code";
  "enteredCode" is DbUpperChar(6) is 'temporary as "Verfication Code";
  
  var enabled = false
}

case class NexmoApp( apiKey:String, secret:String, defaultFrom:String ) {
   // NexmoSmsClient
   var client = new NexmoSmsClient( apiKey, secret )
   
   def send( to:String, text:String, from:String = defaultFrom ) = {
     if ( client != null && SMS.enabled ) {
       val message = new TextMessage( from, to, text )
       
       // SmsSubmissionResult[]
       val results = client.submitMessageHttps( message )
       
       // Evaluate the results of the submission attempt ...
       println( "... Message submitted in [ " + results.length + " ] parts" )
       
       var num = 0
       
       for ( result <- results ) {
         val status = result.getStatus()
         
         println( "--------- part [ " + ( num + 1 ) + " ] ------------\n" +
                  "Status [ " + status + " ] ..." )
              
         if ( status == SmsSubmissionResult.STATUS_OK )
           println( "SUCCESS" )
         else if ( result.getTemporaryError() )
           println( "TEMPORARY FAILURE - PLEASE RETRY" )
         else
           println( "SUBMISSION FAILED!" )
         
         println( "Message-Id [ " + result.getMessageId() + " ] ...\n" +
                  "Error-Text [ " + result.getErrorText() + " ] ..." )
  
        if ( result.getMessagePrice() != null )
          println( "Message-Price [ " + result.getMessagePrice() + " ] ..." )
        
        if ( result.getRemainingBalance() != null )
          println( "Remaining-Balance [ " + result.getRemainingBalance() + " ] ..." )
          
        num += 1
      }
    }
  }
}

object Smslet extends Weblet {

  def handle(web: WebContext) {
    val sess = T.session
    
    rpath match {
    case "/" =>
      if ( web.s( "type" ) == "text" ) {
        val from = web.s( "msisdn" )
        
        println( "SMS message from: " + from )
        
        val msgId = web.s( "messageId" )
        
        val text = web.s( "text" )
        
        text.toLowerCase match {
          case "off" =>
            val users = B.User.db.find( Mobj( "sms.mobilePhone" -> from ), Mobj( "sms.on" -> 1 ) )
            
            for ( u <- users ) if ( u.b( 'smsOn ) )
              B.User.db.update( Mobj( "_id" -> u.id ), Mobj( $set -> Mobj( "sms.on" -> false ) ) )
          case "on" =>
            val users = B.User.db.find( Mobj( "sms.mobilePhone" -> from ), Mobj( "sms.on" -> 1 ) )
            
            for ( u <- users ) if ( !u.b( 'smsOn ) )
              B.User.db.update( Mobj( "_id" -> u.id ), Mobj( $set -> Mobj( "sms.on" -> true ) ) )
          case _ =>
        }
        
        log( Log.SMS_In, "m" -> ( "from=" + from + ", msgId=" + msgId + ", text=" + text ) )
      }
      
    case "/edit" =>
      redirectIfNotLoggedIn( web )
      
      val (user,sms) = smsStart
      
      if ( !sms.b( 'ok ) )
        web.forward( "/sms/verify?id=" + web.s( "id" ) or "" )
    
      val ui = user.view.ui(
         "editSms",
         Grid(
           Row( 'sms_phone ),
           Row( Field( 'sms_ok, Opts( "labels" -> "Verify Now|", "href" -> ( web.path + "?toggleSmsOk=1" ) ), uiStyle = Field.UI_STYLE_TOGGLE ) ),
           Row( Field( 'sms_on, Opts( "labels" -> "Enable|Disable", "href" -> ( web.path + "?toggleSmsOn=1" ) ), uiStyle = Field.UI_STYLE_TOGGLE ) ) ) )
        
      if ( web.b( 'saving ) ) {
        val invalids = Scope( user, saving = true ).submit( user, ui )
        
        if ( invalids.isEmpty ) {
          sess.notice( "SMS information has been updated." )
          user.save
          web.res.html(NodeSeq.Empty)
          return
        }
      } else if ( web.b( "toggleSmsOk" ) ) {
        val smsPhone = user.s( 'mobilePhone ).toPhoneMask
        
        if ( smsPhone != null ) {
          sess.notice( "SMS verification message sent." )
          B.sms.send( "1" + smsPhone, "SMS Volerro verification message" )
        } else {
          sess.warn( "Mobile Number should be set first." )
        }
        
        sess.error( "foo " )
      } else if ( web.b( "toggleSmsOn" ) ) {
        user( 'smsOn ) = !user.b( 'smsOn )
      }
     
      web.res.html(
      { Notification.box } ++
      <header>With Volerro, you can send and receive SMS messages to your mobile phone</header>
      <form method="post" action={ web.path } id="f">
       <table>
        { Scope( user, saving = true ).draw( ui ) }
       </table>
      </form>
      <footer class="btns">
       <input type="submit" id="dlgSubmit" class="greenBtn" value="Save" name="saving"/>
       <a href={ "/user/edit?id=" + user.tid } id="cancel" class="greyBtn">Cancel</a>
      </footer> )
      
    case "/verify" =>
      redirectIfNotLoggedIn( web )
      
      var form:NodeSeq = null
      var header:NodeSeq = null
          
      val (user,sms) = smsStart
      val verifyUi = user.view.ui( "verifySms", Grid( Row( 'sms_phone ) ) )
      val enterVerifyUi = user.view.ui( "enterVerifySms", Grid( Row( 'sms_enteredCode ) ) )
      var ui:org.tyranid.ui.UiObj = null
      
      if ( !web.b( 'saving ) ) {
        ui = verifyUi
        
        header = <header>With Volerro, you can send and receive SMS messages to your mobile phone.</header>
        form = 
        <form method="post" action={ web.path } id="f">
         <table style="width: 100%">
          { Scope( user, saving = true ).draw( ui ) }
         </table>
         <input type="hidden" value={ if ( sms.b( 'ok ) ) sms.s( 'phone ).toOnlyNumbers else null } name="verifiedNumber"/>
         <input type="hidden" value="1" name="verify"/>
         <footer class="btns">
          <input type="submit" id="dlgSubmit" class="greenBtn" value="Send Verification"/>
          <a href={ "/user/edit?id=" + user.tid } id="cancel" class="greyBtn">Cancel</a>
         </footer>
        </form>
      } else {
        if ( web.b( 'verify ) ) {
          ui = verifyUi
          
          val invalids = Scope( user, saving = true ).submit( user, ui )
        
          if ( invalids.isEmpty ) {
            val smsNumber = sms.s( 'phone ).toOnlyNumbers
            
            if ( smsNumber == null || smsNumber.length != 10  ) {
              sess.warn( "Please enter in the format (999) 999-9999" )
            } else {
              val verifiedNumber = web.s( 'verifiedNumber )
              
              if ( verifiedNumber.notBlank && verifiedNumber == smsNumber ) {
                sess.notice( "That number has already been verified!" )
              } else {
                // need to verify
                sess.notice( "We just sent an SMS message to your phone " + smsNumber.toPhoneMask )
                sms( "vCode" ) = Base62.make( 6 ).toUpperCase()
                user.save
                web.res.html(NodeSeq.Empty)
                return
              }
            }
          }
        }
      }
      
      web.res.html(
      { Notification.box } ++
      { header } ++
      { form } )
    }
  
    def smsStart = {
      val user:User = { 
        val tid = web.s( "id" ) or T.session.user.tid
    
        T.session.editing2( B.User.getClass(), {
          Record.byTid( tid, only = B.User ).map( _.snapshot.as[User] ).getOrElse {
            T.session.warn( "User not found." )
            web.redirect( "/" )
          }
        } )
      }
      
      T.editing( user )
      
      ( user, user.o_!( 'sms ) )
    }
  }
}