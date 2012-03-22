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

import scala.xml.{ Unparsed, NodeSeq, Text }

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
import org.tyranid.ui.{ Grid, Row, PathField }


object SMS extends MongoEntity( tid = "a0Gt" ) {
  "phone"        is DbPhone        as "Mobile Number" is 'required;
  "ok"           is DbBoolean      as "Verified";
  "on"           is DbBoolean      as "Notifications";
  "timeStart"    is DbInt          as "Starting Time";
  "timeEnd"      is DbInt          as "Ending Time";
  "vCode"        is DbUpperChar(6) as "Verfication Code";
  "enteredCode"  is DbUpperChar(6)  is 'temporary as "Verfication Code" is 'required;
  
  var enabled = false
}

case class NexmoApp( apiKey:String, secret:String, defaultFrom:String ) {
   // NexmoSmsClient
   var client = new NexmoSmsClient( apiKey, secret )
   
   def send( to:String, text:String, from:String = defaultFrom ) = {
     //println( "client: " + client )
     //println( "sms enabled: " + SMS.enabled )
     //println( "to: " + to )
     //println( "text: " + text )
     
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
           Row( PathField( 'sms_phone, opts = Seq( "readonly" -> "1" ) ) ),
           Row( PathField( 'sms_ok, opts = Seq( "labels" -> "Validate Now|Invalidate and enter new number", "href" -> ( web.path + "?toggleSmsOk=1" ) ), uiStyle = PathField.UI_STYLE_TOGGLE ) ),
           Row( PathField( 'sms_on, opts = Seq( "labels" -> "Enable|Disable", "href" -> ( web.path + "?toggleSmsOn=1" ) ), uiStyle = PathField.UI_STYLE_TOGGLE ) ) ) )
        
      if ( web.b( 'saving ) && !web.b( 'verify ) ) {
        val invalids = Scope( user, saving = true ).submit( user, ui )
        
        if ( invalids.isEmpty ) {
          sess.notice( "SMS information has been updated." )
          user.save
          web.res.html(NodeSeq.Empty)
          return
        }
      } else if ( web.b( "toggleSmsOk" ) ) {
        web.forward( "/sms/verify?ok=1&id=" + web.s( "id" ) or "" )
      } else if ( web.b( "toggleSmsOn" ) ) {
        sms( 'on ) = !sms.b( 'on )
      }
     
      web.res.json( Map( 
        "html" -> ( 
           { Notification.box } ++
           <header>With { Text( B.applicationName ) }, you can send and receive SMS messages to your mobile phone</header>
           <form method="post" action={ web.path } id="f">
            <table style="width:100%">
            { Scope( user, saving = true ).draw( ui ) }
            </table>
           </form>
           <footer class="btns">
            <input type="submit" id="dlgSubmit" class="greenBtn" value="Save" name="saving"/>
            <a href={ "/user/edit?id=" + user.tid } id="cancel" class="greyBtn">Cancel</a>
           </footer> ) ) )

    case "/verify" =>
      redirectIfNotLoggedIn( web )
      
      var form:NodeSeq = null
      var header:NodeSeq = null
          
      val (user,sms) = smsStart
      var saving = web.b( 'saving )
      
      if ( web.b( 'ok ) ) {
        sms( 'ok ) = false
        saving = false
        user.save
      }
        
      if ( sms.b( 'ok ) )
        web.forward( "/sms/edit?id=" + web.s( "id" ) or "" )
        
      val verifyUi = user.view.ui( "verifySms", Grid( Row( 'sms_phone ) ) )
      val enterVerifyUi = user.view.ui( "enterVerifySms", Grid( Row( 'sms_enteredCode ) ) )
      var ui:org.tyranid.ui.UiObj = null
      
      if ( !saving && sms.s( 'vCode ).isBlank ) {
        ui = verifyUi        
      } else {
        if ( web.b( 'sendVerify ) ) {
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
                ui = enterVerifyUi
                
                val vCode = Base62.make( 6 ).toUpperCase()
                sms( "vCode" ) = vCode
                user.save
                
                B.sms.send( "1" + smsNumber, B.applicationName + ". Other charges may apply. Please enter this verification code at " + B.website + ": " + vCode )
                header = <header>{ "We just sent an SMS message to your phone " + smsNumber.toPhoneMask + ". Please enter the verification code below." }</header>
              }
            }
          }
        } else {
          ui = enterVerifyUi
          
          if ( web.b( 'verify ) ) {
            val invalids = Scope( user, saving = true ).submit( user, ui )
            val smsNumber = sms.s( 'phone ).toOnlyNumbers
            header = <header>{ "We sent an SMS message to your phone " + smsNumber.toPhoneMask + ". Please enter the verification code below." }</header>
          
            if ( invalids.isEmpty ) {
               if ( sms.s( 'enteredCode ) == sms.s( 'vCode ) ) {
                 sms( "ok" ) = true
                 sms( "vCode" ) = null
                 user.save
                 sess.notice( "Your SMS phone is now verified." )
                 web.forward( "/sms/edit?id=" + web.s( "id" ) or "" )
               }
               
               sess.error( "That verification code is not correct." )
            }
          } else {
            val smsNumber = sms.s( 'phone ).toOnlyNumbers
            header = <header>{ "We sent an SMS message to your phone " + smsNumber.toPhoneMask + ". Please enter the verification code below." }</header>
          }
        }
      }
      
      if ( ui == verifyUi ) {
        header = <header>With { Text( B.applicationName ) }, you can send and receive SMS messages to your mobile phone.</header>
          
        form = 
        <form method="post" action={ web.path } id="f">
         <table style="width: 100%">
          { Scope( user, saving = true ).draw( ui ) }
         </table>
         <input type="hidden" value={ if ( sms.b( 'ok ) ) sms.s( 'phone ).toOnlyNumbers else null } name="verifiedNumber"/>
         <input type="hidden" value="1" name="sendVerify"/>
         <footer class="btns">
          <input type="submit" id="dlgSubmit" class="greenBtn" value="Send Verification"/>
          <a href={ "/user/edit?id=" + user.tid } id="cancel" class="greyBtn">Cancel</a>
         </footer>
        </form>
      } else {
        form = 
          <form method="post" action={ web.path } id="f">
            <table style="width: 100%">
               { Scope( user, saving = true ).draw( ui ) }
            </table>
            <footer class="btns">
             <input type="hidden" value="1" name="verify"/>
            <input type="submit" id="dlgSubmit" class="greenBtn" value="Verify"/>
            <a href={ "/user/edit?id=" + user.tid } id="cancel" class="greyBtn">Cancel</a>
           </footer>
         </form>
      }
      
      web.res.json( Map( 
        "html" -> (
          { Notification.box } ++
          { header } ++
          { form } ),
        "onCloseRedirect" -> true ) )
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
