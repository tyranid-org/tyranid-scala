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
import org.tyranid.ui.{ Grid, Row, PathField, UiStyle }

object SMS extends MongoEntity( tid = "a0Gt" ) {
  "phone"        is DbPhone        as "Mobile Number" is 'required;
  "ok"           is DbBoolean      as "Verified";
  "on"           is DbBoolean      as "Notifications";
  "timeStart"    is DbInt          as "Starting Time";
  "timeEnd"      is DbInt          as "Ending Time";
  "vCode"        is DbUpperChar(6) as "Verfication Code";
  "enteredCode"  is DbUpperChar(6)  is 'temporary as "Verfication Code" is 'required;
  
  var enabled = B.PRODUCTION
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
        val from = web.s( "msisdn" ).toPhoneMask
        val msgId = web.s( "messageId" )
        val text = web.s( "text" )
        
        text.trim.toLowerCase match {
          case "off" =>
            val users = B.User.db.find( Mobj( "sms.phone" -> from, "sms.on" -> true ) )
            
            for ( u <- users )
              B.User.db.update( Mobj( "_id" -> u.id ), Mobj( $set -> Mobj( "sms.on" -> false ) ) )
          case "on" =>
            val users = B.User.db.find( Mobj( "sms.phone" -> from, "sms.on" -> false ) )
            
            for ( u <- users ) 
              B.User.db.update( Mobj( "_id" -> u.id ), Mobj( $set -> Mobj( "sms.on" -> true ) ) )
          case _ =>
        }
        
        log( Log.SMS_In, "m" -> ( "from=" + from + ", msgId=" + msgId + ", text=" + text ) )
      }
      
    case "/toggleOk" =>
      redirectIfNotLoggedIn( web )
      val (user,sms) = smsStart
      
      if ( sms.b( 'ok ) ) {
        sms( 'ok ) = false
        sms( 'on ) = false
        user.save
      }
        
      web.forward( "/sms/verify?id=" + web.s( "id" ) or "" )
    case "/toggleOn" =>
      redirectIfNotLoggedIn( web )
      val (user,sms) = smsStart
      
      sms( 'on ) = !sms.b( 'on )
      user.save
        
      web.forward( "/sms/edit?id=" + web.s( "id" ) or "" )
    case "/edit" =>
      redirectIfNotLoggedIn( web )
      val (user,sms) = smsStart
    
      if ( !sms.b( 'ok ) && !web.b( 'savingHere ) )
        web.forward( "/sms/verify?id=" + web.s( "id" ) or "" )
    
      var tid = user.tid
      
      val ui = user.view.ui(
         "editSms",
         Grid(
           Row( PathField( "sms.phone", opts = Seq( "readonly" -> "1" ) ) ),
           Row( PathField( "sms.ok", opts = Seq( "labels" -> "Validate Now|Invalidate and enter new number", "href" -> ( "/sms/toggleOk?id=" + tid ) ), uiStyle = UiStyle.Toggle ) ),
           Row( PathField( "sms.on", opts = Seq( "labels" -> "Enable|Disable", "href" -> ( "/sms/toggleOn?id=" + tid ) ), uiStyle = UiStyle.Toggle ) ) ) )
        
      if ( web.b( 'saving ) && !web.b( 'verify ) ) {
        val invalids = Scope( user, saving = true ).submit( user, ui )
        
        if ( invalids.isEmpty ) {
          sess.notice( "SMS information has been updated." )
          user.save
          web.res.html(NodeSeq.Empty)
          return
        }
      } else if ( web.b( "toggleSmsOk" ) ) {
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
            <input type="hidden" value="1" name="savingHere"/>
            <a href={ "/user/edit?id=" + tid } id="cancel" class="greyBtn">Done</a>
           </footer> ), 
         "endpoint" -> ( "/sms/edit?id=" + tid ),
         "onCloseRedirect" -> true ) )

    case "/verify" =>
      redirectIfNotLoggedIn( web )
      
      var form:NodeSeq = null
      var header:NodeSeq = null
          
      var (user,sms) = smsStart
      var saving = web.b( 'saving )
      
      if ( sms.b( 'ok ) )
        web.forward( "/sms/edit?id=" + web.s( "id" ) or "" )
        
      val verifyUi = user.view.ui( "verifySms", Grid( Row( "sms.phone" ) ) )
      val enterVerifyUi = user.view.ui( "enterVerifySms", Grid( Row( "sms.enteredCode" ) ) )
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
                user.clearSubmit
                
                val vCode = Base62.make( 6 ).toUpperCase()
                sms( "vCode" ) = vCode
                user.save
                
                B.sms.send( "1" + smsNumber, B.applicationName + ". Other charges may apply. Please enter this verification code at " + B.website + ": " + vCode )
                header = sendHeader( smsNumber.toPhoneMask, user.tid )
              }
            }
          }
        } else {
          ui = enterVerifyUi
          
          if ( web.b( 'verify ) ) {
            val invalids = Scope( user, saving = true ).submit( user, ui )
          
            if ( invalids.isEmpty ) {
               if ( sms.s( 'enteredCode ).toUpperCase() == sms.s( 'vCode ) ) {
                 sms( "ok" ) = true
                 sms( "vCode" ) = null
                 user.save
                 sess.notice( "Your SMS phone is now verified." )
                 web.forward( "/sms/edit?id=" + web.s( "id" ) or "" )
               }
               
               println( sms.s( 'enteredCode ).toUpperCase() )
               println( sms.s( 'vCode ) )
               sess.error( "That verification code is not correct." )
            }
          }

          header = sendHeader( sms.s( 'phone ), user.tid )
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
        "onCloseRedirect" -> true,
        "endpoint" -> "/sms/verify") )
        
    case "/sendAgain" =>
      redirectIfNotLoggedIn( web )
      
      var (user,sms) = smsStart
      
      var vCode = sms.s( 'vCode )
      
      if ( vCode isBlank ) {
         vCode = Base62.make( 6 ).toUpperCase()
         sms( "vCode" ) = vCode
         user.save
      }
      
      B.sms.send( "1" + sms.s( 'phone ).toOnlyNumbers, B.applicationName + ". Other charges may apply. Please enter this verification code at " + B.website + ": " + vCode )
      sess.notice( "Verfication code has been sent again." )
      web.forward( "/sms/verify?id=" + web.s( "id" ) or "" )
  
    case "/clearSend" =>
      redirectIfNotLoggedIn( web )
      
      var (user,sms) = smsStart
      
      sess.notice( "Verfication code has been cleared." )
      sms( 'vCode ) = null
      user.save
       
      web.forward( "/sms/verify?id=" + web.s( "id" ) or "" )
    }
  
    def sendHeader( smsNumber:String, tid:String ) = 
      <header>
       { B.applicationName + " sent an SMS message to your phone " + smsNumber + ". Please enter the verification code below. " }
       <a id="sendAgain" href={ "/sms/sendAgain?id=" + tid }>Send Again</a> or <a id="clearSend" href={ "/sms/clearSend?id=" + tid }>Clear</a>
       <script>{ Unparsed( """window.lastDialog.updateHref( 'sendAgain' ); window.lastDialog.updateHref( 'clearSend' )""" ) }</script>
      </header>
    
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
