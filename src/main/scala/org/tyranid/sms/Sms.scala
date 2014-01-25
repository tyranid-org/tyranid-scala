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

package org.tyranid.sms

import scala.language.postfixOps
// http://ai.fmcsa.dot.gov/SMS/Data/Downloads.aspx

import scala.xml.{ Unparsed, NodeSeq, Text }

import com.nexmo.messaging.sdk.{ NexmoSmsClient, SmsSubmissionResult }
import com.nexmo.messaging.sdk.messages.TextMessage

import org.tyranid.bson.BsonObject
import org.tyranid.db.{ DbBoolean, DbUpperChar, DbPhone, DbInt, Scope, Record }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity }
import org.tyranid.Imp._
import org.tyranid.math.Base62
import org.tyranid.profile.User
import org.tyranid.session.Notification
import org.tyranid.time.Time
import org.tyranid.web.{ Weblet, WebContext }
import org.tyranid.ui.{ Grid, Row, PathField, UiStyle }

import java.util.Date

object SMS extends MongoEntity( tid = "a0Gt" ) {
  "phone"        is DbPhone        as "Mobile Number" is 'required;
  "ok"           is DbBoolean      as "Verified";
  "on"           is DbBoolean      as "Notifications";
  "timeStart"    is DbInt          as "Starting Time";
  "timeEnd"      is DbInt          as "Ending Time";
  "vCode"        is DbUpperChar(6) as "Verfication Code";
  "enteredCode"  is DbUpperChar(6)  is 'temporary as "Verification Code" is 'required;
  
  var enabled = B.PRODUCTION

  def send( sms:BsonObject, msg: => String ) = {
          
    if ( enabled && sms.b( 'ok ) && sms.b( 'on ) ) {
      val smsPhone = sms.s( 'phone ).toOnlyNumbers
            
      if ( smsPhone notBlank )
        B.sms.send( "1" + smsPhone, msg )
    }
  }
}

case class NexmoApp( apiKey:String, secret:String, defaultFrom:String ) {
   // NexmoSmsClient
   val client = new NexmoSmsClient( apiKey, secret )
   
   var lastBalanceSentDate:Long = System.currentTimeMillis
   
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
         
         /*
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
        
        */
        val remainingBalance = result.getRemainingBalance
         
        if ( remainingBalance != null ) {
          println( "Remaining-Balance [ " + remainingBalance + " ] ..." )
          
          if ( remainingBalance.intValue() < 10 ) {
            val now = System.currentTimeMillis
            
            // Only send one every six hours
            if ( ( now - lastBalanceSentDate ) > ( 6 * Time.OneHourMs ) ) { 
              log( Event.Alert, "m" -> ( "SMS Nexmo balance is low, at [" + remainingBalance + "]!" ) )
              lastBalanceSentDate = now
            }
          }
        }
          
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
            val users = B.User.db.find( Mobj( "sms.phone" -> from, "sms.ok" -> true, "sms.on" -> true ) )
            
            for ( u <- users )
              B.User.db.update( Mobj( "_id" -> u.id ), Mobj( $set -> Mobj( "sms.on" -> false ) ) )
          case "on" =>
            val users = B.User.db.find( Mobj( "sms.phone" -> from, "sms.ok" -> true, "sms.on" -> false ) )
            
            for ( u <- users ) 
              B.User.db.update( Mobj( "_id" -> u.id ), Mobj( $set -> Mobj( "sms.on" -> true ) ) )
          case _ =>
        }
        
        log( Event.SmsIn, "m" -> ( "from=" + from + ", msgId=" + msgId + ", text=" + text ) )
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
    case "/clearNumber" =>
      redirectIfNotLoggedIn( web )
      val (user,sms) = smsStart
      
      sms( 'phone ) = null
      user.save
      
      sess.notice( "SMS/Mobile Phone cleared" )
      web.forward( "/user/edit?id=" + web.s( "id" ) or "" )
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
           <header>With { Text( B.applicationName ) }, you can send and receive SMS messages to your mobile phone</header>
           <form method="post" action={ web.path } id="f">
            <table style="width:100%">
            { Scope( user, saving = true ).draw( ui ) }
            </table>
           </form>
           <footer class="btns">
            <input type="hidden" value="1" name="savingHere"/>
            <a href={ "/user/edit?id=" + tid } id="cancelsms" class="btn">Done</a>
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
                
                B.sms.send( "1" + smsNumber, B.applicationName + ". Other charges may apply. Please enter this verification code at " + T.baseWebsite + ": " + vCode )
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
          <input type="submit" id="dlgSubmit" class="btn-success btn" value="Send Verification"/>
          { if ( sms.s( 'phone ).toOnlyNumbers.notBlank ) 
            <a href={ "/sms/clearNumber?id=" + user.tid } class="btn-success btn">Clear Number</a>
          }
          <a href={ "/user/edit?id=" + user.tid } id="cancelsms" class="btn">Cancel</a>
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
            <input type="submit" id="dlgSubmit" class="btn-success btn" value="Verify"/>
            <a href={ "/user/edit?id=" + user.tid } id="cancel" class="btn">Cancel</a>
           </footer>
         </form>
      }
      
      web.res.json( Map( 
        "html" -> (
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
      
      B.sms.send( "1" + sms.s( 'phone ).toOnlyNumbers, B.applicationName + ". Other charges may apply. Please enter this verification code at " + T.baseWebsite + ": " + vCode )
      sess.notice( "Verfication code has been sent again." )
      web.forward( "/sms/verify?id=" + web.s( "id" ) or "" )
  
    case "/clearSend" =>
      redirectIfNotLoggedIn( web )
      
      var (user,sms) = smsStart
      
      sess.notice( "SMS Information cleared." )
      sms( 'vCode ) = null
      sms( 'phone ) = null
      sms( 'on ) = false
      sms( 'ok ) = false
      
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
        val user = T.session.user
        val tid = web.s( "id" ) or user.tid
        val orgId = user.org.id

        B.User.getByTid( tid )
      }
      
      T.editing( user )
      
      ( user, user.o_!( 'sms ) )
    }
  }
}
