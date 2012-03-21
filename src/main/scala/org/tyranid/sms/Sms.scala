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

import org.tyranid.db.{ DbBoolean, DbChar, DbPhone, DbInt, Scope, Record }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity }
import org.tyranid.Imp._
import org.tyranid.profile.User
import org.tyranid.session.Notification
import org.tyranid.web.{ Weblet, WebContext, WebTemplate }
import org.tyranid.ui.{ Grid, Row, Field, Opts }


object SMS extends MongoEntity( tid = "a0Gt" ) {
  "phone"       is DbPhone   as "Mobile Number" is 'required;
  "ok"          is DbBoolean as "SMS Verified";
  "on"          is DbBoolean as "SMS Notifications";
  "timeStart"   is DbInt     as "Starting Time";
  "timeEnd"     is DbInt     as "Ending Time";
  "aCode"       is DbChar(5) as "Activation Code";
  
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
        
      val ui = user.view.ui(
         "editSms",
         Grid(
           Row( 'sms_phone ),
           Row( Field( 'sms_ok, Opts( "labels" -> "Verify Now|", "href" -> ( web.path + "?toggleSmsOk=1" ) ), uiStyle = Field.UI_STYLE_TOGGLE ) ),
           Row( Field( 'sms_on, Opts( "labels" -> "Enable|Disable", "href" -> ( web.path + "?toggleSmsOn=1" ) ), uiStyle = Field.UI_STYLE_TOGGLE ) ) ) )
        
      val invalids = Scope( user, saving = true ).submit( user, ui )
      
      if ( web.b( 'saving ) && invalids.isEmpty ) {
        sess.notice( "SMS information has been updated." )
        user.save
        web.res.html(NodeSeq.Empty)
        return
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
      <form method="post" action={ web.path } id="f">
       <table>
        { Scope( user, saving = true ).draw( ui ) }
       </table>
       <div class="btns">
        <input type="submit" id="dlgSubmit" class="greenBtn" value="Save" name="saving"/>
        <a href={ "/user/edit?id=" + user.tid } id="cancel" class="greyBtn">Cancel</a>
       </div>
      </form> )
    }
  }
}