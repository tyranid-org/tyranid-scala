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

import com.nexmo.messaging.sdk.{ NexmoSmsClient, SmsSubmissionResult }
import com.nexmo.messaging.sdk.messages.TextMessage

import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity }
import org.tyranid.Imp._
import org.tyranid.web.{ Weblet, WebContext, WebTemplate }

object SMS {
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
        val msgId = web.s( "messageId" )
        
        val text = web.s( "text" )
        
        text.toLowerCase match {
          case "off" =>
            val users = B.User.db.find( Mobj( "mobilePhone" -> from ), Mobj( "smsOn" -> 1 ) )
            
            for ( u <- users ) if ( u.b( 'smsOn ) )
              B.User.db.update( Mobj( "_id" -> u.id ), Mobj( $set -> Mobj( "smsOn" -> false ) ) )
          case "on" =>
            val users = B.User.db.find( Mobj( "mobilePhone" -> from ), Mobj( "smsOn" -> 1 ) )
            
            for ( u <- users ) if ( !u.b( 'smsOn ) )
              B.User.db.update( Mobj( "_id" -> u.id ), Mobj( $set -> Mobj( "smsOn" -> true ) ) )
          case _ =>
        }
        
        log( Log.SMS_In, "m" -> ( "from=" + from + ", msgId=" + msgId + ", text=" + text ) )
      }
    }
  }
}