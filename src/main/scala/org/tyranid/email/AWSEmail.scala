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

package org.tyranid.email

import javax.mail.MessagingException
import javax.mail.internet.InternetAddress

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import org.tyranid.Imp._

import com.amazonaws.AmazonClientException
import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.simpleemail._
import com.amazonaws.services.simpleemail.model._

object AWSEmail {
  var lastSent:Long = 0
  var currentCount:Int = 0
  
  def apply( subject:String, text:String, to:String, from:String ) {
    AWSEmail( subject, text ).addTo( to ).from( from ).send
  }
  
  private def throttle = synchronized {
    if ( currentCount > 5 && System.currentTimeMillis - lastSent < 1000 ) 
      Thread.sleep( 1000 )

    if ( currentCount > 5 )
      currentCount = 1
    else 
      currentCount += 1 

    AWSEmail.lastSent = System.currentTimeMillis
  }
  
  /*
  val verifyCache = mutable.ArrayBuffer[String]()
  
  def verifyEmailAddress( email:String ):Boolean = {
    val verifiedAddresses = client.listVerifiedEmailAddresses().getVerifiedEmailAddresses()
    
    if ( verifiedAddresses.contains( email ) )
      return true
    
    val request = new VerifyEmailAddressRequest()
    request.setEmailAddress( email )
    client.verifyEmailAddress( request )
    
    return false
  }
  */
  
  val client = new AmazonSimpleEmailServiceClient( B.awsCredentials )
}

case class AWSEmail( subject:String, text:String, html:String=null ) extends Email {
  var request:SendEmailRequest = null
  
  @throws(classOf[MessagingException])
  override def compose:Email = {
    //com.amazonaws.services.simpleemail.
    //if (defaultFrom) 
    //  sender( Configs.getDefaultMailFrom() )

    if ( from == null ) 
      throw new MessagingException( "A from must be set on this email message!" )
    
    request = new SendEmailRequest().withSource( from.getAddress() )

    if ( replyTo != null && replyTo != from )  {
      val toAddresses = new java.util.ArrayList[String]()
      toAddresses.add( replyTo.getAddress() )
      request.setReplyToAddresses( toAddresses )
    }
      
    if ( primaryRecipients == null ) 
      throw new MessagingException( "The primary recipients must be set on this email message." )
   
    val toAddresses = new java.util.ArrayList[String]()
    
    for ( recipient <- primaryRecipients )
      toAddresses.add( recipient.getAddress() )
    
    request.setDestination( new Destination().withToAddresses( toAddresses ) )

    if ( ccRecipients != null ) {
        toAddresses.clear()
    
        for ( recipient <- ccRecipients )
          toAddresses.add( recipient.getAddress() )
    
        request.setDestination( new Destination().withCcAddresses( toAddresses ) )
    }

    if ( bccRecipients != null ) {
        toAddresses.clear()
    
        for ( recipient <- bccRecipients )
          toAddresses.add( recipient.getAddress() )
    
        request.setDestination( new Destination().withBccAddresses( toAddresses ) )
    }
    
    val subjContent = new Content().withData( if ( subject == null ) "" else subject )

    val msg = new Message().withSubject( subjContent )
                
    val body = new Body()
    
    if ( text != null )
      body.withText( new Content().withData( text ) )
      
    if ( html != null )
      body.withHtml( new Content().withData( html ) )

    msg.setBody( body )
    //request.setReturnPath( B.bounceEmail )
    request.setMessage( msg )
    
    this
  }
  
  @throws(classOf[MessagingException])
  override def send():Email = {
    compose
    
    AWSEmail.throttle
    
    try {
      AWSEmail.client.sendEmail( request )
    } catch {
      case e:MessageRejectedException =>
        spam(
            "| MessageRejectedException: " + e.getMessage() + "\n" +
            "|  From: " + from.getAddress() + "\n" +
            "|  Sent to: " + primaryRecipients.mkString( "," ) + "\n" +
            "|  Reply to: " + ( if ( replyTo != null && replyTo != from ) replyTo.getAddress() else "" ) + "\n" +
            "|  Subject: " + subject + "\n" +
            "|  Text: " + text + "\n" +
            "|  HTML: " + html
        )
        
        throw e
    } 
    
    this
  }
}
