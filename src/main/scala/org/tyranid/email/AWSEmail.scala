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

package org.tyranid.email

import java.io.ByteArrayOutputStream

import java.nio.ByteBuffer
import java.util.Arrays

import javax.mail.MessagingException
import javax.mail.internet._

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import org.tyranid.Imp._

import com.amazonaws.{ AmazonClientException, AmazonServiceException }
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
    if ( currentCount > 5 && ( System.currentTimeMillis - lastSent ) < 2000 )
      Thread.sleep( 1000 )

    if ( currentCount > 5 )
      currentCount = 1
    else
      currentCount += 1

    AWSEmail.lastSent = System.currentTimeMillis
  }

  val client = new AmazonSimpleEmailServiceClient( B.awsCredentials )
}

case class AWSEmail( subject:String, text:String, html:String=null, fromLog: Boolean = false ) extends Email {
  var request:SendEmailRequest = null
  var rawRequest:SendRawEmailRequest = null

  @throws(classOf[MessagingException])
  def composeJavaMail:Email = {
    if ( from == null )
      throw new MessagingException( "A from must be set on this email message!" )

    val jMail = JavaEmail( subject = subject, text = text, html = html )

    for ( recipient <- primaryRecipients )
      jMail.addTo( recipient.getAddress() )

    jMail.from( from.getAddress() )

    if ( replyTo != null && replyTo != from )
      jMail.replyTo( replyTo.getAddress )

    for ( attachment <- attachments )
      jMail.addAttachment( attachment )

    jMail.compose

    val mimeMessage = jMail.message

    val outputStream = new ByteArrayOutputStream()
    mimeMessage.writeTo( outputStream )
    val rawMessage = new RawMessage( ByteBuffer.wrap( outputStream.toByteArray ) )


    rawRequest = new SendRawEmailRequest( rawMessage )

    rawRequest.setDestinations( primaryRecipients.map( _.getAddress ) )

    rawRequest.setSource( from.getAddress )

    this
  }

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

    val subjContent = new Content().withData( subject.isBlank ? "" | subject )

    val msg = new Message().withSubject( subjContent )
    val body = new Body()

    if ( text.notBlank )
      body.withText( new Content().withData( text ) )

    if ( html.notBlank )
      body.withHtml( new Content().withData( html ) )

    msg.setBody( body )

    //request.setReturnPath( B.bounceEmail )
    request.setMessage( msg )

    this
  }

  /*
   * This only buries exceptions if the exception is on an email originating from a Log message
   */
  @throws(classOf[MessagingException])
  override def send():Email = {
    if ( Email.enabled ) {

      if ( !B.PRODUCTION )
        if ( T.session != null && !T.session.isAllowingEmail ) return this

      val withAttachments = ( attachments != null && attachments.length > 0 )

      if ( withAttachments ) {
        // Create javamail and just send it to SES
        composeJavaMail
      } else {
        compose
      }

      if ( false && request.getDestination().getToAddresses().find( a => a.contains( "mrkcbradley" ) || a.contains( "mbradley" ) ) == None ) {
        println( """
*********

  Warning!!! EMAIL IS ONLY BEING SENT TO mbradley@volerro.com

*********
""")
        return null
      }

      AWSEmail.throttle

      try {
        if ( rawRequest != null )
          AWSEmail.client.sendRawEmail( rawRequest )
        else
          AWSEmail.client.sendEmail( request )
      } catch {
        case e:MessageRejectedException =>
          if ( !fromLog ) {
            val sess = T.session
            val msg = e.getMessage
            val fromAddress = from.getAddress
            val recipients = primaryRecipients.mkString( "," )
            val user = ( sess == null ) ? null | sess.user

            // Only send these if one of the real users is sending email
            val userEmail = ( user == null || user == B.systemUser ) ? null | user.s( 'email )

            if ( userEmail.notBlank && !Email.isBlacklisted( userEmail ) )
              sendRejectionNotice( msg, userEmail )

              e.logWith( "m" -> (
                  "| MessageRejectedException: " + msg + "\n" +
                  "|  From: " + fromAddress + "\n" +
                  ( userEmail.notBlank ? ( "|  From User Email: " + userEmail + ( ( Email.isBlacklisted( userEmail ) ) |* " (BLACKLISTED)" ) + "\n" ) | "" ) +
                  "|  Sent to: " + recipients + "\n" +
                  "|  Reply to: " + ( if ( replyTo != null && replyTo != from ) replyTo.getAddress() else "" ) + "\n" +
                  "|  Subject: " + subject + "\n" +
                  "|  Text: " + text + "\n" +
                  "|  HTML: " + html )
                  )

            throw e
          }
        case e2:Throwable =>
          if ( !fromLog ) {
            val sess = T.session
            val msg = e2.getMessage
            val fromAddress = from.getAddress
            val recipients = primaryRecipients.mkString( "," )
            val user = ( sess == null ) ? null | sess.user
            val userEmail:String = ( user == null || user == B.systemUser ) ? null | user.s( 'email )

              e2.logWith( "m" -> (
                  "| AWS Mail Exception: " + msg + "\n" +
                  "|  From: " + fromAddress + "\n" +
                  ( userEmail.notBlank ? ( "|  From User Email: " + userEmail + ( ( Email.isBlacklisted( userEmail ) ) |* " (BLACKLISTED)" ) + "\n" ) | "" ) +
                  "|  Sent to: " + recipients + "\n" +
                  "|  Reply to: " + ( if ( replyTo != null && replyTo != from ) replyTo.getAddress() else "" ) + "\n" +
                  "|  Subject: " + subject + "\n" +
                  "|  Text: " + text + "\n" +
                  "|  HTML: " + html )
                  )

            throw e2
          }
      }
    }

    this
  }

  private def sendRejectionNotice( msg:String, userEmail:String ) {
    if ( primaryRecipients.length == 1 ) Email.blacklist( primaryRecipients.head.getAddress )
    val recipients = primaryRecipients.mkString( "," )

    AWSEmail( subject = "Failed to send email: " + subject,
              text = """
Hi,

Sorry, but """ + B.applicationName + """ failed to send the following mail because one of the recipients rejected it as: """ + msg + """

Email To: """ + recipients + """
----

""" + text,
              html = """
<p style="font-family: 'Helvetica Neue', Arial, Helvetica, sans-serif;font-size: 18px;font-weight: bold;color: #333333;">
  <p>Hi,</p>
  <p>Sorry, but Volerro failed to send the following mail because one of the recipients rejected it as: <b>""" + msg + """</b></p>
  <p>Email To: """ + recipients + """</p>
  <p>----</p>
</p>
""" + html )
              .addTo( userEmail )
              .from( "no-reply@" + B.domain )
              .send
  }
}
