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

import java.io.{ File, UnsupportedEncodingException }
import java.util.{ Date, Properties }

import javax.activation._
import javax.mail._
import javax.mail.internet._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import org.tyranid.Imp._
import org.tyranid.db.{ DbChar, DbInt, DbPassword, DbEmail, DbBoolean }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.MongoEntity
import org.tyranid.profile.User
import org.tyranid.ui.LnF


object EmailConfig extends MongoEntity( tid = "a0At" ) {
  "_id"          is DbInt is 'id;
  "host"         is DbChar(40);
  "port"         is DbInt;
  "authUser"     is DbChar(40);
  "authPassword" is DbPassword;
  "tls"          is DbBoolean;
  "ssl"          is DbBoolean;
  "pollHost"     is DbChar(40);
  "pollUser"     is DbEmail;
  "pollPassword" is DbPassword;
  
  override lazy val dbName = name
}


object Email {
  var enabled = !B.DEV

  private val wellKnownProviders = Array(
    "@aol.com",
    "@fastmail.fm",
    "@gmail.com",
    "@hotmail.com",
    "@inbox.com",
    "@live.com",
    "@lycos.com",
    "@mac.com",
    "@mail.com",
    "@me.com",
    "@mediacomcc.com",
    "@msn.com",
    "@netamumail.com",
    "@yahoo.com",
    ".net" )

  def isBlacklisted( email:String ) = {
    val u = B.User.db.findOne( Mobj( "email" -> email.toPatternI ), Mobj( "noEmail" -> 1 ) )
    
    ( u == null ) ? false | u.b( 'noEmail )
  }

  def blacklist( email:String ) {
    B.User.db.update( Mobj( "email" -> email.toPatternI ), Mobj( $set -> Mobj( "noEmail" -> true ) ) )  
  }
  
  def isWellKnownProvider( email:String ) =
    wellKnownProviders exists email.toLowerCase.endsWith
  
  def domainFor( email:String ) =
    email.indexOf( '@' ) match {
    case -1 => ""
    case n  => email.substring( n+1 )
    }
    
  def domainPart( email:String ) =
    try {
      val emailParser = """([\w\d\+\-\_\.]+)(\+\d+)?@([\w\d\-]+)([\w\d\.])+""".r
      val emailParser( name, num, domain, com ) = email
      domain
    } catch {
    case e:MatchError =>
      email
    }

  @throws(classOf[AddressException])
  def getInetAddress( emailAddress:String, displayName:String = null ) : InternetAddress = {
    if ( displayName.notBlank ) {
      try {
        return new InternetAddress( emailAddress, displayName )
      } catch { 
        case uee:UnsupportedEncodingException => 
      }
    }

    return new InternetAddress( emailAddress )
  }
}

trait EmailTemplate {
  def welcome( user:User, activationCode:String = null, subjectPrefix:String = "", footerText:String = "" )
  def rejectUnknownFromEmail( unknownFromEmail:String, subject:String )
  def rejectUnknownTo( from:String, unknownTo:String, subject:String )
  def forgotPassword( user:User )
}

trait Email {
  val subject:String
  val text:String
  val html:String

  def send:Email
  def compose:Email

  var replyTo:InternetAddress = null
  var from:InternetAddress = null
  var primaryRecipients = ArrayBuffer[InternetAddress]()
  var ccRecipients:ArrayBuffer[InternetAddress] = null
  var bccRecipients:ArrayBuffer[InternetAddress] = null
  var sendDate:Date = null
  var attachments:ArrayBuffer[File] = null
  var defaultFrom:Boolean = false;

  def addTo( emailAddress:String )  = {
    if ( emailAddress.notBlank ) 
      add( Message.RecipientType.TO, emailAddress.split( "," ) : _* )
    else 
      this
  }

  def addCc( emailAddress:String ):Email = {
    if ( emailAddress.notBlank ) 
      return add( Message.RecipientType.CC, emailAddress.split( "," ) : _* )

    this
  }

  def addBcc( emailAddress:String ):Email = {
    if ( emailAddress.notBlank ) 
      return add( Message.RecipientType.BCC, emailAddress.split( "," ) : _* )

    this
  }

  def add( recipientType:Message.RecipientType, emailAddresses:String* ):Email = {
    if ( emailAddresses != null ) {
      for ( emailAddress <- emailAddresses ) {
        try {
          add( recipientType, Email.getInetAddress( emailAddress ) )
        } catch { 
          case e:AddressException => 
            e.log
        }
      }
    }

    this
  }

  private def add( recipientType:Message.RecipientType, inetAddress:InternetAddress ) : Unit = {
    recipientType match {
      case Message.RecipientType.TO => primaryRecipients += inetAddress
      case Message.RecipientType.CC => 
        if ( ccRecipients == null ) ccRecipients = ArrayBuffer[InternetAddress]()      
        ccRecipients += inetAddress
      case Message.RecipientType.BCC => 
        if ( bccRecipients == null ) bccRecipients = ArrayBuffer[InternetAddress]()
        bccRecipients += inetAddress
    }
  }

  def replyTo( _replyToEmailAddress:String ):Email = {
    try {
      replyTo = Email.getInetAddress( _replyToEmailAddress )
    } catch { 
      case ae:AddressException => 
    }
    
    this
  }

  def from(_fromEmailAddress:String):Email = {
    try {
      from = Email.getInetAddress( _fromEmailAddress )
    } catch { 
      case ae:AddressException => 
    }
    
    this
  }

  def addAttachment( _attachment:File ) = {
    if (attachments == null) 
      attachments = ArrayBuffer[File]()

    attachments += _attachment
    this
  }

  def guessContentType( f:File ):String = {
    new MimetypesFileTypeMap().getContentType( f );
  }
}

case class JavaEmail( subject:String, text:String, html:String=null ) extends Email {
  private var emailSession:Session = null;
  
  var message:MimeMessage = null

  def getMimeMessage = message

  @throws(classOf[MessagingException])
  def send:Email = {
    if ( T.session != null && T.session.isAllowingEmail ) return this
    
    compose
    
    if ( Email.enabled ) {
      Transport.send(message)
    }
    
    this
  }

  @throws(classOf[MessagingException])
  def compose:Email = {
    if ( message == null ) {
      var session:Session = getMailSession
      message = new MimeMessage( session )
    }

    //if (defaultFrom) 
    //  sender( Configs.getDefaultMailFrom() )

    if ( from == null ) 
      throw new MessagingException( "A from must be set on this email message!" )

    message.setFrom( from )
    
    if ( replyTo != null && replyTo != from ) 
      message.setReplyTo( Array[Address]( replyTo ) )
    
    if ( primaryRecipients == null ) 
      throw new MessagingException("The primary recipients must be set on this email message.")

    message.addRecipients( Message.RecipientType.TO, primaryRecipients.toArray[Address] )

    if ( ccRecipients != null ) 
      message.addRecipients( Message.RecipientType.CC, ccRecipients.toArray[Address] )

    if ( bccRecipients != null ) 
      message.addRecipients( Message.RecipientType.BCC, bccRecipients.toArray[Address] )

    if ( sendDate == null ) 
      sendDate = new Date()

    message.setSentDate( sendDate )
    
    if ( subject.notBlank ) 
      message.setSubject( subject )

    if ( text. notBlank ) {
      if ( html.notBlank ) {
        var multipart:Multipart = new MimeMultipart( "alternative" )
        var plainMessageBodyPart = new MimeBodyPart()
        plainMessageBodyPart.setContent( text,"text/plain" )
        multipart.addBodyPart( plainMessageBodyPart )
        var htmlMessageBodyPart = new MimeBodyPart()
        htmlMessageBodyPart.setContent( html, "text/html" )
        multipart.addBodyPart( htmlMessageBodyPart )
        
        if ( attachments != null ) {
          for ( attachment <- attachments ) {
            var messageBodyPart:BodyPart = new MimeBodyPart()
            var source:DataSource = new FileDataSource( attachment )
            messageBodyPart.setDataHandler( new DataHandler( source ) )
            messageBodyPart.setFileName( attachment.getName() )
            var contentType:String = guessContentType( attachment )

            if ( contentType != null ) 
              messageBodyPart.setHeader( "Content-Type", contentType )

            multipart.addBodyPart(messageBodyPart)
          }
        }
        
        message.setContent( multipart )
      } else {
        message.setText( text )
        
        if ( attachments != null ) {
          var multipart:Multipart = new MimeMultipart()

          for ( attachment <- attachments ) {
            var messageBodyPart:BodyPart = new MimeBodyPart()
            var source:DataSource = new FileDataSource( attachment )
            messageBodyPart.setDataHandler( new DataHandler( source ) )
            messageBodyPart.setFileName( attachment.getName() )
            var contentType:String = guessContentType( attachment )

            if ( contentType != null ) 
              messageBodyPart.setHeader( "Content-Type", contentType )

            multipart.addBodyPart(messageBodyPart)
          }

          message.setContent( multipart )
        }
      }
    }

    this
  }

  private def getMailSession:Session  = {
    if ( emailSession == null ) {
      val emailConfig = EmailConfig.db.findOne()
      
      if ( emailConfig == null )
        throw new RuntimeException( "Email failed because there is no emailConfig is available." );
        
      val host = emailConfig.s( 'host )
      
      if ( host.isBlank )
        throw new RuntimeException( "WARNING: host not set in emailConfig.  Sending of mail failed!" );

      var props:Properties = System.getProperties()
      props.put( "mail.smtp.host", host )
      
      val port = emailConfig.i( 'port )
      
      if ( port != 0 )
          props.put( "mail.smtp.port", port._s )
            
      val tls = emailConfig.b( 'tls )
      
      if ( tls )
          props.put( "mail.smtp.starttls.enable", "true" );
      
      val ssl = emailConfig.b( 'ssl )
      
      if ( ssl ) {
          props.put( "mail.smtp.socketFactory.class", "javax.net.ssl.SSLSocketFactory" );
          props.put( "mail.smtp.socketFactory.fallback", "false" );
      }
          
//    props.put( "mail.smtp.debug", "true" );
            
      val authUser = emailConfig.s( 'authUser )
      val authPassword = emailConfig.s( 'authPassword )
      
      if ( authUser notBlank ) {
        props.put( "mail.smtp.auth", "true" );
        props.put( "mail.smtp.user", authUser );
        props.put( "mail.smtp.password", authPassword );
        
        emailSession = Session.getDefaultInstance( props, EmailAuth( user = authUser, password = authPassword ) );
      } else {
        emailSession = Session.getDefaultInstance( props, null );
      }
    }
    
    return emailSession;
  }
}

case class EmailAuth( user:String, password:String ) extends Authenticator {
  override def getPasswordAuthentication() = { new PasswordAuthentication( user , password ) };
}

/*

Security.addProvider( new com.sun.net.ssl.internal.ssl.Provider() );



  mail.<protocol>.socketFactory.class
  mail.<protocol>.socketFactory.fallback 
  mail.<protocol>.socketFactory.port
  mail.<protocol>.timeout
  
 final String SSL_FACTORY = "javax.net.ssl.SSLSocketFactory";
  Properties props = System.getProperties();
  // IMAP provider
  props.setProperty( "mail.imap.socketFactory.class", SSL_FACTORY);
  // POP3 provider
  props.setProperty( "mail.pop3.socketFactory.class", SSL_FACTORY);
  // NNTP provider (if any)
  // props.setProperty( "mail.nntp.socketFactory.class", SSL_FACTORY);



// IMAP provider
  props.setProperty( "mail.imap.socketFactory.fallback", "false");
  // POP3 provider
  props.setProperty( "mail.pop3.socketFactory.fallback", "false");
  // NNTP provider (if any)
  // props.setProperty( "mail.nntp.socketFactory.fallback", "false");


// IMAP provider
  props.setProperty( "mail.imap.port", "993");
  props.setProperty( "mail.imap.socketFactory.port", "993");
  // POP3 provider
  props.setProperty( "mail.pop3.port", "995");
  props.setProperty( "mail.pop3.socketFactory.port", "995");
  // NNTP provider (if any)
  // props.setProperty( "mail.pop3.port", "563");
  // props.setProperty( "mail.pop3.socketFactory.port", "563");

*/
