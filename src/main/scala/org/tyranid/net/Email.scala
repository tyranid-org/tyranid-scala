package org.tyranid.net

import org.tyranid.Imp._

import java.io.{ File, UnsupportedEncodingException }
import java.util.Date

import javax.activation._

import javax.mail._
import javax.mail.internet._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


//Email( subject = ...,
//       text = ... )


case class Email( subject:String, text:String ) {
  private var sender:InternetAddress = null
  private var primaryRecipients = ArrayBuffer[InternetAddress]()
  private var ccRecipients:ArrayBuffer[InternetAddress] = null
  private var bccRecipients:ArrayBuffer[InternetAddress] = null
  private var message:MimeMessage = null
  private var sendDate:Date = null
  private var attachments:ArrayBuffer[File] = null
  private var defaultFrom:Boolean = false;
  
  def getMimeMessage() : MimeMessage = {
    return message
  }

  def addTo( emailAddress:String )  = {
    if ( emailAddress.notBlank ) 
      add( Message.RecipientType.TO, emailAddress.split( "," ):_* )
    else 
      this
  }

  def addCc( emailAddress:String ):Email = {
    if ( emailAddress.notBlank ) 
      return add( Message.RecipientType.CC, emailAddress.split( "," ):_* )

    this
  }

  def addBcc( emailAddress:String ): Email = {
    if ( emailAddress.notBlank ) 
      return add( Message.RecipientType.BCC, emailAddress.split( "," ):_* )

    this
  }

  def add( recipientType:Message.RecipientType, emailAddresses:String* ):Email = {
    if ( emailAddresses != null ) {
      for ( emailAddress <- emailAddresses ) {
        try {
          add( recipientType, getInetAddress( emailAddress, null ) )
        } catch { 
          case e:AddressException => 
            log( exception = e )
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

  @throws(classOf[AddressException])
  private def getInetAddress( emailAddress:String, displayName:String ) : InternetAddress = {
    if ( displayName.notBlank ) {
      try {
        return new InternetAddress( emailAddress, displayName )
      } catch { 
        case uee:UnsupportedEncodingException => 
      }
    }

    return new InternetAddress( emailAddress )
  }

  def sender(_senderEmailAddress:String) : Email = {
    try {
      sender = getInetAddress(_senderEmailAddress, null)
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

  @throws(classOf[MessagingException])
  def send():Email = {
    compose()
    Transport.send(message)
    this
  }

  @throws(classOf[MessagingException])
  def compose():Email = {
    if ( message == null ) {
      //var session:Session = Configs.getMailSession()
      //message = new MimeMessage( session )
    }

    //if (defaultFrom) 
    //  sender( Configs.getDefaultMailFrom() )

    if (sender == null) 
      throw new MessagingException("A sender must be set on this email message.")


    message.setFrom(sender)
    message.setReplyTo( Array[Address]( sender ) )
    
    if (primaryRecipients == null) 
      throw new MessagingException("The primary recipients must be set on this email message.")


    message.addRecipients( Message.RecipientType.TO, primaryRecipients.toArray[Address] )

    if ( ccRecipients != null ) 
      message.addRecipients( Message.RecipientType.CC, ccRecipients.toArray[Address] )

    if ( bccRecipients != null ) 
      message.addRecipients( Message.RecipientType.BCC, bccRecipients.toArray[Address] )

    if ( sendDate == null ) 
      sendDate = new Date()

    message.setSentDate( sendDate )
    
    if ( subject != null ) 
      message.setSubject( subject )

    if ( text != null ) 
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

    this
  }

  private def guessContentType( f:File ):String = {
    new MimetypesFileTypeMap().getContentType( f );
  }
}

object Email {
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
