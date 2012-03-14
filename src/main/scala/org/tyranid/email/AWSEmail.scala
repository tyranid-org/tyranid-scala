package org.tyranid.email

import java.io.{ File, UnsupportedEncodingException }
import java.util.{ Date, Properties }

import javax.activation._
import javax.mail._
import javax.mail.internet._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import org.tyranid.Imp._
import org.tyranid.db.{ DbChar, DbInt, DbPassword }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.MongoEntity
import org.tyranid.profile.User


object AwsEmail {
  def apply( subject:String, text:String, to:String, from:String ) {
    AwsEmail( subject, text ).addTo( to ).from( from ).send
  }
}

case class AwsEmail( subject:String, text:String, html:String=null ) extends Email {
  @throws(classOf[MessagingException])
  override def compose():Email = {
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

    if ( text != null ) {
      if ( html != null ) {
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
  
  @throws(classOf[MessagingException])
  override def send():Email = {
    compose()
    Transport.send(message)
    this
  }
}