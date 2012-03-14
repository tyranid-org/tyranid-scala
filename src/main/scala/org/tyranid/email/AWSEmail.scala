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

import com.amazonaws.AmazonClientException;
import com.amazonaws.auth.BasicAWSCredentials;
//import com.amazonaws.services.simpleemail.*;
//import com.amazonaws.services.simpleemail.model.*;

object AWSEmail {
  def apply( subject:String, text:String, to:String, from:String ) {
    AWSEmail( subject, text ).addTo( to ).from( from ).send
  }
}

case class AWSEmail( subject:String, text:String, html:String=null ) extends Email {
  @throws(classOf[MessagingException])
  override def compose:Email = {
  /*  
        
            SendEmailRequest request = new SendEmailRequest()
                .withSource("bob@example.com");
                
            List<String> toAddresses = new ArrayList<String>();
            toAddresses.add("andrew@example.com");
            Destination dest = new Destination().withToAddresses(toAddresses);
            request.setDestination(dest);
    
            Content subjContent = new Content().withData("Test of Amazon SES");
            Message msg = new Message().withSubject(subjContent);
                
                // Include a body in both text and HTML formats
                Content textContent = new Content().withData("Hello - I hope you're having a good day.");
            Content htmlContent = new Content().withData("<h1>Hello - I hope you're having a good day.</h1>");
            Body body = new Body().withHtml(htmlContent).withText(textContent);
            msg.setBody(body);
 
            request.setMessage(msg);
          
                // Set AWS access credentials
                AmazonSimpleEmailServiceClient client = 
                    new AmazonSimpleEmailServiceClient(
                        new BasicAWSCredentials(
                            "Access_key_ID_goes_here", 
                            "Secret_key_goes_here"));
                            
            // Call Amazon SES to send the message 
            try {
              client.sendEmail(request);
            } catch (AmazonClientException e) {
              System.out.println(e.getMessage());
            } catch (Exception e) {
              e.printStackTrace();
            }         
          }
  */
  
    /*
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
  */
    this
  }
  
  @throws(classOf[MessagingException])
  override def send():Email = {
    /*
    compose()
    Transport.send(message)
    
    */
    this
  }
}
