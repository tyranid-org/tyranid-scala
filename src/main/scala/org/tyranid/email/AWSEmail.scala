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
    if ( currentCount > 5 && System.currentTimeMillis - lastSent > 1000 ) 
      Thread.sleep( 1000 )

    if ( currentCount > 4 )
      currentCount = 1
    else 
      currentCount = currentCount + 1 

    AWSEmail.lastSent = System.currentTimeMillis
  }
}

case class AWSEmail( subject:String, text:String, html:String=null ) extends Email {
  var request:SendEmailRequest = null
  
  @throws(classOf[MessagingException])
  override def compose:Email = {
    //if (defaultFrom) 
    //  sender( Configs.getDefaultMailFrom() )

    if ( from == null ) 
      throw new MessagingException( "A from must be set on this email message!" )
    
    request = new SendEmailRequest().withSource( from.getAddress() )
                
    if ( replyTo != null && replyTo != from ) 
      request.setReplyToAddresses( java.util.Arrays.asList( Array[String]( replyTo.getAddress() ) ).as[java.util.List[String]] )
      
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
    request.setMessage( msg )
    
    this
  }
  
  @throws(classOf[MessagingException])
  override def send():Email = {
    compose
    
    AWSEmail.throttle
      
    new AmazonSimpleEmailServiceClient( B.awsCredentials ).sendEmail( request )
    this
  }
}
