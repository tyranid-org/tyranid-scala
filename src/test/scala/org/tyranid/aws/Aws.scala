package org.tyranid.aws

import java.io.File
import java.util.{ Date, Calendar }

import scala.xml.NodeSeq

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.tyranid.ImpT._
import org.tyranid.cloud.aws.{ Aws, S3, S3Bucket }
import org.tyranid.db.mongo.Imp._
import org.tyranid.session.ThreadData
import org.tyranid.secure.AccessType
import org.tyranid.test.db.{ Session, User }
import org.tyranid.time.Time

@RunWith(classOf[JUnitRunner])
class AwsSuite extends FunSuite {
  org.tyranid.boot.Boot.TEST = true
  org.tyranid.boot.Boot.boot
  
  test( "basic" ) {
    val dir = new File( System.getenv( "HOME" ) + "/Downloads/aws4_testsuite" )
    
    if ( dir.exists ) {
      val example = "get-vanilla-query-order-key"
      val exampleFile = new File( dir, example + ".req" )
  
      val reqLines = scala.io.Source.fromFile( exampleFile ).getLines
      
      //GET /?a=foo&b=foo http/1.1
      //Date:Mon, 09 Sep 2011 23:36:00 GMT
      //Host:host.foo.com
      
      val line1Parts = reqLines.next.split( " " )
      val method = line1Parts(0)
      val url = line1Parts(1)
      
      val qsIdx = url.indexOf( '?' )
      
      val uri = url.substring( 0, ( qsIdx == -1 ) ? 0 | qsIdx )
      val queryString = url.substring( ( qsIdx == -1 ) ? 0 | ( qsIdx + 1 ) )
      val httpVersion = line1Parts(2)
      
      val date = reqLines.next
      val host = reqLines.next
  
      println( method )
      println( uri )
      println( queryString )
      println( date )
      println( host )
      
      val canonicalRequest = Aws.canonicalRequest( "GET", uri, "a=foo&b=foo", Map( "Date" -> "Mon, 09 Sep 2011 23:36:00 GMT", "Host" -> "host.foo.com" ), null )
      
      println( "canonicalRequest:\n" + canonicalRequest )
  
      // 20120228/us-east-1/iam/aws4_request\n
      
      val credentialScope = "20110909/us-east-1/host/aws4_request"
        
      val cal = Time.createNullCalendar
      cal.set( Calendar.YEAR, 2011 )
      cal.set( Calendar.MONTH, 8 )
      cal.set( Calendar.DAY_OF_MONTH, 10 )
      cal.set( Calendar.HOUR_OF_DAY, 4 )
      cal.set( Calendar.MINUTE, 36 )
      
      println( cal.getTime )
      
      val stringToSign = Aws.stringToSign( "AWS4-HMAC-SHA256", cal.getTime(), credentialScope, canonicalRequest ) 
        
     // AWS4-HMAC-SHA256
     // 20110909T233600Z
     // 20110909/us-east-1/host/aws4_request
     // 2f23d14fe13caebf6dfda346285c6d9c14f49eaca8f5ec55c627dd7404f7a727
      
     // AWS4-HMAC-SHA256
     // 20110909T233600Z
     // 20110909/us-east-1/host/aws4_request
     // 2f23d14fe13caebf6dfda346285c6d9c14f49eaca8f5ec55c627dd7404f7a727
      println( "toSign:\n" + stringToSign )
      
  //      def getSignatureKey( key:String, dateStamp:String, regionName:String, serviceName:String ) = {
  
      val signedString = Aws.getSignatureKey( B.awsCredentials.getAWSSecretKey, "20110909", "us-east-1", "host" )
      //AWS4-HMAC-SHA256 Credential=AKIDEXAMPLE/20110909/us-east-1/host/aws4_request, SignedHeaders=date;host, Signature=
      //0dc122f3b28b831ab48ba65cb47300de53fbe91b577fe113edac383730254a3b
      //e220a8ee99f059729066fd06efe5c0f949d6aa8973360d189dd0e0eddd7a9596
      println( "signedString:\n" + signedString.toHexString )
      
      val signature = Aws.HmacSHA256( stringToSign, signedString )
      
      println( signature.toHexString )
    }
  } 
}
