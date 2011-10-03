
package org.tyranid.secure

import org.joda.time._
import org.scala_tools.time.Imports._

import java.io.{ ByteArrayInputStream, ByteArrayOutputStream, InputStream, OutputStream }
import java.security.{ InvalidAlgorithmParameterException, InvalidKeyException, NoSuchAlgorithmException }

import javax.crypto.{ Cipher, CipherOutputStream, NoSuchPaddingException }
import javax.crypto.spec.{ IvParameterSpec, SecretKeySpec }

import org.apache.commons.codec.digest.DigestUtils

import org.tyranid.Imp._
import org.tyranid.math.Base64


object Multipass {

  val DefaultInitVector = "OpenSSL for Ruby".getBytes
}

case class Multipass( accountKey:String, apiKey:String, initVector:Array[Byte] = Multipass.DefaultInitVector ) {

  private val secretKeySpec = {
    val salted = apiKey + accountKey
    val hash = DigestUtils.sha( salted )

    val saltedHash = new Array[Byte]( 16 )
    System.arraycopy( hash, 0, saltedHash, 0, 16 )

    new SecretKeySpec( saltedHash, "AES" )
  }

  private val ivSpec = new IvParameterSpec( initVector )

  private def encrypt( in:InputStream, out:OutputStream ) {
    val buf = new Array[Byte]( 1024 )
      
    val cipher = Cipher.getInstance( "AES/CBC/PKCS5Padding" )
    cipher.init( Cipher.ENCRYPT_MODE, secretKeySpec, ivSpec )
      
    val cout = new CipherOutputStream( out, cipher )
      
    var numRead = in.read( buf )
    while ( numRead >= 0 ) {
      cout.write( buf, 0, numRead )
      numRead = in.read( buf )
    }

    cout.close
  }

  def json( json:String ) = {
    val data = json.getBytes
    for ( i <- 0 until 16 )
      data( i ) = ( data( i ) ^ initVector( i ) ).toByte

    val out = new ByteArrayOutputStream
    encrypt( new ByteArrayInputStream( data ), out )
    Base64.toString( out.toByteArray )
  }    

  def props( uid:String, redirect:String, email:String, name:String, tags: (String,String)* ):String = {
    val expires = ( new Instant() + 5.minutes ).toString()

    json( "{ \"uid\":\"" + uid +
          "\", \"expires\":\"" + expires +
          "\", \"customer_email\":\"" + email +
          "\", \"customer_name\":\"" + name + "\"" +
          tags.map( t => ", \"customer_custom_" + t._1 + "\":\"" + t._2 + "\"" ).mkString +
          " }" )
  }
}

