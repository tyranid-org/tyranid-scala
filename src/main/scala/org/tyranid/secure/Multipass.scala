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

package org.tyranid.secure

import java.util.{ Calendar, Date }

import java.io.{ ByteArrayInputStream, ByteArrayOutputStream, InputStream, OutputStream }
import java.security.{ InvalidAlgorithmParameterException, InvalidKeyException, NoSuchAlgorithmException }

import javax.crypto.{ Cipher, CipherOutputStream, NoSuchPaddingException }
import javax.crypto.spec.{ IvParameterSpec, SecretKeySpec }

import scala.collection.mutable

import org.apache.commons.codec.digest.DigestUtils

import org.tyranid.Imp._
import org.tyranid.math.Base64
import org.tyranid.session.Session
import org.tyranid.web.{ Weblet, WebContext }


object Multipass {

  val DefaultInitVector = "OpenSSL for Ruby".getBytes
}

case class Multipass( accountKey:String, apiKey:String, initVector:Array[Byte] = Multipass.DefaultInitVector ) {

  private val secretKeySpec = {
    val salted = apiKey + accountKey
    val hash = DigestUtils.sha1( salted )

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
    val expires = new Date().add( Calendar.MINUTE, 5 ).toUtcCalendar.toIso8601

    json( "{ \"uid\":\"" + uid +
          "\", \"expires\":\"" + expires +
          "\", \"customer_email\":\"" + email +
          "\", \"customer_name\":\"" + name + "\"" +
          tags.map( t => ", \"customer_custom_" + t._1 + "\":\"" + t._2 + "\"" ).mkString +
          " }" )
  }
}

object Multipasslet extends Weblet {
  lazy val tokener = B.assistly

  def handle( web:WebContext ) {
    val sess = Session()
    val user = sess.user

    web.path match {
    case "/multipass" | "/multipass/login" =>
      T.editing( user )

      val tags = mutable.ArrayBuffer[(String,String)]()

      val org = user.org
      if ( org != null ) {
        tags += ( "org_id" -> user.org.tid )
        tags += ( "org" -> user.org.name )
      }

      web.redirect(
        "http://volerro.assistly.com/customer/authentication/multipass/callback?multipass=" +
        tokener.props(
          user.tid, redirect = "", email = user.s( 'email ), name = user.fullName, tags:_* ) )

    case "/multipass/logout" =>
      web.res.ok
    }
  }
}

