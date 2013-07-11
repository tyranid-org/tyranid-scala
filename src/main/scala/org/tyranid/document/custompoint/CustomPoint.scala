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

package org.tyranid.document.custompoint

import java.util.{ Date, Calendar }

import scala.xml.NodeSeq

import org.tyranid.Imp._
import org.tyranid.json.Js
import org.tyranid.math.Base64
import org.tyranid.profile.User
import org.tyranid.time.Time

object CustomPoint {
  var TEST_SEED:Int = 0
  
  def cryptCustomPoint( s:String, cipherVal:Int ) = {
    val refURLChar = "0123456789abcdefghijklmnopqrstuvwxyz._~ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    val refLen = refURLChar.length 
   
    val temp = new StringBuilder()
 
    for ( count <- 0 until s.length ) {
      val tempChar = s.charAt( count )
      val conv = refURLChar.indexOf( tempChar )
     
      if ( conv != -1 ) {
        val newCipher = ( conv ^ cipherVal ) % refLen 
        val cipherChar = refURLChar.charAt( newCipher )  
        temp += ( ( cipherChar == '+' ) ? '~' | cipherChar )
      }
    }
   
    temp
  }
  
  def crypt64CustomPoint( s:String, cipherVal:Int ) = {
    val refURLChar = "0123456789+/=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    val refLen = refURLChar.length 
   
    val temp = new StringBuilder()
 
    for ( count <- 0 until s.length ) {
      val tempChar = s.charAt( count )
      val conv = refURLChar.indexOf( tempChar )
     
      if ( conv != -1 ) {
        val newCipher = ( conv + cipherVal ) % refLen 
        val cipherChar = refURLChar.charAt( newCipher )  
        temp += ( ( cipherChar == '+' ) ? '~' | cipherChar )
      }
    }
   
    temp
  }

  private def getSeed( cal:Calendar ) = {
    val time = cal.get( Calendar.HOUR_OF_DAY ) + cal.get( Calendar.MINUTE ) + cal.get( Calendar.SECOND )
    val date = ( cal.get( Calendar.MONTH ) + 1 ) + cal.get( Calendar.DAY_OF_MONTH ) + cal.get( Calendar.YEAR )
    ( ( TEST_SEED == 0 ) ? ( date + time ) | TEST_SEED )
  }
  
  def queryStringBuild( cal:Calendar, qs:String ) = {
    val origseed = "0040" + getSeed( cal )
    val seed = origseed.substring( origseed.length - 4, origseed.length )
    
    val qs64 = Base64.toString( qs.getBytes() )
    val queryStringB64URLE = crypt64CustomPoint( qs64, seed._i )
    "" + seed + queryStringB64URLE
  }
  
  def customPointUrl( user:User, accountNumber:String, username:String, password:String, customPointVendorId:String ):NodeSeq = {
    if ( customPointVendorId.isBlank )
      return NodeSeq.Empty
  
    if ( accountNumber.isBlank || username.isBlank || password.isBlank )
      return NodeSeq.Empty

    val now = new Date()
    val nowCustomPoint = now.toCustomPointFormat
    val cal = now.toCalendar()
    val seed = getSeed( cal )
    
    val queryString = "Option=6&username=" + username +
               "&password=" + cryptCustomPoint( password, seed ) +
               "&account=" + accountNumber +
               "&email=" + user.s( 'email ) +
               "&lname=" + user.s( 'lastName ) +
               "&fname=" + user.s( 'firstName ) +
               "&Module=VI&custItemNum=" + customPointVendorId +
               "&datetime=" + seed +
               "&tstamp=" + now.toCustomPointFormat
      
    <a href={ "https://custompoint.rrd.com/xs2/?mw="+ queryStringBuild( cal, queryString ) } target='_blank'>{ customPointVendorId }</a>
  }
}