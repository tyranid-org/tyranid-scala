/**
 * Copyright (c) 2008-2011 2 <http://tyranid.org>
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

package org.tyranid.text

import scala.util.matching.Regex

import org.tyranid.Imp._
import org.tyranid.time.{ Time }
import java.util.{ Date }

class StringImp( s:String ) {
	def denull = if ( s == null ) "" else s

	def splitFirst( sep:Char ) = {
		val idx = s.indexOf( sep )
		( s.substring( 0, idx ), s.substring( idx+1 ) )
	}


	/**
	 * Named this way to be similar to Lift's "encJs" method on Strings.
	 */
	def encUrl = java.net.URLEncoder.encode( s, "UTF-8" ) 
	def decUrl = java.net.URLDecoder.decode( s, "UTF-8" )
  //def decUrl = new org.apache.commons.codec.net.URLCodec( "UTF-8" ).decode( s )

  def encJson = {
    val sb = new StringBuilder
    val len = s.length

    for ( i <- 0 until len ) {
      s.charAt( i ) match {
      case '\b' => sb ++= "\\b"
      case '\f' => sb ++= "\\f"
      case '\n' => sb ++= "\\n"
      case '\r' => sb ++= "\\r"
      case '\t' => sb ++= "\\t"
      case '\\' => sb ++= "\\"
      case '"'  => sb ++= "\""
      case ch   => sb += ch
      }
    }

    sb.toString
  }

	def isBlank  = ( s == null || s.length == 0 )
	def notBlank = ( s != null && s.length >  0 )

  /**
   * Similar to Groovy's ?: (Elvis) operator.
   *
   * Example:  user.name or "Unknown"
   */
	def or( fallback:String ) = if ( isBlank ) fallback else s

  /**
   * Equivalent to:    !s.isBlank |* ...
   */
  def |*( v: => String ):String = if ( !isBlank ) v else ""

  /**
   * Example:
   *
   * "foo" |* ( "class=\"" + _ + "\"" )  becomes:  class="foo"
   * ""    |* ( "class=\"" + _ + "\"" )  becomes:  (empty string)
   */
  def |*( v: ( String ) => String ):String = v( s )

	def toXml = scala.xml.XML.loadString( s )

  def toLiftJson = _root_.net.liftweb.json.JsonParser.parse( s )

  def toJson = org.tyranid.json.Json.parse( s )

  def matches( r:Regex ) = r.pattern.matcher( s ).matches

  def word =
    if ( s == null ) ""
    else             s.trim

  def lowerWord = word.toLowerCase

  def suffix( sep:Char ) = {
    val idx = s.indexOf( sep )
    if ( idx != -1 ) s.substring( idx+1 ) else ""
  }

  /**
   * Generates a plural form of a singular word.
   */
	def plural:String = s match {
		case s if s.endsWith( "status" ) => s
		case s if s.endsWith( "s" )      => s + "es"
		case s if s.endsWith( "y" )      => s.substring( 0, s.length - 1 ) + "ies"
		case s                           => s + "s"
	  }

  /**
   * Generates a plural form of a singular word based if the passed in number is not 1.
   * i.e. 0 cats, 1 cat, 2 cats, ...
   */
	def plural( cnt:Int ):String = if ( cnt == 1 ) s else plural

  /**
   * Generates a possessive form of a singular word.
   */
	def possessive:String = s match {
		case s if s.endsWith( "s" ) => s + "'"
		case s                      => s + "'s"
	  }

	/**
 	 * Scala's StringOps defines a toBoolean(), but it is very minimal ... it only accepts "true" and "false"
 	 */
	def toLaxBoolean =
		lowerWord match {
		case ""
	     | "n" | "no"
		   | "f" | "false"
			 | "off"         => false
		case _             => true
	  }

  def isInt = s.forall( _.isDigit )

  def toLaxInt =
    if ( s.isBlank )
      0
    else
      try {
        s.toInt
      } catch {
      case e:NumberFormatException =>
        0
      }

  def toLaxDouble = if ( s.isBlank ) 0
                    else             s.toDouble

  def toLaxLong =
    if ( s.isBlank )
      0
    else
      try {
        s.toLong
      } catch {
      case e:NumberFormatException =>
        0
      }

  def toBigInt = BigInt( s )

	def uncapitalize = if ( s.length > 1 ) s.charAt( 0 ).toLower + s.substring( 1 ) else s
	
  /**
   * Used to a case-insensitive identifier from a camelcase identifier,
   * like for example a SQL database field.
   *
   * Example:   "helloThere" to "hello_there"
   */
	def camelCaseToUnderLower:String = {
	  val sb = new StringBuilder
	  var first = true
	  
	  for ( ch <- s )
	    if ( first )           { sb += ch.toLower; first = false }
	    else if ( ch.isUpper ) sb += '_' += ch.toLower
	    else                   sb += ch
	  
	  sb.toString
	}

  /**
   * Used to generate a display name from a camelcase name.
   *
   * Example:   "helloThere" to "Hello There"
   */
	def camelCaseToSpaceUpper:String = {
	  val sb = new StringBuilder
	  var first = true
	  
	  for ( ch <- s )
	    if ( first ) {
	      sb += ch.toUpper
	      first = false
      } else {
        if ( ch.isUpper ) sb += ' '
        sb += ch
      }
	  
	  var s1 = sb.toString
	  
	  for ( sub <- substitutions )
	    s1 = sub._1.replaceAllIn( s1, sub._2 )
	    
	  s1
	}

	val substitutions = Array(
    ( """\bUrl\b""".r, "URL" ),
    ( """\bId\b""".r, "ID" ),
    ( """\bUuid\b""".r, "UUID" ) )

  /**
   * Does this string represent a valid email address?
   */
  def isEmail:Boolean =
    try {
      val addr = new javax.mail.internet.InternetAddress( s )
      val idx = s.indexOf( '@' )

      idx > 0 && idx < s.length - 5
    } catch {
      case e:javax.mail.internet.AddressException => false
    }

  def isDate = Time.isDate( s )
    
  def toLaxDate:Date = Time.parse( s.trim )
  
  def toLaxDate( f:String ):Date = {
    if ( isDate ) {
      try {
        new java.text.SimpleDateFormat( f ).parse( s.trim )
      } catch {
        case e => null
      }
    } else
      null
  }
  
  // Gets the first character, null, or an empty string
  def toLaxChar() = {
    if ( s == null )
      null
    else if ( s.length > 0 )
      s.substring( 0, 1 )
    else
      s // must be empty string
  }
  
  def toLaxMoney() = {
    if ( s.isBlank )
      0.0
    else
      s.trim.replace( "$", "" ).replace( ",", "" ).toLaxDouble
  }
  
  def toLaxDateTime:Date = {
    if ( isDate )
      Time.DateTimeFormat.parse( s.trim )
    else 
      null
  }
  
  def toLaxDateTime( f:String ):Date = toLaxDateTime( f )
}

