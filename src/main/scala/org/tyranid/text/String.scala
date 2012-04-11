/**
 * Copyright (c) 2008-2012 2 <http://tyranid.org>
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

import java.util.Date
import java.util.regex.Pattern

import scala.util.matching.Regex
import scala.xml.{ NodeSeq, Text, Unparsed }

import org.tyranid.Imp._
import org.tyranid.http.Http
import org.tyranid.net.Uri
import org.tyranid.oauth.OAuth
import org.tyranid.time.{ Time }

object StringImp {
  val AmpersandPattern = Pattern.compile( "&" )
  val CommaPattern = Pattern.compile( "," )

  val UnicodeLeftQuote  = 8220
  val UnicodeRightQuote = 8221
}

class StringImp( s:String ) {
	def denull = if ( s == null ) "" else s

  def literal =
    if ( s == null ) "null"
    else             "\"" + s.replace( "\"", "\\\"" ) + "\""

	def splitFirst( sep:Char ) = {
		val idx = s.indexOf( sep )
    if ( idx != -1 )
      ( s.substring( 0, idx ), s.substring( idx+1 ) )
    else
      ( s, null )
	}

  /*
   * Faster than s.split( "&" )
   */
  def splitAmp = StringImp.AmpersandPattern.split( s )
  def splitComma = StringImp.CommaPattern.split( s )

  def asUrl =
    if ( s.isBlank || s.startsWith( "https://" ) || s.startsWith( "http://" ) ) s
    else                                                                        "http://" + s

	def encUrl = java.net.URLEncoder.encode( s, "UTF-8" ) 
	def decUrl = java.net.URLDecoder.decode( s, "UTF-8" )
  //def decUrl = new org.apache.commons.codec.net.URLCodec( "UTF-8" ).decode( s )

	def encOAuthUrl = OAuth.encOAuthUrl( s )
	def decOAuthUrl = OAuth.decOAuthUrl( s )

  def toPattern = Pattern.compile( s )
  def toPatternI = Pattern.compile( s, Pattern.CASE_INSENSITIVE )

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
      case '"'  => sb ++= "\\\""
      case ch   => sb += ch
      }
    }

    sb.toString
  }
  
  def encUnicode:String = {

    for ( i <- 0 until s.length ) {
      Character.codePointAt( s, i ) match {
      case StringImp.UnicodeLeftQuote | StringImp.UnicodeRightQuote =>
        val sb = new StringBuilder( s.substring( 0, i ) )

        for ( j <- i until s.length ) {
          val ch = s.charAt( j )

          Character.codePointAt( s, j ) match {
          case StringImp.UnicodeLeftQuote | StringImp.UnicodeRightQuote =>
            sb ++= "&#" ++= Character.codePointAt( s, j ).toString += ';'

          case _ =>
            sb += ch
          }
        }

        return sb.toString

      case _ =>
      }
    }

    s
  }

  def encRegex = s.replace( ".", "\\." ).replace( "@", "\\@" ).replace( "+", "\\+" )

  def toUrl = new java.net.URL( Uri.completeUri( s ) )

  def toPhoneMask:String = {
    var offset = s.length match {
                          case 10 => 0
                          case 11 => 1
                          case _ => -1
                 }
    
    if ( offset == -1 )
      return null
      
    return "(" + s.slice( offset,3+offset ) + ") " + s.slice( 3+offset, 6+offset ) + "-" + s.slice( 6+offset, 10+offset ) 
  }

  def toOnlyNumbers:String = {
    if ( s == null )
      return null
     
    val sb = new StringBuilder
    
    for ( i <- 0 until s.length )
      if ( s( i ).isDigit )
        sb ++= s( i ).toString
    
    return sb.toString
  }
  
  def toHtmlPreserveWhitespace:NodeSeq =
    s.replace( "\n \n", "\n\n" ).split( "\n\n" ).map( para => <p>{ Unparsed( para.replace( "\r\n", "<br/>" ).replace( "\n", "<br/>" ).replace( "\r", "<br/>" ) ) }</p> ).toSeq

	def isBlank  = ( s == null || s.length == 0 )
	def notBlank = ( s != null && s.length >  0 )
	def nonBlank = ( s != null && s.length >  0 )

  def allBlank:Boolean = {
    if ( s != null )
      for ( i <- 0 until s.length )
        if ( !s( i ).isWhitespace )
          return false

    true
  }

  def notAllBlank:Boolean = {
    if ( s != null )
      for ( i <- 0 until s.length )
        if ( !s( i ).isWhitespace )
          return true

    false
  }

  /*
   * Tokenizes the string into an array of tokens.  For example:
   *
   * "  blue green,purple    violet" becomes [ "blue", "green", "purple", "violet" ]
   */
  def tokenize:Array[String] = {

    val l = s.length
    val b = new collection.mutable.ArrayBuffer[String]( 8 )
    val sb = new StringBuilder

    for ( i <- 0 until l ) {
      val ch = s.charAt( i )

      if ( ch.isWhitespace || ch == ',' || ch == ';' ) {
        if ( sb.nonEmpty ) {
          b += sb.toString
          sb.clear
        }
      } else {
        sb += ch
      }
    }

    if ( sb.nonEmpty ) {
      b += sb.toString
      sb.clear
    }

    Array[String]( b:_* )
  }

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

  def toNodeSeq = if ( s != null ) Text( s ) else NodeSeq.Empty

  //def toJson = org.tyranid.json.Json.parse( s )

  def parseJson = org.tyranid.json.JsonDecoder( s )
  def parseJsonObject = parseJson.as[ObjectMap]
  def parseJsonArray  = parseJson.as[Array[Any]]

  def matches( r:Regex ) = r.pattern.matcher( s ).matches

  def word =
    if ( s == null ) ""
    else             s.trim

  def lowerWord = word.toLowerCase

  def suffix( sep:Char ) = {
    val idx = s.lastIndexOf( sep )
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

  def summarize( maxLength:Int = 40 ) =
    if ( s != null && s.length > maxLength )
      s.substring( 0, maxLength - 3 ) + "..."
    else
      s

	/**
 	 * Scala's StringOps defines a toBoolean(), but it is very minimal ... it only accepts "true" and "false"
 	 */
	def toLaxBoolean =
		lowerWord match {
		case ""
       | "0"
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
        s.replaceAll( ",", "" ).replaceAll( " ", "" ).toInt
      } catch {
      case e:NumberFormatException =>
        0
      }

  def toLaxDouble = 
    if ( s.isBlank )
      0
    else
      try {
        s.replaceAll( ",", "" ).replaceAll( " ", "" ).toDouble
      } catch {
      case e:NumberFormatException =>
        0
      }

  def toLaxLong =
    if ( s.isBlank )
      0
    else
      try {
        s.replaceAll( ",", "" ).replaceAll( " ", "" ).toLong
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


  /*
   * * *   Date / Time
   */

  def toDate( format:String ):Date =
    try {
      new java.text.SimpleDateFormat( format ).parse( s.trim )
    } catch {
      case e => null
    }
  
  def parseCalendar( dateOnly:Boolean = false, userTime:Boolean = false ) = {
    // TODO:  share time parser on local thread or something?
    new org.tyranid.time.TimeParser().parse( s, dateOnly = dateOnly, forceUserTime = userTime )
  }

  def parseDate( dateOnly:Boolean = false, userTime:Boolean = false ) = parseCalendar( dateOnly, userTime ).getTime

  /**
   * This method does all the work of parseDate, so it is slow.  Only use if you don't need the date value.
   */
  def isDate( dateOnly:Boolean = false ):Boolean = parseCalendar( dateOnly = dateOnly ) != null
 
  /**
   * Warning!  If the date is invalid, null is returned.  Use parseCalendar() and look for a ParseException with a
   *           description of any parsing errors if user error reporting is needed.
   */
  def toLaxDate:Date =
    try {
      parseDate( dateOnly = true )
    } catch {
    case p:java.text.ParseException =>
      null
    }
  
  /**
   * Warning!  If the date is invalid, null is returned.  Use parseCalendar() and look for a ParseException with a
   *           description of any parsing errors if user error reporting is needed.
   */
  def toLaxDateTime:Date = {
    try {
      parseDate()
    } catch {
    case p:java.text.ParseException =>
      null
    }
  }

  def removeSpecialCharacters = {
   val sb = new StringBuilder()
   
   s.foreach( c =>
      if ( ( c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '.' || c == '_' )
         sb.append( c ) )
  
   sb.toString()
  }
  
  /*
   * * *   HTTP / URLs
   */

  def GET( query:collection.Map[String,String] = null, headers:collection.Map[String,String] = null ) =
    Http.GET( s, query = query, headers )

  def POST( form:collection.Map[String,String] = null, content:String = null, contentType:String = null, headers:collection.Map[String,String] = null ) =
    Http.POST( s, content, form, contentType, headers )

  def DELETE( query:collection.Map[String,String] = null ) =
    Http.DELETE( s, query = query )
}

