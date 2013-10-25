/**
 * Copyright (c) 2008-2013 2 <http://tyranid.org>
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

import java.io.FileWriter
import java.net.URL
import java.util.Date
import java.util.regex.Pattern

import scala.util.matching.Regex
import scala.xml.{ NodeSeq, Text, Unparsed }

import org.mindrot.jbcrypt.BCrypt

import org.tyranid.Imp._
import org.tyranid.http.Http
import org.tyranid.net.Uri
import org.tyranid.oauth.OAuth
import org.tyranid.time.{ Time }

object StringImp {
  val AmpersandPattern = Pattern.compile( "&" )
  val CommaPattern     = Pattern.compile( "," )
  val PipePattern      = Pattern.compile( "\\|" )

  val UnicodeLeftQuote  = 8220
  val UnicodeRightQuote = 8221
  
  val scriptPattern = Pattern.compile("<script>(.*?)</script>", Pattern.CASE_INSENSITIVE)  
  val srcPattern = Pattern.compile("src[\r\n]*=[\r\n]*\\\'(.*?)\\\'", Pattern.CASE_INSENSITIVE | Pattern.MULTILINE | Pattern.DOTALL)
  val lonesomePattern = Pattern.compile("</script>", Pattern.CASE_INSENSITIVE)
  val lonesome2Pattern = Pattern.compile("<script(.*?)>", Pattern.CASE_INSENSITIVE | Pattern.MULTILINE | Pattern.DOTALL)
  val evalPattern = Pattern.compile("eval\\((.*?)\\)", Pattern.CASE_INSENSITIVE | Pattern.MULTILINE | Pattern.DOTALL)
  val expressionPattern = Pattern.compile("expression\\((.*?)\\)", Pattern.CASE_INSENSITIVE | Pattern.MULTILINE | Pattern.DOTALL)
  val javascriptPattern = Pattern.compile("javascript:", Pattern.CASE_INSENSITIVE)
  val vbscriptPattern = Pattern.compile("vbscript:", Pattern.CASE_INSENSITIVE)
  val onloadPattern = Pattern.compile("onload(.*?)=", Pattern.CASE_INSENSITIVE | Pattern.MULTILINE | Pattern.DOTALL)

  val urlPattern = Pattern.compile( "(https?:\\/\\/[^\\s<]+)", Pattern.CASE_INSENSITIVE)
}

class StringImp( s:String ) extends Serializable {
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

	def removeAllChars( chars:String ) = {
	  var newS = s
	  
	  chars.foreach( c => newS = newS.replace( c._s, "" ) )
	    
	  newS
	}
	
	def safeIdPart = ( s == null ) ? null | s.removeAllChars( "@'().&!#$%^*+={}|\\][:;\"<>/?~`" ).replace( ",", "_" ).replace( " ", "_" ).replace( "__", "_" ).toLowerCase
	
  /*
   * Faster than s.split( "&" )
   */
  def splitAmp   = StringImp.AmpersandPattern.split( s )
  def splitComma = StringImp.CommaPattern.split( s )
  def splitPipe  = StringImp.PipePattern.split( s )

  def asUrl =
    if ( s.isBlank || s.startsWith( "https://" ) || s.startsWith( "http://" ) ) s
    else                                                            "http://" + s

  def encCsv = s.replace( "\"", "\\\"" )
  
	def encUrl = ( s == null ) ? null | java.net.URLEncoder.encode( s, "UTF-8" ) 
	def decUrl = ( s == null ) ? null | java.net.URLDecoder.decode( s, "UTF-8" )
  //def decUrl = new org.apache.commons.codec.net.URLCodec( "UTF-8" ).decode( s )

	def encOAuthUrl = OAuth.encOAuthUrl( s )
	def decOAuthUrl = OAuth.decOAuthUrl( s )

  def toPattern = Pattern.compile( s )
  def toPatternI = Pattern.compile( s, Pattern.CASE_INSENSITIVE )

  def stripTags( tagName:String ) =
    ( "(?s)<" + tagName + ".*?(/>|</" + tagName + ">)" ).toPatternI.matcher( s ).replaceAll( "" )
	
  def noUtf8Pattern = Pattern.compile("[^\\x00-\\x7F]", Pattern.UNICODE_CASE | Pattern.CANON_EQ | Pattern.CASE_INSENSITIVE)
  
  def stripNonUtf8 = ( s == null ) ? s | noUtf8Pattern.matcher( s ).replaceAll( " " )
  
  def pad( minLen:Int, c:Char = '0' ) = {
    if ( s == null || s.length >= minLen ) {
      s
    } else {
      val sb = new StringBuilder( s )
      
      for ( i <- 0 until ( minLen - s.length ) )
        sb.insert( 0, c )
        
      sb._s
    }
  }
	
	def every( c:Char, where:Int ) = {
    if ( s == null ) {
      s
    } else {
  	  val sb = new StringBuilder( s )
  	  var idx = s.length - where
  
  	  while ( idx > 0 ) {
  	    sb.insert( idx, c )
  	    idx = idx - where
  	  }
  	  
  	  sb._s
    }
	}
	
  def urlify = {
    val nodes = scala.collection.mutable.Buffer[NodeSeq]()
    val matcher = StringImp.urlPattern.matcher( s )
    
    var i = 0
    
    while ( matcher.find ) {
      val grp = matcher.group
      
      val mi = matcher.start
      
      if ( mi > i ) 
        nodes += Text( s.substring( i, mi ) )
        
      nodes += <a target="_blank" class="urlify" href={ grp }>{ grp }</a> // wrap this grp with the <a>
      
      i = matcher.end
    }
    
    if ( i < s.length )
      nodes += Text( s.substring( i ) )
      
    nodes.flatten
  }  
      
  def urlifyAsString = {
    var sb = new StringBuilder
    val matcher = StringImp.urlPattern.matcher( s )
    
    var i = 0
    
    while ( matcher.find ) {
      val grp = matcher.group
      
      val mi = matcher.start
      
      if ( mi > i ) 
        sb ++= s.substring( i, mi )

      if ( mi > 6 && s.substring( mi - 6, mi ) == "href=\"" )
        sb ++= grp
      else
        sb ++= "<a target=\"_blank\" class=\"urlify\" href=\"" ++= grp ++= "\">" ++= grp ++= "</a>" // wrap this grp with the <a>

      i = matcher.end
    }
    
    if ( i < s.length )
      sb ++= s.substring( i )

    sb.toString
  }  
      
   // }Unparsed( urlPattern.matcher( s ).replaceAll( "<a target='_blank' class='urlify' href='$1'>$1</a>" ) )
//    [ Text( 'ksd' ), Unparsed( "<a ....>" + baseText + "</a>" ) ]
  
	def generateFilename( extension:String ) = {
	  if ( s != null ) {
	    val ext = extension.isBlank ? s.suffix( '.' ).toLowerCase | extension
      val urlParts = s.split( "/" )
      val firstPart = urlParts(2)
      val lastPart = urlParts(urlParts.length - 1 )
      val firstParts = firstPart.split( "\\." )
      
      val name = 
        if ( firstParts.length == 0 )
          ""
        else if ( firstParts.length == 1 || firstParts.length == 2 || ( firstParts.length == 3 && firstParts(0) != "www" ) )
          firstParts(0)
        else 
          firstParts( firstParts.length - 2 )
        
      val suffix = 
        if ( lastPart != firstPart ) {
          val lastParts = lastPart.split( "\\." )
          
          if ( lastParts.length == 0 ) {
            "_html"
          } else if ( lastParts.length == 1 ) {
            "_" + lastParts( 0 )
          } else {
            "_" + lastParts( lastParts.length - 2 )
          }
        } else {
          "_html"
        }
      
      name + suffix + "." + ext
	  } else {
	    s
	  }
  }
	
  def stripXss:String = {
    if ( s == null )
      return s
      
    // NOTE: It's highly recommended to use the ESAPI library and uncomment the following line to
    // avoid encoded attacks.
    // value = ESAPI.encoder().canonicalize(value);

    // Avoid null characters
    var value = s.replaceAll("", "")

    // Avoid anything between iframe tags
    value = value.stripTags( "iframe" )
    
    // Avoid anything between script tags
    value = StringImp.scriptPattern.matcher(value).replaceAll("")

    // Avoid anything in a src='...' type of expression
    value = StringImp.srcPattern.matcher(value).replaceAll("")

    // Remove any lonesome </script> tag
    value = StringImp.lonesomePattern.matcher(value).replaceAll("")

    // Remove any lonesome <script ...> tag
    value = StringImp.lonesome2Pattern.matcher(value).replaceAll("")

    // Avoid eval(...) expressions
    value = StringImp.evalPattern.matcher(value).replaceAll("")

    // Avoid expression(...) expressions
    value = StringImp.expressionPattern.matcher(value).replaceAll("")

    // Avoid javascript:... expressions
    value = StringImp.javascriptPattern.matcher(value).replaceAll("")

    // Avoid vbscript:... expressions
    value = StringImp.vbscriptPattern.matcher(value).replaceAll("")

    // Avoid onload= expressions
    value = StringImp.onloadPattern.matcher(value).replaceAll("")
    
    value
  }

  def unquote:String = {
    val l = s.length

    if ( l >= 2 ) {
      val first = s.head
      val last = s.last

      if (   ( first == StringImp.UnicodeLeftQuote && last == StringImp.UnicodeRightQuote )
          || ( first == '"'                        && last == '"'               )
          || ( first == '\''                       && last == '\''              ) )
        return s.substring( 1, l - 1 ).unquote
    }

    s
  }
    
  def encJson = {
    if ( s != null ) {
      val sb = new StringBuilder
      
      for ( i <- 0 until s.length ) {
        s.charAt( i ) match {
        case '\b' => sb ++= "\\b"
        case '\f' => sb ++= "\\f"
        case '\n' => sb ++= "\\n"
        case '\r' => sb ++= "\\r"
        case '\t' => sb ++= "\\t"
        case '\\' => sb ++= "\\\\"
        case '"'  => sb ++= "\\\""
        case '\'' => sb ++= "\\u0027"
        case ch   => sb += ch
        }
      }
  
      sb.toString
    } else {
      null
    }
  }
  
  def encBase64 = ( s == null ) ? null | new String( org.tyranid.math.Base64.toBytes( s ) )
  
  def encUnicode:String = {

    for ( i <- 0 until s.length ) {
      Character.codePointAt( s, i ) match {
      // TODO:  not sure about this > 127 algorithm ...
      case cp if cp > 127 =>
        val sb = new StringBuilder( s.substring( 0, i ) )

        for ( j <- i until s.length ) {
          val ch = s.charAt( j )

          Character.codePointAt( s, j ) match {
          case cp if cp > 127 =>
            sb ++= "&#" ++= cp.toString += ';'

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

  def encRegex = s.replace( ".", "\\." ).replace( "@", "\\@" ).replace( "+", "\\+" ).replace( "{", "\\{" ).replace( "^", "\\^" ).replace( "$", "\\$" ).replace( "*", "\\*" )

  def toUrl =
    if ( s != null && s.notBlank ) {
      try {
        new java.net.URL( Uri.completeUri( s ) )
      } catch {
        case e:Exception =>
          e.printStackTrace
          null
      }
    } else { 
      null
    }
  
  def containsIgnoreCase( v:String ) = 
    s != null && v.notBlank && s.toLowerCase.contains( v.toLowerCase )

  def endsWithIgnoreCase( v:String ) = 
    s != null && v.notBlank && s.toLowerCase.endsWith( v.toLowerCase )
    
  def safeFilename = s.replaceAll( " ", "_" ).replaceAll( "/", "" ).replaceAll( "\\\\", "" ).replaceAll( "\\?", "" ).replaceAll( "\\*", "" )
  
  /*
   * This should not throw any exceptions.  If the URL is invalid in any way, returns null.
   */
  def safeUrl( base:URL = null ):URL =
    try {
      if ( s.isBlank )
        null
      else if ( base != null )
        new URL( base, s )
      else
        new URL( s )
    } catch {
    case e:java.net.MalformedURLException =>
      null
    }

  // I want just the name of the file: https://a.b.c/d/e?f=g, I want e
  def getFilenameFromUrl:String = {
    val end:String = ( s == null ) ? null | ( s.suffix( '/' ) or s )
    val preEnd = end.prefix( '?' )
    
    preEnd.notBlank ? preEnd | end
  }

  def encHtml = s.replace( "<", "&lt;" ).replace( ">", "&gt;" )

  def toHtmlPreserveWhitespace:NodeSeq =
    // TODO:  need to escape javascript in this
    s.encUnicode.replace( "\n \n", "\n\n" ).split( "\n\n" ).map( para => <p>{ Unparsed( para.replace( "\r\n", "<br/>" ).replace( "\n", "<br/>" ).replace( "\r", "<br/>" ) ) }</p> ).toSeq

  def toHtmlPreserveWhitespaceCompact:NodeSeq =
    // TODO:  need to escape javascript in this
    Unparsed( s.trim.encUnicode.replace( "\n \n", "\n\n" ).replace( "\r\n", "<br/>" ).replace( "\n", "<br/>" ).replace( "\r", "<br/>" ) )

	def isBlank  = ( s == null || s.length == 0 )
	def notBlank = ( s != null && s.length >  0 )
	def nonBlank = ( s != null && s.length >  0 )

  def blankOr( v:String ) = isBlank || s == v;

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

  def count( ch:Char ) = s.foldLeft(0)( ( sum, c ) => sum + ( ( c == ch ) ? 1 | 0 ) )
  
	def toXml = {
    val xmlInstructionEnd = s.indexOf( "?>" )
    ( xmlInstructionEnd == -1 ) ? scala.xml.XML.loadString( s ) | scala.xml.XML.loadString( s.substring( xmlInstructionEnd + 2 ) ) 
  }
	
  def toNodeSeq = if ( s != null ) Text( s ) else NodeSeq.Empty

  def parseJson       = if ( s.notBlank ) org.tyranid.json.JsonDecoder( s )
                        else              null
  def parseJsonObject = parseJson.as[ObjectMap]
  def parseJsonArray  = parseJson.as[Array[Any]]

  def parseHtml = org.tyranid.web.Html( s )


  def matches( r:Regex ) = r.pattern.matcher( s ).matches

  def word =
    if ( s == null ) ""
    else             s.trim

  def lowerWord = word.toLowerCase

  def prefix( sep:Char ) = s.reverse.firstSuffix( sep ).reverse
	
  def firstSuffix( sep:Char ) = {
    val idx = s.indexOf( sep )
    if ( idx != -1 ) s.substring( idx+1 ) else ""
  }
	
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


  def isUrl:Boolean = StringImp.urlPattern.matcher( s ).matches

  /**
   * Does this string represent a valid email address?
   */
  def isEmail:Boolean =
    try {
      // for some reason this was occassionally getting past the InternetAddress check below, but not always
      if ( s.endsWith( "." ) )
        return false

      val addr = new javax.mail.internet.InternetAddress( s )
      val idx = s.indexOf( '@' )

      idx > 0 && idx < s.length - 5
    } catch {
      case e:javax.mail.internet.AddressException => false
    }

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

  def toWebsite:String = {
    if ( s == null || s.isBlank )
      return s
     
    if ( s.toLowerCase.startsWith( "http" ) )
      return s
      
    return "http://" + s
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

  def toOnlyNumbers:String = {
    if ( s == null )
      return null
     
    val sb = new StringBuilder
    
    for ( i <- 0 until s.length )
      if ( s( i ).isDigit )
        sb += s( i )
    
    sb.toString
  }
  
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


  /*
   * * *   Identifiers & Labels
   */

  def toIdentifier = {
    val sb = new StringBuilder

    for ( c <- s )
      c match {
      case c if Character.isJavaIdentifierStart( c ) =>
        sb += c
    
      case c if Character.isJavaIdentifierPart( c ) =>
        if ( sb.isEmpty )
          sb += '_'

        sb += c

      case ' ' | '-' =>
        sb += '_'
    
      case _ =>
      }
  
    sb.toString
  }

	def uncapitalize = if ( s.length > 1 ) s.charAt( 0 ).toLower + s.substring( 1 ) else s

  def initials = {
    var sb = new StringBuilder

    for ( i <- 0 until s.length )
      if ( i == 0 || s.charAt( i - 1 ) == ' ' )
        sb += s.charAt( i )

    sb.toString
  }
	
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


  /*
   * * *   Date / Time
   */

  def toDate( format:String ):Date =
    try {
      new java.text.SimpleDateFormat( format ).parse( s.trim )
    } catch {
      case e:Throwable => null
    }
  
  def parseCalendar( dateOnly:Boolean = false, userTime:Boolean = false ) = {
    // TODO:  share time parser on local thread or something?
    new org.tyranid.time.TimeParser().parse( s, dateOnly = dateOnly, forceUserTime = userTime )
  }

  def parseDate( dateOnly:Boolean = false, userTime:Boolean = false ) =
    if ( s.allBlank ) null
    else              parseCalendar( dateOnly, userTime ).getTime

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

  /**
   * Warning!  If the date is invalid, null is returned.  Use parseCalendar() and look for a ParseException with a
   *           description of any parsing errors if user error reporting is needed.
   */
  def toLaxUserDate:Date =
    try {
      parseDate( dateOnly = true, userTime = true )
    } catch {
    case p:java.text.ParseException =>
      null
    }
  
  /**
   * Warning!  If the date is invalid, null is returned.  Use parseCalendar() and look for a ParseException with a
   *           description of any parsing errors if user error reporting is needed.
   */
  def toLaxUserDateTime:Date = {
    try {
      parseDate( userTime = true )
    } catch {
    case p:java.text.ParseException =>
      null
    }
  }

  // This assumes that the string is a Tid
  
  def isMe( u:org.tyranid.profile.User = T.user ) = u.tid == s || u.org.tid == s
  def notMe = !isMe()
  
  /*
   * * *   HTTP / URLs
   */

  def GET( query:collection.Map[String,String] = null, headers:collection.Map[String,String] = null ) =
    Http.GET( Uri.completeUri( s ), query = query, headers = headers )

  def POST( form:collection.Map[String,String] = null, content:String = null, contentType:String = null, headers:collection.Map[String,String] = null ) =
    Http.POST( s, content, form, contentType, headers )

  def PUT( form:collection.Map[String,String] = null, content:String = null, contentType:String = null, headers:collection.Map[String,String] = null ) =
    Http.PUT( s, content, form, contentType, headers )

  def DELETE( query:collection.Map[String,String] = null ) =
    Http.DELETE( s, query = query )
    
  def shash( salt:String = BCrypt.gensalt() ):String = {
    if ( s == null )
      return null
      
    BCrypt.hashpw( s, salt )
  }

  def checkShash( shash:String ):Boolean = {
    if ( s == null )
      return false
      
    BCrypt.checkpw( s, shash )
  }
  
  def toFile( filename:String ) {
    var out:FileWriter = null
    
    try {
      out = new FileWriter( filename )
      out.write( s )
    } finally {
      if ( out != null )
        out.close
    }
  }
}

