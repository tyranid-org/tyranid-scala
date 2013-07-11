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

package org.tyranid.http

import scala.collection.mutable
import scala.xml.{ NodeSeq, Unparsed }

import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.db.{ DbBoolean, DbChar, DbIntSerial, DbUrl }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ MongoEntity, MongoRecord }
import org.tyranid.db.meta.AutoIncrement

/*
 { "agent_type":"Browser",
   "agent_name":"Opera",
   "agent_version":"9.70",
   "os_type":"Linux",
   "os_name":"Linux",
   "os_versionName":"",
   "os_versionNumber":"",
   "os_producer":"",
   "os_producerURL":"",
   "linux_distibution":"Null",
   "agent_language":"English - United States",
   "agent_languageTag":"en-us"
 }

*/

// Converted from: https://code.google.com/p/user-agent-parser-java/
class UserAgentParseException( message:String ) extends RuntimeException

case class UserAgentDetails( browserName:String, browserVersion:String, browserComments:String ) {}

class UserAgentParser( ua:String ) {
  var userAgentString = ua
  var browserName:String = null
  var browserVersion:String = null
  var browserOperatingSystem:String = null
  
  val parsedBrowsers = mutable.ArrayBuffer[UserAgentDetails]()
  
  val pattern = "([^/\\s]*)(/([^\\s]*))?(\\s*\\[[a-zA-Z][a-zA-Z]\\])?\\s*(\\((([^()]|(\\([^()]*\\)))*)\\))?\\s*".toPattern

  val matcher = pattern.matcher( userAgentString )

  while ( matcher.find() ) {
    val nextBrowserName = matcher.group(1)
    val nextBrowserVersion = matcher.group(3)
    var nextBrowserComments:String = null
    
    if ( matcher.groupCount() >= 6 )
      nextBrowserComments = matcher.group(6)
    
    parsedBrowsers += UserAgentDetails( nextBrowserName, nextBrowserVersion, nextBrowserComments )
  }
  
  if ( parsedBrowsers.length > 0) {
    processBrowserDetails()
  } else {
    throw new UserAgentParseException("Unable to parse user agent string: " + userAgentString)
  }

  def processBrowserDetails() {
    val browserNameAndVersion = extractBrowserNameAndVersion()
    browserName = browserNameAndVersion(0)
    browserVersion = browserNameAndVersion(1)
    browserOperatingSystem = extractOperatingSystem( parsedBrowsers(0).browserComments )
  }

  /**
   * Iterates through all component browser details to try and find the
   * canonical browser name and version.
   *
   * @return a string array with browser name in element 0 and browser
   * version in element 1. Null can be present in either or both.
   */
  def extractBrowserNameAndVersion():Array[String] = {

    val knownBrowsers = Array( "firefox", "netscape", "chrome", "safari", "camino", "mosaic", "opera", "galeon" )
      

    for ( nextBrowser <- parsedBrowsers )
      for ( nextKnown <- knownBrowsers )
        if ( nextBrowser.browserName.toLowerCase().startsWith(nextKnown ) )
          return Array( nextBrowser.browserName, nextBrowser.browserVersion )
      
    val firstAgent = parsedBrowsers(0)
    
    if ( firstAgent.browserName.toLowerCase().startsWith("mozilla" ) ) {
      if (firstAgent.browserComments.notBlank ) {
        val comments = firstAgent.browserComments.split(";")
        
        if ( comments.length > 2 && comments(0).toLowerCase().startsWith("compatible") ) {
          val realBrowserWithVersion = comments(1).trim()
          val firstSpace = realBrowserWithVersion.indexOf(' ')
          val firstSlash = realBrowserWithVersion.indexOf('/')
          
          if ( ( firstSlash > -1 && firstSpace > -1 ) || ( firstSlash > -1 && firstSpace == -1 ) ) {
            // we have slash and space, or just a slash, so let's choose slash for the split
            return Array( realBrowserWithVersion.substring( 0, firstSlash ), realBrowserWithVersion.substring( firstSlash + 1 ) )
          } 
          
          if ( firstSpace > -1 )
            return Array( realBrowserWithVersion.substring(0, firstSpace), realBrowserWithVersion.substring(firstSpace+1) )
         
          // out of ideas for version, or no version supplied
          return Array( realBrowserWithVersion, null )          
        }
      }

      // Looks like a *real* Mozilla :-)
      if ( firstAgent.browserVersion._d < 5.0)
        return Array( "Netscape", firstAgent.browserVersion )
      
      // TODO: get version from comment string
      val bc = firstAgent.browserComments
      val commentParts:Array[String] = bc.isBlank ? null | bc.split(";")
      val comment = ( commentParts == null || commentParts.length < 2 ) ? "" | commentParts(0)
      return Array( "Mozilla", comment.isBlank ? "" | comment.trim() )
    }
    
    return Array( firstAgent.browserName, firstAgent.browserVersion )
  }

  /**
   * Extracts the operating system from the browser comments.
   *
   * @param comments the comment string afer the browser version
   * @return a string representing the operating system
   */
  
  def extractOperatingSystem( comments:String ):String = {
    if ( comments.isBlank )
      return null
     
    val knownOS = Array( "win", "linux", "mac", "freebsd", "netbsd", "openbsd", "sunos", "amiga", "beos", "irix", "os/2", "warp", "iphone" )
    val osDetails = mutable.ArrayBuffer[String]()
    
    val parts = comments.split(";")
    
    for ( comment <- parts ) {
      val lowerComment = comment.toLowerCase().trim()
      
      for ( os <- knownOS )
        if ( lowerComment.startsWith( os ) )
          osDetails += comment.trim()
    }
    
    osDetails.length match {
      case 0 => return null
      case 1 => return osDetails(0)
      case _ => return osDetails(0) // need to parse more stuff here
    }
  }
}

object UserAgent extends MongoEntity( tid = "a0Dt" ) {
  type RecType = UserAgent
  override def convert( obj:DBObject, parent:MongoRecord ) = new UserAgent( obj, parent )


  "_id"               is DbIntSerial   is 'id;
  "ua"                is DbChar(256)   is 'label as "User Agent";
  "bot"               is DbBoolean     ;
  "agentType"         is DbChar(64)    ;
  "agentName"         is DbChar(64)    ;
  "agentVersion"      is DbChar(64)    ;
  "osType"            is DbChar(64)    ;
  "osName"            is DbChar(64)    ;
  "osVersionName"     is DbChar(64)    ;
  "osVersionNumber"   is DbChar(64)    ;
  "osProducer"        is DbChar(64)    ;
  "osProducerUrl"     is DbUrl         ;
  "linuxDistribution" is DbChar(64)    ;
  "agentLanguage"     is DbChar(64)    ;
  "agentLanguageTag"  is DbChar(64)    ;

  private val idByUa = mutable.HashMap[String,Int]()
  private val uaById = mutable.HashMap[Int,String]()

  def uaFor( id:Int ):String = synchronized {
    uaById.getOrElseUpdate( id, {
      db.findOne( Mobj( "_id" -> id ) ) match {
      case null => "unknown"
      case to   => to.s( "ua" )
      }
    } )
  }

  def idFor( ua:String ):Int = synchronized {
    idByUa.getOrElseUpdate( ua, {
      db.findOne( Mobj( "ua" -> ua ) ) match {
      case null =>
        val id = AutoIncrement( "userAgent" )
        db.save( Mobj( "_id" -> id, "ua" -> ua ) )
        id

      case to =>
        to.i( "_id" )
      }
    } )
  }

  lazy val system = UserAgent.getById( idFor( "Volerro System" ) )
  
  @volatile var serviceFailure = false
}

class UserAgent( obj:DBObject, parent:MongoRecord ) extends MongoRecord( UserAgent.makeView, obj, parent ) {
  def updateIfNeeded =
    if ( !has( 'agentName ) )
      update
      
  def getAgentName( ua:String, agentName:String ):String = {
    if ( agentName == "unknown" ) {
      if ( ua startsWith "AdsBot-Google" )
        return "AdsBot-Google"
      
      if ( ua contains "AhrefsBot" )
        return "AhrefsBot"
      
      if ( ua startsWith "ClickTale bot" )
        return "ClickTale bot"
        
      if ( ua startsWith "LinkedInBot" )
        return "LinkedInBot"
        
      if ( ua startsWith "Jakarta Commons" )
        return "Jakarta Commons"
        
      if ( ua startsWith "Plesk" )
        return "Plesk"
        
      if ( ua startsWith "SkimBot" )
        return "SkimBot"
      if ( ua startsWith "ZmEu" )
        return "ZmEu"
          
      if ( ua startsWith "Morfeus Fucking Scanner" ) 
        return "Morfeus Fucking Scanner"
    }
  
    return agentName
  }
  
  def update = {
    val ua = s( 'ua )
    
    if ( !UserAgent.serviceFailure ) {
      try {
        val json = ( "http://useragentstring.com?uas=" + ua.encUrl + "&getJSON=all" ).GET().s.parseJsonObject        
        val agentName = getAgentName( ua, json.s( 'agent_name ) )
        
        obj( 'agentType )         = json.s( 'agent_type )
        obj( 'agentName )         = agentName
        obj( 'agentVersion )      = json.s( 'agent_version )
        obj( 'osType )            = json.s( 'os_type )
        obj( 'osName )            = json.s( 'os_name )
        obj( 'osVersionName )     = json.s( 'os_versionName )
        obj( 'osVersionNumber )   = json.s( 'os_versionNumber )
        obj( 'osProducer )        = json.s( 'os_producer )
        obj( 'osProducerUrl )     = json.s( 'os_producerURL )
        obj( 'linuxDistribution ) = json.s( 'linux_distribution )
        obj( 'agentLanguage )     = json.s( 'agent_language )
        obj( 'agentLanguageTag )  = json.s( 'agent_languageTag )
    
        obj.remove( 'bot )
        
        if ( agentName match {
             case "AdsBot-Google"
                | "Baiduspider"
                | "Bingbot"
                | "ClickTale bot"
                | "Exabot"
                | "FeedFetcher-Google"
                | "Googlebot"
                | "Googlebot-Image"
                | "Jakarta Commons"
                | "Java"
                | "ia_archiver"
                | "libwww-perl"
                | "LinkedInBot"
                | "Morfeus Fucking Scanner"
                | "msnbot"
                | "Plesk"
                | "Python-urllib"
                | "SkimBot"
                | "webcollage"
                | "Yahoo! Slurp"
                | "YandexBot"
                | "ZmEu"               => true
             case _                    => false } )
          obj( 'bot ) = true
    
        save
      } catch {
        case j:org.tyranid.json.JsonDecoderException =>
          UserAgent.serviceFailure = true
          j.printStackTrace()
        case k:org.tyranid.http.Http403Exception =>
          UserAgent.serviceFailure = true
          k.printStackTrace()
        case e:Throwable => 
          UserAgent.serviceFailure = true
          e.printStackTrace
      }
    }
    
    if ( UserAgent.serviceFailure ) {
      val uap = new UserAgentParser( ua )
      
      obj( 'agentVersion ) = uap.browserVersion
      obj( 'agentName ) = uap.browserName
      obj( 'osName ) = uap.browserOperatingSystem
    }
  }

  def bot = b( 'bot )

  def agentName = s( 'agentName )
  def agent = agentName + " " + s( 'agentVersion )
  def os    = s( 'osName ) + " " + s( 'osVersionName ) + " " + s( 'osVersionNumber )

  def isFirefox = s( 'agentName ) == "Firefox"
  //def isIE = T.session.getOrElse( "isIE", false )._b // s( 'agentName ) == "Internet Explorer"
  def isIE = ( s( 'agentName ) == "Internet Explorer" || s( 'agentName ) == "MSIE" ) || T.session.getOrElse( "isIE", false.as[Serializable] )._b
    
  def uaVersion = T.session.getOrElse( "uav", 0 )._i
  
 // def betterThanIE8 = !isIE || uaVersion >=9 // !isIE || s( 'agentVersion )._i >= 9
  def betterThanIE8 = !isIE || s( 'agentVersion )._i >= 9 || uaVersion >= 9
 
  //def betterThanIE9 = !isIE || uaVersion >=10 // !isIE || s( 'agentVersion )._i >= 10
  def betterThanIE9 = !isIE || s( 'agentVersion )._i >= 10 || uaVersion >= 10
  
  def html5FileSupport = {
    //updateIfNeeded
    betterThanIE9
  }
  
  def htmlUnicodeSupport = {
    //updateIfNeeded
    betterThanIE8
  }
  
  def htmlCss3Rotate = {
    //updateIfNeeded
    betterThanIE8
  }  
  
  def html5VideoSupport = {
    //updateIfNeeded
    betterThanIE9
  }
  
  def isMobile = {
    //updateIfNeeded
    s( 'agentType ).containsIgnoreCase( "mobile" )  
  }
}
