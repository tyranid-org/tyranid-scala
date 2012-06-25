/**
 * Copyright (c) 2008-2012 Tyranid <http://tyranid.org>
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

  def uaFor( id:Int ) = synchronized {
    uaById.getOrElseUpdate( id, {
      db.findOne( Mobj( "_id" -> id ) ) match {
      case null => "unknown"
      case to   => to.s( "ua" )
      }
    } )
  }

  def idFor( ua:String ) = synchronized {
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
}

class UserAgent( obj:DBObject, parent:MongoRecord ) extends MongoRecord( UserAgent.makeView, obj, parent ) {

  def updateIfNeeded =
    if ( !has( 'agentName ) )
      update

  def update = {
    val ua = s( 'ua )
    val json = ( "http://useragentstring.com?uas=" + ua.encUrl + "&getJSON=all" ).GET().s.parseJsonObject

    var agentName = json.s( 'agent_name )

    if ( agentName == "unknown" ) {
      if      ( ua startsWith "LinkedInBot" )
        agentName = "LinkedInBot"
      else if ( ua startsWith "Jakarta Commons" )
        agentName = "Jakarta Commons"
      else if ( ua startsWith "ZmEu" )
        agentName = "ZmEu"
      else if ( ua startsWith "AdsBot-Google" )
        agentName = "AdsBot-Google"
    }

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
         case "Bingbot"
            | "FeedFetcher-Google"
            | "Googlebot"
            | "Googlebot-Image"
            | "Jakarta Commons"
            | "Java"
            | "libwww-perl"
            | "LinkedInBot"
            | "msnbot"
            | "Python-urllib"
            | "webcollage"
            | "Yahoo! Slurp"
            | "YandexBot"
            | "ZmEu"               => true
         case _                    => false } )
      obj( 'bot ) = true

    save
  }

  def bot = b( 'bot )

  def agentName = s( 'agentName )
  def agent = agentName + " " + s( 'agentVersion )
  def os    = s( 'osName ) + " " + s( 'osVersionName ) + " " + s( 'osVersionNumber )
}


