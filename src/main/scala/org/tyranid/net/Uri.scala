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

package org.tyranid.net

import scala.collection.mutable

import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.db.{ DbChar, DbIntSerial }
import org.tyranid.db.meta.AutoIncrement
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ MongoEntity, MongoRecord }


object Uri {

  def isSecure( uri:String ) = uri.toLowerCase.startsWith( "https://" ) || uri.startsWith( "//" )

  /*
   * Given "google.com" returns "google"
   */
  def domainPart( domain:String ) =
    // TODO:  make this more efficient & intelligent
    org.tyranid.email.Email.domainPart( "junk@" + domain )

  def completeUri( base:String ):String = {

    if ( base.isBlank || base.indexOf( "://" ) != -1 )
      return base

    "http://" + base
  }

  def nameForDomain( domain:String ):String =
    try {
      val s = ( "http://" + domain ).GET().s.toLowerCase

      var idx = s.indexOf( "<title>" )

      if ( idx != -1 ) {
        idx += 7 // skip past <title>
        val sb = new StringBuilder

        while ( idx < s.length ) {
          s.charAt( idx ) match {
          case '<' | '-' | '|' =>
            idx = s.length
        
          case ch =>
            sb += ch
          }

          idx += 1
        }

        sb.toString.trim
      } else {
        null
      }
    } catch {
    case e:java.net.UnknownHostException =>
      return null
    case other:Throwable =>
      other.printStackTrace()
      return null
    }

  /*
   * Example:  AT&T, returns att
   */
  def lowerDomainChars( domain:String ) = {
    val sb = new StringBuilder

    for ( ch <- domain )
      ch match {
      case ch if ch.isLetterOrDigit || ch == '-' => sb += ch.toLower
      case _ =>
      }

    sb.toString
  }

  def cite( url:String ):String =
    try {
      val s = url.GET()

      ""
    } catch {
    case e:java.net.UnknownHostException =>
      return null
    case other:Throwable =>
      other.printStackTrace()
      return null
    }
}


object DnsDomain extends MongoEntity( tid = "a0Pt" ) {
  type RecType = DnsDomain
  override def convert( obj:DBObject, parent:MongoRecord ) = new DnsDomain( obj, parent )


  "_id"               is DbIntSerial   is 'id;
  "domain"            is DbChar(256)   is 'label as "User Agent";

  private val idByDomain = mutable.HashMap[String,Int]()
  private val domainById = mutable.HashMap[Int,String]()

  def domainFor( id:Int ) = synchronized {
    domainById.getOrElseUpdate( id, {
      val uac = db.find( Mobj( "_id" -> id ) ).limit(1)
      val ua = uac.hasNext ? uac.next | null
      
      ua match {
      case null => "unknown"
      case to   => to.s( "ua" )
      }
    } )
  }

  def idFor( domain:String ) = synchronized {
    idByDomain.getOrElseUpdate( domain, {
      val uac = db.find( Mobj( "domain" -> domain ) ).limit(1)
      val ua = uac.hasNext ? uac.next | null
      
      ua match {
      case null =>
        val id = AutoIncrement( "dnsDomain" )
        db.save( Mobj( "_id" -> id, "domain" -> domain ) )
        id

      case to =>
        to.i( "_id" )
      }
    } )
  }
}

class DnsDomain( obj:DBObject, parent:MongoRecord ) extends MongoRecord( DnsDomain.makeView, obj, parent )

