/**
 * Copyright (c) 2008-2011 Tyranid <http://tyranid.org>
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

package org.tyranid.log

import java.util.Date

import javax.servlet.http.HttpSession

import scala.xml.{ Text, Unparsed }

import org.tyranid.Imp._
import org.tyranid.db.{ DbChar, DbDateTime, DbInt, DbLink, DbLong, DbText, Record, Scope, EnumEntity }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity }
import org.tyranid.db.ram.RamEntity
import org.tyranid.db.tuple.Tuple
import org.tyranid.email.AWSEmail
import org.tyranid.http.UserAgent
import org.tyranid.report.{ Run, MongoQuery }
import org.tyranid.ui.{ CustomField, PathField, Search }


object Event extends RamEntity( tid = "a0It" ) with EnumEntity[Event] {
  "id"     is DbInt      is 'key;
  "name"   is DbChar(64) is 'label;

  override lazy val makeView = viewFor( "id", "name" )

  def apply( id:Int, name:String ) = {
    val t = new Event
    t( 'id )   = id
    t( 'name ) = name
    t
  }

  val Access     = apply(  1, "Access" )
  val StackTrace = apply(  2, "StackTrace" )
  val LinkedIn   = apply(  3, "LinkedIn" )
  val Error404   = apply(  4, "404" )
  val Scraper    = apply(  5, "Scraper" )
  val Import     = apply(  6, "Import" )
  val Facebook   = apply(  7, "Facebook" )
  val SmsOut     = apply(  8, "SMS-Out" )
  val SmsIn      = apply( 10, "SMS-In" )
  val Scheduler  = apply( 11, "Scheduler" )
  val Trackur    = apply( 12, "Trackur" )
  val Noaa       = apply( 13, "NOAA" )
  val Eof        = apply( 14, "EOF" )
  val Google     = apply( 15, "Google" )

  static( Access, StackTrace, LinkedIn, Error404, Scraper, Import, Facebook, SmsOut, SmsIn, Scheduler, Noaa, Eof, Google )
}

class Event extends Tuple( Event.makeView ) {
  def name = s( 'name )
}


object Log extends MongoEntity( tid = "a0Ht" ) {

  "id"                  is DbMongoId      is 'key;
  "e"                   is DbLink(Event)  as "Event";
  "on"                  is DbDateTime     ;
  "m"                   is DbText         as "Message";
  "du"                  is DbLong         as "Duration in MS";
  "ct"                  is DbInt          as "Count";
  "ex"                  is DbText         as "Stack Trace";
  "sid"                 is DbChar(64)     as "Session";
  "uid"                 is DbMongoId      as "User";
  "ua"                  is DbChar(256)    as "User Agent";
  "ip"                  is DbChar(32)     as "IP";

  def log( event:Event, opts:(String,Any)* ) = {
    val l = Mobj(
      "on" -> new Date
    )

    val thread = T
    val session = thread.session

    var user =
      if ( session != null ) {
        val user = session.user
        if ( user != null )
          l( "uid" ) = user.id
        user
      } else {
        null
      }

    val http = thread.http
    if ( http != null )
      l( "sid" ) = http.getId

    // TODO:  maybe log comet sid ?

    var throwable:Throwable = null

    for ( opt <- opts ) {
      opt match {
      case ( "ex", t:Throwable ) =>
        throwable = t
        val lines = t.getStackTrace

        val m = t.getMessage + '\n' + t.getClass.getSimpleName

        val existingMsg = l.s( 'm )
        if ( existingMsg.isBlank )
          l( 'm ) = m
        else
          l( 'm ) = existingMsg + "\n" + m

        l( "ex" ) = t.getStackTrace.take(10).map( _.toString ).mkString( "\n" )

      case ( "m", v:String ) =>
        val existingMsg = l.s( 'm )
        if ( existingMsg.isBlank )
          l( 'm ) = v
        else
          l( 'm ) = existingMsg + "\n" + v

      case ( n:String, v:Any ) =>
        l( n ) = v
      }
    }

    if ( !l.has( 'ip ) )
      l( 'ip ) = thread.ip

    val effEvent =
      event match {
      case e if e == Event.StackTrace && throwable.getClass.getSimpleName == "EofException" => Event.Eof
      case e                                                                                => e
      }

    l( 'e ) = effEvent.id.as[Int]
    db.save( l )

    if ( effEvent == Event.StackTrace && B.PRODUCTION ) {
      println( "*** stack trace entering" )
      val sb = new StringBuilder

      if ( user != null )
        sb ++= "User: " ++= user.fullName += '\n'
          
      sb ++= "On: " ++= l.t( 'on ).toDateTimeStr += '\n'
        
      var ua = l.s( 'ua )
      
      if ( ua.isBlank ) {
        try {
          ua = T.web.req.getHeader( "User-Agent" )
        } catch {
        case e =>
          e.printStackTrace
        }
      }

      if ( ua.notBlank )
        sb ++= "User-Agent: " ++= ua += '\n'

      val msg = l.s( 'm )
      
      if ( msg.notBlank )
        sb ++= "Message:\n\n" + msg

      sb ++= "\n\nStack Trace:\n\n" + throwable.getStackTrace.map( _.toString ).mkString( "\n" )

      background {
        try {
          println( "*** sending email" )
            
          AWSEmail( subject = "Volerro Stack Trace",
                    text = sb.toString ).
            addTo( B.alertEmail ).
            from( "no-reply@" + B.domain ).
            send
        } catch {
        case e =>
          e.printStackTrace
        }
      }
    }
  }
}

object LogQuery extends MongoQuery {

  //def connections( run:Run ) =
    //run.cache.getOrElseUpdate( "connections", Connection.db.find( Mobj( "from" -> Session().user.org.id ) ).toSeq ).asInstanceOf[Seq[DBObject]]

  val entity = Log
  val name = "log"

  override def newReport = {
    var r = super.newReport
    r.sort = Mobj( "on" -> -1 )
    r
  }

  val allFields = Seq(
    PathField( "e",                search = Search.Equals ),
    PathField( "on" ),
    PathField( "on", "From Date",  search = Search.Gte,   data = false ),
    PathField( "on", "To Date",    search = Search.Lte,   data = false ),
    PathField( "sid",              search = Search.Equals ),
    new CustomField {
      def name = "uid"
      override lazy val label = "User"
      def cell( s:Scope ) = {
        s.rec.oid( 'uid ) match {
        case null => Unparsed( "" )
        case uid  => Unparsed( B.userMeta.nameFor( uid ) )
        }
      }
    },
    PathField( "m", search = Search.Subst ),
    new CustomField {
      def name = "ua"
      override lazy val label = "User Agent"
      def cell( s:Scope ) = {
        Text(
          s.rec( 'ua ) match {
          case str:String           => s.rec( 'ua ) = UserAgent.idFor( str )
                                       s.rec.save
                                       str
          case id:java.lang.Integer => UserAgent.uaFor( id )
          case _                    => ""
          } )
      }
    },
    PathField( "ip", search = Search.Subst ),
    PathField( "ex", search = Search.Subst ),
    PathField( "ct", search = Search.Gte ),
    PathField( "du", search = Search.Gte )
  )

  val defaultFields = dataFields.take( 4 )
}

