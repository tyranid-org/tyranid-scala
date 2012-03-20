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
import org.tyranid.db.{ DbChar, DbDateTime, DbInt, Record, DbLong }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity }
import org.tyranid.email.AWSEmail
import org.tyranid.http.UserAgent
import org.tyranid.report.{ Field, Run, MongoQuery }


object Log extends MongoEntity( tid = "a0Bu" ) {

  // Event Types
  val Access     = 1
  val StackTrace = 2
  val LinkedIn   = 3
  val Error404   = 4
  val Scraper    = 5
  val Import     = 6
  val Facebook   = 7
  val SMS_Out    = 8
  val SMS_In     = 9

  val Events = Array( 
    "n/a",
    "Access",
    "StackTrace",
    "LinkedIn",
    "404",
    "Scraper",
    "Import",
    "Facebook",
    "SMS-Out",
    "SMS-In" )


  "id"                  is DbMongoId      is 'key;
  "e"                   is DbInt          as "Event";
  "on"                  is DbDateTime     ;
  "m"                   is DbChar(1024)   as "Message";
  "du"                  is DbLong         as "Duration in MS";
  "ct"                  is DbInt          as "Count";
  "ex"                  is DbChar(1024)   as "Stack Trace";
  "sid"                 is DbChar(64)     as "Session";
  "uid"                 is DbMongoId      as "User";
  "ua"                  is DbChar(256)    as "User Agent";
  "ip"                  is DbChar(32)     as "IP"; // req.getRemoteAddr

  def log( event:Int, opts:(String,Any)* ) = {
    val l = Mobj(
      "e" -> event,
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

    db.save( l )

    if ( event == StackTrace && B.PRODUCTION ) {
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
          val stackTraceText = sb.toString()
      
          if ( !shouldIgnore( stackTraceText ) ) {
            println( "*** sending email" )
            
            AWSEmail( subject = "Volerro Stack Trace",
                   text = stackTraceText ).
              addTo( B.alertEmail ).
              from( "no-reply@" + B.domain ).
              send
          }
        } catch {
        case e =>
          e.printStackTrace
        }
      }
    }
  }
  
  val ignoredExceptions = List( "EofException" )
  
  private def shouldIgnore( stackTraceText:String ):Boolean = {
    for ( ignore <- ignoredExceptions )
      if ( stackTraceText.contains( ignore ) )
        return true
    
    return false
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
    new Field {
      def name = "e"
      override lazy val label = "Event"
      def cell( run:Run, r:Record ) = Unparsed( Log.Events( r.i( 'e ) ) )
    },
    dateTime( "on" ),
    string( "sid" ),
    new Field {
      def name = "uid"
      override lazy val label = "User"
      def cell( run:Run, r:Record ) = {
        r.oid( 'uid ) match {
        case null => Unparsed( "" )
        case uid  => Unparsed( B.userMeta.nameFor( uid ) )
        }
      }
    },
    multistring( "m" ),
    new Field {
      def name = "ua"
      override lazy val label = "User Agent"
      def cell( run:Run, r:Record ) = {
        Text(
          r( 'ua ) match {
          case s:String             => r( 'ua ) = UserAgent.idFor( s )
                                       r.save
                                       s
          case id:java.lang.Integer => UserAgent.uaFor( id )
          case _                    => ""
          } )
      }
    },
    string( "ip" ),
    multistring( "ex" ),
    new Field {
      def name = "ct"
      override lazy val label = "Count"
      def cell( run:Run, r:Record ) = Unparsed( r.s( 'ct ) )
    },
    new Field {
      def name = "du"
      override lazy val label = "Duration (ms)"
      def cell( run:Run, r:Record ) = Unparsed( r.s( 'du ) )
    }
  )

  val defaultFields = allFields.take( 4 )
}

