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

import scala.xml.Unparsed

import org.tyranid.Imp._
import org.tyranid.db.{ DbChar, DbDateTime, DbInt, Record }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity }
import org.tyranid.report.{ Field, Run, MongoQuery }
import org.tyranid.session.{ Session, ThreadData }


object Log extends MongoEntity( tid = "a0Bu" ) {

  // Event Types
  val Access     = 1
  val StackTrace = 2
  val LinkedIn   = 3

  val Events = Array( "n/a", "Access", "StackTrace", "LinkedIn" )


  "id"                  is DbMongoId      is 'key;
  "e"                   is DbInt          as "Event";
  "on"                  is DbDateTime     ;
  "m"                   is DbChar(1024)   as "Message";
  "ex"                  is DbChar(1024)   as "Stack Trace";
  "sid"                 is DbChar(64)     as "Session";
  "uid"                 is DbMongoId      as "User";
  "ua"                  is DbChar(256)    as "User Agent";

  def log( event:Int, opts:(String,Any)* ) = {
    val l = Mobj(
      "e" -> event,
      "on" -> new Date
    )

    val thread = ThreadData()
    val session = thread.tyr

    if ( session != null ) {
      val user = session.user
      if ( user != null )
        l( "uid" ) = user.id
    }

    val http = thread.http
    if ( http != null )
      l( "sid" ) = http.getId

    // TODO:  maybe log comet sid ?

    for ( opt <- opts ) {
      opt match {
      case ( "ex", t:Throwable ) =>
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
      override def label = "Event"
      def cell( run:Run, r:Record ) = Unparsed( Log.Events( r.i( 'e ) ) )
    },
    dateTime( "on" ),
    string( "sid" ),
    new Field {
      def name = "uid"
      override def label = "User"
      def cell( run:Run, r:Record ) = {
        r.oid( 'uid ) match {
        case null => Unparsed( "" )
        case uid  => Unparsed( Tyr.userMeta.nameFor( uid ) )
        }
      }
    },
    multistring( "m" ),
    string( "ua" ),
    multistring( "ex" )
  )

  val defaultFields = allFields.take( 4 )
}

