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

package org.tyranid.log

import java.util.Date

import javax.servlet.http.HttpSession

import scala.xml.{ Text, Unparsed }

import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.db.{ DbChar, DbDateTime, DbInt, DbLink, DbLong, DbText, Record, Scope }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity, MongoRecord }
import org.tyranid.db.ram.RamEntity
import org.tyranid.db.tuple.{ Tuple, TupleView }
import org.tyranid.email.AWSEmail
import org.tyranid.http.UserAgent
import org.tyranid.net.DnsDomain
import org.tyranid.report.{ Query, Run, Sort }
import org.tyranid.ui.{ CustomField, PathField, Search }
import org.tyranid.web.{ Weblet, WebContext }


object Event extends RamEntity( tid = "a0It" ) {
  type RecType = Event
  override def convert( view:TupleView ) = new Event( view )


  "_id"    is DbInt      is 'id;
  "name"   is DbChar(64) is 'label;

  override val addNames = Seq( "_id", "name" )

  val Access     = add(  1, "Access" )
  val StackTrace = add(  2, "StackTrace" )
  val LinkedIn   = add(  3, "LinkedIn" )
  val Error404   = add(  4, "404" )
  val Scraper    = add(  5, "Scraper" )
  val Import     = add(  6, "Import" )
  val Facebook   = add(  7, "Facebook" )
  val SmsOut     = add(  8, "SMS-Out" )
  val SmsIn      = add( 10, "SMS-In" )
  val Scheduler  = add( 11, "Scheduler" )
  val Trackur    = add( 12, "Trackur" )
  val Noaa       = add( 13, "NOAA" )
  val Eof        = add( 14, "EOF" )
  val Google     = add( 15, "Google" )
  val RefInt     = add( 16, "RefInt" ) // referential integrity violation
  val Alert      = add( 17, "Alert" )
  val Crocodoc   = add( 18, "Crocodoc" )
  val Scribd     = add( 19, "Scribd" )
  val Issuu      = add( 20, "Issuu" )
  val License    = add( 21, "License" )
  val Search     = add( 22, "Search" )
}

case class Event( override val view:TupleView ) extends Tuple( view ) {
  def name = s( 'name )
}


object Log extends MongoEntity( tid = "a0Ht" ) {
  type RecType = Log
  override def convert( obj:DBObject, parent:MongoRecord ) = new Log( obj, parent )

  "_id"      is DbMongoId         is 'id;
  "e"        is DbLink(Event)     as "Event";
  "on"       is DbDateTime        ;
  "m"        is DbText            as "Message";
  "du"       is DbLong            as "Duration in MS";
  "ct"       is DbInt             as "Count";
  "ex"       is DbText            as "Stack Trace";
  "sid"      is DbChar(64)        as "Session ID";
  "d"        is DbLink(DnsDomain) as "Domain";
  "ua"       is DbLink(UserAgent) as "User Agent";
  "ip"       is DbChar(32)        as "IP";
  "p"        is DbChar(128)       as "Path";
  "bid"      is DbChar(10)        as "Browser ID";

  override def init = {
    super.init
    "uid"    is DbLink(B.User)    as "User";
  }

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

        l( "ex" ) = t.getStackTrace.take(25).map( _.toString ).mkString( "\n" )

      case ( "m", v:String ) =>
        val existingMsg = l.s( 'm )
        if ( existingMsg.isBlank )
          l( 'm ) = v
        else
          l( 'm ) = existingMsg + "\n" + v

      case ( "ua", null ) =>
      case ( "ua", ua:String ) =>
        l( 'ua ) = UserAgent.idFor( ua )

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

    if ( ( effEvent == Event.StackTrace || effEvent == Event.Alert ) && B.PRODUCTION ) {
      println( "*** stack trace/alert entering" )
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

      if ( effEvent == Event.StackTrace )
        sb ++= "\n\nStack Trace:\n\n" + throwable.getStackTrace.map( _.toString ).mkString( "\n" )

      background {
        try {
          println( "*** sending email" )
            
          AWSEmail( subject = ( effEvent == Event.StackTrace ) ? "Volerro Stack Trace" | "Volerro Alert",
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

  override lazy val defaultSort = Sort( "on", "On", "on" -> -1 )
}

class Log( obj:DBObject, parent:MongoRecord ) extends MongoRecord( Log.makeView, obj, parent ) {

  def ua = UserAgent.byId( get( 'ua ) )
}


object LogQuery extends Query {

  //def connections( run:Run ) =
    //run.cache.getOrElseUpdate( "connections", Connection.db.find( Mobj( "from" -> Session().user.org.id ) ).toSeq ).asInstanceOf[Seq[DBObject]]

  val entity = Log
  val name = "log"

  override def newReport = {
    var r = super.newReport
    r.sort = Log.defaultSort
    r
  }

  val fields = Seq(
    new CustomField {
      def name = "tid"
      override lazy val label = "TID"
      override def cell( s:Scope ) = <a href={ "/admin/tid?tid=" + s.rec.tid } class="eyeBtn" style="margin:0 1px;">T</a>
    },
    PathField( "e",                search = Search.Equals ),
    PathField( "on" ),
    PathField( "on", "From Date",  search = Search.Gte,   data = false ),
    PathField( "on", "To Date",    search = Search.Lte,   data = false ),
    PathField( "sid",              search = Search.Equals ),
    new CustomField {
      def name = "uid"
      override lazy val label = "User"
      override def cell( s:Scope ) = {
        s.rec.oid( 'uid ) match {
        case null => Unparsed( "" )
        case uid  => <a href={ "/admin/tid?tid=" + B.User.idToTid( uid ) }>{ B.userMeta.nameFor( uid ) }</a>
        }
      }
    },
    PathField( "m", search = Search.Subst ),
    new CustomField {
      def name = "ua"
      override lazy val label = "User Agent"
      override def cell( s:Scope ) = {
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
    PathField( "d",   search = Search.Equals ),
    PathField( "ip",  search = Search.Subst ),
    PathField( "ex",  search = Search.Subst ),
    PathField( "ct",  search = Search.Gte ),
    PathField( "du",  search = Search.Gte ),
    PathField( "p",   search = Search.Subst ),
    PathField( "bid", search = Search.Equals ),
    new CustomField {
      def name = "agent"
      override def cell( s:Scope ) = Text( s.rec.as[Log].ua.flatten( _.agent, "" ) )
    },
    new CustomField {
      def name = "agentName"
      override def cell( s:Scope ) = Text( s.rec.as[Log].ua.flatten( _.s( 'agentName ), "" ) )
    },
    new CustomField {
      def name = "agentVersion"
      override def cell( s:Scope ) = Text( s.rec.as[Log].ua.flatten( _.s( 'agentVersion ), "" ) )
    },
    new CustomField {
      def name = "os"
      override lazy val label = "OS"
      override def cell( s:Scope ) = Text( s.rec.as[Log].ua.flatten( _.os, "" ) )
    }
  )

  val defaultFields = dataFields.take( 5 )
}

object Loglet extends Weblet {

  def handle( web:WebContext ) = {

    if ( !T.user.isGod )
      _404

    rpath match {
    case "/" =>
      shell( LogQuery.draw )

    case _ =>
      _404
    }
  }
}


