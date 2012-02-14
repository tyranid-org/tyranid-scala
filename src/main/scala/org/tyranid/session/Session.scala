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

package org.tyranid.session

import javax.servlet.http.{ HttpSession, HttpSessionEvent, HttpSessionListener }

import scala.collection.mutable
import scala.xml.{ Node, NodeSeq, Unparsed }

import org.tyranid.Imp._
import org.tyranid.profile.User
import org.tyranid.report.Query
import org.tyranid.web.WebContext


object WebSession {

  val sessions = mutable.Map[String,HttpSession]()

  val CometHttpSessionIdKey = "tyrSessId"
  val HttpSessionKey = "tyrSess"
}

class WebSessionListener extends HttpSessionListener {
 
  def sessionCreated( e:HttpSessionEvent ) {
    val session = e.getSession
    WebSession.sessions( session.getId ) = session
  }
 
  def sessionDestroyed( e:HttpSessionEvent ) {
    WebSession.sessions.remove( e.getSession.getId )
  }	
}

object ThreadData {

  private val data = new ThreadLocal[ThreadData]()

  def apply():ThreadData = {
    var td = data.get

    if ( td == null ) {
      td = new ThreadData
      data.set( td )
    }

    td
  }

}

class ThreadData {

  // --- HTTP Session

  private var httpData:HttpSession = _

  def http:HttpSession = {

    if ( httpData == null )
      net.liftweb.http.S.session.foreach { assignFromLiftSession _ }

    httpData
  }

  def http_=( obj:HttpSession ) = {
    httpData = obj
    tyrData = null
  }


  def assignFromLiftSession( liftSession:net.liftweb.http.LiftSession ) =
    http = {
      val liftSess = liftSession.httpSession.open_!.asInstanceOf[net.liftweb.http.provider.servlet.HTTPServletSession]
      val field = liftSess.getClass.getDeclaredField( "session" )
      field.setAccessible( true )
      field.get( liftSess ).asInstanceOf[javax.servlet.http.HttpSession]
    }


  // --- Tyranid Session

  private var tyrData:Session = _

  // rename tyr to session once lift is removed
  def tyr:Session = {
    if ( tyrData == null ) {
      tyrData =
        if ( http != null ) {
          http.getAttribute( WebSession.HttpSessionKey ) match {
          case s:Session => s
          case _         =>
            val s = Tyr.newSession()
            http.setAttribute( WebSession.HttpSessionKey, s )
            s
          }
        } else {
          Tyr.newSession()
        }
    }

    tyrData
  }

  def user:User = {
    if ( tyr != null ) tyr.user
    else               null
  }


  // --- WebContext

  @volatile var ctx:WebContext = _
}


object SessionMeta {

}

trait SessionMeta {

  def apply():Session = ThreadData().tyr


  def byHttpSessionId( id:String ) =
    WebSession.sessions( id ) match {
    case s:HttpSession => from( s )
    case _             => null
    }

  def from( httpSession:HttpSession ) =
    httpSession.getAttribute( WebSession.HttpSessionKey ) match {
    case s:Session => s
    case _         => null
    }
}


object Session extends SessionMeta

trait Session {

  private var userVar = Tyr.newUser()

  def user:User           = userVar
  def user_=( user:User ) = userVar = user

  var loggedEntry = false
  var loggedUser  = false


  /*
   * * *   Reports
   */

  private val reports = mutable.Map[String,org.tyranid.report.Report]()

  def reportFor( query:Query ) = reports.synchronized {
    reports.getOrElseUpdate( query.name, query.newReport )
  }


  /*
   * * *   Editing
   */

  private val editings = mutable.Map[ Class[_], AnyRef ]()

  def editing[ T: Manifest ]( gen: => AnyRef ) =
    editings.getOrElseUpdate( manifest[T].erasure, gen ).asInstanceOf[T]
  def doneEditing[ T: Manifest ] =
    editings.remove( manifest[T].erasure )
  def clearAllEditing = editings.clear


  /*
   * * *   Notifications
   */

  @volatile private var notes:List[Notification] = Nil

  def notice( msg:AnyRef ) = notes ::= Notification( "notice",  msg.toString )
  def warn( msg:AnyRef )   = notes ::= Notification( "warning", msg.toString )
  def error( msg:AnyRef )  = notes ::= Notification( "error",   msg.toString )

  def popNotes = {
    val n = notes
    notes = Nil
    n
  }
}

object Notification {

  def box:NodeSeq = {
    val sess = Session()

    <div class="notify">
     { sess.popNotes.map { note => <div class={ note.level }>{ Unparsed( note.msg ) }</div> } }
    </div>
  }
}

case class Notification( level:String, msg:String )


