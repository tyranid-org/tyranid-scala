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

package org.tyranid.session

import java.util.Date

import javax.servlet.http.{ HttpSession, HttpSessionEvent, HttpSessionListener }

import scala.collection.mutable
import scala.xml.{ Node, NodeSeq, Unparsed }

import org.bson.types.ObjectId

import org.tyranid.db.meta.TidCache
import org.tyranid.db.mongo.Imp._
import org.tyranid.Imp._
import org.tyranid.math.Base62
import org.tyranid.profile.User
import org.tyranid.report.Query
import org.tyranid.social.Social
import org.tyranid.time.Time
import org.tyranid.web.WebContext


object SessionCleaner { 
  def clean {
    val now = System.currentTimeMillis
    
    WebSession.sessions.filter( sess => {
      val httpsess = sess._2; 
      val tyrsess = httpsess.getAttribute( WebSession.HttpSessionKey ).as[Session]
      
      if ( tyrsess != null ) { 
        val idle = now - httpsess.getLastAccessedTime
        val user = tyrsess.user
        
        ( user.isNew && idle > (2*Time.OneMinuteMs) ) || idle > Time.HalfHourMs
      } else {
        true
      }
    }).foreach( sess => { WebSession.sessions.remove( sess._1 ); sess._2.invalidate } )
  }
}
   
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

  def http:HttpSession = httpData

  def http_=( obj:HttpSession ) = {
    httpData = obj
    clear
  }

  def clear {
    tyrData = null
  }


  // --- Tyranid Session

  private var tyrData:Session = _
  
  def becomeSession( s:Session ) = tyrData = s

  // rename tyr to session once lift is removed
  def session:Session = {
    if ( tyrData == null ) {
      tyrData =
        if ( http != null ) {
          http.getAttribute( WebSession.HttpSessionKey ) match {
          case s:Session => s
          case _         =>
            val s = B.newSession()
            http.setAttribute( WebSession.HttpSessionKey, s )
            s
          }
        } else {
          B.newSession()
        }
    }

    tyrData
  }

  def user:User =
    if ( session != null ) session.user
    else                   null


  /*
   * * *  WebContext
   */

  @volatile var web:WebContext = _


  /*
   * * *  Security
   */

  def isEye = user != null && user.isGod && user.b( 'eye )
  def isGod = user != null && user.isGod
  def viewing( ref:AnyRef ) = B.access( this, org.tyranid.secure.Viewing, ref )
  def editing( ref:AnyRef ) = B.access( this, org.tyranid.secure.Editing, ref )

  def ip = if ( web != null ) web.ip else null


  /*
   * * *  TidCache
   */

  val tidCache = new TidCache
}


object SessionMeta {
}

trait SessionMeta {
  def apply():Session = ThreadData().session

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

  lazy val id = Base62.make( 10 )

  private var userVar = B.newUser()

  def user:User           = userVar
  def orgId               = user.orgId
  def orgTid              = user.orgTid
  def user_=( user:User ) = userVar = user

  var loggedEntry = false
  var loggedUser  = false

  var passedCaptcha = !B.requireReCaptcha

  def clear {
    reports.clear
    editings.clear
    cache.clear
    
    loggedEntry = false
    loggedUser = false

    val u = B.newUser()
    u.loggedIn = false
    u.isLoggingOut = true
    user = u
  }


  /*
   * * *   Login
   */

  def login( user:User ) = {
    this.user = user
    user.loggedIn = true
    B.User.db.update( Mobj( "_id" -> user.id ), Mobj( $set -> Mobj( "lastLogin" -> new Date ) ) )
  }

  def logout = {
    clear
    org.tyranid.profile.LoginCookie.remove
    Social.removeCookies
  }


  /*
   * * *   Reports
   */

  private val reports = mutable.Map[String,org.tyranid.report.Report]()

  def reportFor( queryName:String ) = reports.synchronized {
    ( queryName isBlank ) ? null | reports.getOrElseUpdate( queryName, Query.byName( queryName ).newReport )
  }


  /*
   * * *   Editing
   */

  private val editings = mutable.Map[ Class[_], AnyRef ]()

  def edit[ T: Manifest ]( v:AnyRef ) = editings( manifest[T].erasure ) = v
  def editing[ T: Manifest ]( gen: => AnyRef ):T = editings.getOrElseUpdate( manifest[T].erasure, gen ).asInstanceOf[T]

  def editing[ T: Manifest ]:T                   = editings( manifest[T].erasure ).asInstanceOf[T]
  
  // Note that T and clz are not usually the same ... T might be org.tyranid.profile.User while clz represents com.company.profile.User
  def editing2[ T:Manifest ]( clz:Class[_], gen: => AnyRef ):T = editings.getOrElseUpdate( clz, gen ).asInstanceOf[T]
  
  def doneEditing[ T: Manifest ] = editings.remove( manifest[T].erasure )

  def clearAllEditing = editings.clear


  /*
   * * *   Cache
   */

  val cache = mutable.Map[String,AnyRef]()
  def get( key:String ) = cache.getOrElse( key, null )
  def put( key:String, value:AnyRef ) = cache.put( key, value )
  def clear( key:String = null ) = key.isBlank ? cache.clear | cache.remove( key )


  /*
   * * *   Notifications
   */

  @volatile private var notes:List[Notification] = Nil

  def notice( msg:AnyRef, extra:NodeSeq = null ) = notes ::= Notification( "notice",  msg.toString, extra )
  def warn( msg:AnyRef, extra:NodeSeq = null )   = notes ::= Notification( "warning", msg.toString, extra )
  def error( msg:AnyRef, extra:NodeSeq = null )  = notes ::= Notification( "error",   msg.toString, extra )

  def popNotes = {
    val n = notes
    notes = Nil
    n
  }


  /*
   * * *   Web Paths / Tabs Memory
   */

  private val pathChoices = mutable.Map[String,String]()

  def pathChoiceAt( wpath:String, default:String ) = pathChoices.synchronized {
    pathChoices.getOrElseUpdate( wpath, default )
  }

  def setPathChoiceAt( wpath:String, rpath:String ) = pathChoices.synchronized {
    pathChoices( wpath ) = rpath
  }
}

object Notification {

  def box:NodeSeq = {
    val sess = Session()

    <div class="notify">
     { sess.popNotes.map { note => 
       <div class={ note.level }>
        { Unparsed( note.msg ) }
        { if ( note.extra != null ) note.extra }
       </div> 
       } 
     }
    </div>
  }
}

case class Notification( level:String, msg:String, extra:NodeSeq = null )


