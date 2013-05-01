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

import java.util.{ Date, TimeZone }

import javax.servlet.http.{ HttpSession, HttpSessionEvent, HttpSessionListener }

import scala.collection.mutable
import scala.xml.{ Node, NodeSeq, Unparsed }

import org.bson.types.ObjectId

import org.tyranid.db.meta.TidCache
import org.tyranid.db.mongo.Imp._
import org.tyranid.http.UserAgent
import org.tyranid.json.{ Js, JsCmd }
import org.tyranid.Imp._
import org.tyranid.math.Base62
import org.tyranid.profile.User
import org.tyranid.report.Query
import org.tyranid.social.Social
import org.tyranid.time.Time
import org.tyranid.ui.LnF
import org.tyranid.QuickCache
import org.tyranid.web.{ Comet, WebContext }

object SessionCleaner { 
  def clean {
    val now = System.currentTimeMillis
    
    WebSession.sessions.filter( sess => {
      val httpsess = sess._2; 
    
      try {
        val tyrsess = httpsess.getAttribute( WebSession.HttpSessionKey ).as[Session]
        
        if ( tyrsess != null ) { 
          //val lastPathTime = tyrsess.get( "lastPathTime" ).as[Date]
          val idle = now - httpsess.getLastAccessedTime
          val user = tyrsess.user
          
          ( user.isNew && idle > (2*Time.OneMinuteMs) ) || idle > Time.HalfHourMs
        } else {
          true
        }
      } catch {
        case e:IllegalStateException =>
          true
        case e =>
          e.printStackTrace
          false
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

  def LnF = Session().LnF

  def website = "https://" + B.domainPort

  // --- HTTP Session

  private var httpData:HttpSession = _

  def http:HttpSession = httpData

  def http_=( obj:HttpSession ) = {
    httpData = obj
    clear
  }

  def clear {
    tyrSession = null
  }


  // --- Tyranid Session

  private var tyrSession:Session = _
  
  def becomeSession( s:Session ) = tyrSession = s

  def session:Session = {
    if ( tyrSession == null ) {
      tyrSession =
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

    tyrSession
  }
  
  def unlinkSession = {
    http.setAttribute( WebSession.HttpSessionKey, null )
    http.isLoggingOut = true
    tyrSession = null
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


  /*
   * * *  RequestCache ... prefer TidCache to this, this is problematic to extend into something like memcache
   */

  val requestCache = mutable.Map[String,Any]()

	def requestCached[ T ]( key:String )( block: => T ):T = requestCache.getOrElseUpdate( key, block ).as[T]



  /*
   * * *  PegDown
   */

  lazy val pegdown = new org.pegdown.PegDownProcessor( org.pegdown.Extensions.ALL )
}


trait SessionMeta {
  val LnF_KEY = "LnF"
  val UA_KEY = "UA"
    
  def apply():Session = ThreadData().session

  def byHttpSessionId( id:String ) =
    WebSession.sessions.get( id ) match {
    case Some( s ) => from( s )
    case None      => null
    }

  def from( httpSession:HttpSession ) =
    httpSession.getAttribute( WebSession.HttpSessionKey ) match {
    case s:Session => s
    case _         => null
    }
}


object Session extends SessionMeta

trait Session extends QuickCache {
  lazy val id = Base62.make( 10 )

  private var userVar = B.newUser()

  def user:User           = userVar
  def orgId               = user.orgId
  def orgTid              = user.orgTid
  def user_=( user:User ) = userVar = user

  var loggedEntry = false
  var loggedUser  = false

  var debug       = B.DEV

  var passedCaptcha = !B.requireReCaptcha

  def LnF = get( Session.LnF_KEY ).as[LnF] ?| org.tyranid.ui.LnF.RetailBrand
  
  def get( key:String ) = getV( key )
  def getOrElse( key:String, any:Any ) = getVOrElse( key, any.as[AnyRef] )
  def getOrElseUpdate( key:String, any:Any ) = getVOrElseUpdate( key, any.as[AnyRef] )
  def put( key:String, value:Any ) = putV( key, value.as[AnyRef] )
  def clear( key:String ) = clearCache( key )
  
  def ua( web: WebContext ) = {
    var tUa:UserAgent = get( Session.UA_KEY ).as[UserAgent]
    
    if ( tUa == null ) {
      if ( web == null )
        tUa = UserAgent.getById( 1 )
      else {
        tUa = UserAgent.getById( web.userAgentId )
      }
      
      try {
        tUa.updateIfNeeded
      } catch {
        case e =>
          e.printStackTrace();
      }
      
      put( Session.UA_KEY, tUa )
    }
      
    tUa
  } 
  
  /*
   * * *   Login
   */

  def login( user:User, incognito:Boolean = false ) = {
    this.user = user
    user.loggedIn = true
    put( "lastLogin", user.t( 'lastLogin ) )
    
    if ( !incognito ) {
      var updates = Mobj( "lastLogin" -> new Date )

      if ( tz != null && tz != user.timeZone ) {
        var id = tz.getID
        updates( 'tz ) = id
        user( 'tz ) = id
      }

      B.User.db.update( Mobj( "_id" -> user.id ), Mobj( $set -> updates, $inc -> Mobj( "numLogins" -> 1 ) ) )
      log( Event.Login, "bid" -> TrackingCookie.get )

      B.loginListeners.foreach( _( user ) )
    } else {
      put( "incognito", Boolean.box( true ) )
    }
      
    val onLogin = B.onLogin
    
    if ( onLogin != null )
      onLogin( this )
      
    val web = T.web
    var userAgent:UserAgent = null
    
    if ( web != null ) {
      val req = web.req
      
      if ( req != null ) {
        web.req.addJsCmd( Js( "tyrl( window.cometConnect );" ) )
        
        put( "remoteHost", web.req.getRemoteHost() )
        put( "remoteAddr", web.req.getRemoteAddr() )
        userAgent = ua( web )
      }
    }
  }
  
  def isIncognito = get( "incognito" ).as[Boolean] ? true | false

  def isAllowingEmail = !isIncognito || get( "allowEmail" ).as[Boolean]

  def setAllowEmail = put( "allowEmail", Boolean.box( true ) )
  def clearAllowEmail = clear( "allowEmail" )

  def logout( unlink:Boolean = true ) = {
    org.tyranid.profile.LoginCookie.remove
    Social.removeCookies
    
    if ( unlink ) T.unlinkSession

    val u = user
    if ( u != null && !isIncognito )
      B.logoutListeners.foreach( _( u ) )
  }

  /*
   * * *   Time Zones
   */

  private var tz:TimeZone = null

  def setTimeZoneFromClient( olsonCode:String ) {

    val tz = TimeZone.getTimeZone( olsonCode )

    val u = user

    if ( u != null ) {
      u( 'tz ) = olsonCode

      if ( !u.isNew )
        B.User.db.update( Mobj( "_id" -> user.id ), Mobj( $set -> Mobj( "tz" -> olsonCode ) ) )
    }
  } 

  /*
   * This returns the definitive timeZone for the user or null if the timeZone is not known
   */
  def timeZone:TimeZone = {

    if ( tz != null )
      return tz

    val u = user
    if ( u != null && u.loggedIn )
      tz = u.timeZone

    tz
  }

  def netTimeZone:TimeZone = {
    val tz = timeZone

    if ( tz != null ) tz
    else              TimeZone.getDefault
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
   * * *   Notifications
   */

  @volatile private var notes:List[Notification] = Nil
  
  def notice( msg:AnyRef, extra:NodeSeq = null, deferred:String = null ) = notes ::= Notification( Notification.NOTICE, msg.toString, cssClass = "alert-success", extra, deferred )
  def warn( msg:AnyRef, extra:NodeSeq = null, deferred:String = null )   = notes ::= Notification( Notification.WARN, msg.toString, cssClass= "alert", extra, deferred )
  def error( msg:AnyRef, extra:NodeSeq = null, deferred:String = null )  = notes ::= Notification( Notification.ERROR, msg.toString, cssClass= "alert-error", extra, deferred )

  def popNotices = popNotes( Notification.NOTICE )
  def popWarnings = popNotes( Notification.WARN )
  def popErrors = popNotes( Notification.ERROR )
   
  def popNotes( level:Int = 0 ) = {
    val webPath = T.web.path or ""
    if ( level == 0 ) {
      val ( n, n2 ) = notes.partition( no => no.deferred.isBlank || webPath.endsWith( no.deferred ) )
      notes = ( n2 == null || n2.length == 0 ) ? Nil | n2
      n
    } else {
      var ( n, n2 ) = notes.partition( no => ( no.level == level && ( no.deferred.isBlank || webPath.endsWith( no.deferred ) ) ) )
      notes = n2
      n
    }
  }

  def hasNotices = peekNotes( Notification.NOTICE ).length > 0 
  def hasWarnings = peekNotes( Notification.WARN ).length > 0 
  def hasErrors = peekNotes( Notification.ERROR ).length > 0
  
  def hasAny = notes.length > 0 
  
  def peekNotes( level:Int = 0 ): List[Notification] = ( level == 0 ) ? notes | notes.filter( n => n.level == level )

  @volatile var unshownPosts:Int = 0


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
  val NOTICE = 1
  val WARN = 2
  val ERROR = 3
  
  def box:NodeSeq = {
    val sess = Session()

     { sess.popNotes().map { note =>
       <div class={ "alert" + ( ( note.cssClass.notBlank ) |* ( " " + note.cssClass ) ) } data-dismiss="alert">
        <a class="close" data-dismiss="alert" href="#">&times;</a>
        { Unparsed( note.msg ) }
        { if ( note.extra != null ) note.extra }
       </div> 
       } 
     }
  }
}

case class Notification( level:Int, msg:String, cssClass:String = "", extra:NodeSeq = null, deferred:String = null )


