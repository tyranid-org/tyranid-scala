/**
 * Copyright (c) 2008-2014 Tyranid <http://tyranid.org>
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
import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.db.{ DbBoolean, DbChar, DbDateTime, DbInt, DbLink }
import org.tyranid.db.meta.TidCache
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity, MongoRecord }
import org.tyranid.http.UserAgent
import org.tyranid.json.{ Js, JsCmd }
import org.tyranid.math.Base62
import org.tyranid.net.Ip
import org.tyranid.profile.{ LoginCookie, User }
import org.tyranid.report.Query
import org.tyranid.social.Social
import org.tyranid.sso.SsoMapping
import org.tyranid.time.Time
import org.tyranid.QuickCache
import org.tyranid.web.{ Comet, WebContext }


/*
 * * *  WebSession
 */

object SessionCleaner {
  val maxIdleTimeCheck = WebSession.IDLE_TIMEOUT + (5*Time.OneMinuteMs)
  
  def cleanLocal {
    if ( B.SHUTTINGDOWN )
      return

    // Clean up local in-memory sessions
    val now = System.currentTimeMillis
    
    WebSession.sessions.filter { sess =>
      val httpsess = sess._2 
    
      try {
        val tyrsess = httpsess.getAttribute( WebSession.HttpSessionKey ).as[Session]
spam( "tyrsess for " + sess._1 + " = " + tyrsess )
        
        if ( tyrsess != null ) { 
          val idle = now - httpsess.getLastAccessedTime
spam( "idle time for " + sess._1 + " = " + idle )
          val v =
          ( !tyrsess.isVerified && idle > (5*Time.OneMinuteMs) ) || 
            idle > maxIdleTimeCheck || 
            httpsess.getAttribute( WebSession.InvalidateKey ) != null
spam( "removal check for " + sess._1 + " = " + v )
v
        } else {
spam( "true for " + sess._1 )
          true
        }
      } catch {
        case e:IllegalStateException =>
spam( "true-ex for " + sess._1 )
          true
        case e:Throwable =>
          e.printStackTrace
spam( "false-ex for " + sess._1 )
          false
      }
    } foreach { sess =>
      WebSession.sessions.remove( sess._1 )
      sess._2.removeAttribute( WebSession.HttpSessionKey )
      sess._2.invalidate 
    }
    
    // Remove any SessionData records for this server which don't map to local sessions

    val ipHost = Ip.Host.toString
spam( "ipHost: " + ipHost )

    val invalidSessions:Seq[String] = (
      for ( sd <- B.SessionData.db.find( Mobj( "sv" -> ipHost ) );
            ss = sd.s( 'ss );
            if !WebSession.sessions.contains( ss ) )
        yield ss
    ).toSeq
spam( "invalidSessions: " + invalidSessions.mkString )

    if ( invalidSessions.nonEmpty )
      B.SessionData.db.remove( Mobj( "sv" -> ipHost, "ss" -> Mobj( $in -> invalidSessions.toMlist ) ) )
      
    if ( B.profile )
      log( Event.Profile, "m" -> ( "WebSession size: " + WebSession.sessions.memorySize ) )
  }

  def cleanGlobal {
    if ( B.SHUTTINGDOWN )
      return

    val cutoff = new Date - ( 8 * Time.OneHourMs )

    B.SessionData.db.remove(
      Mobj(
        $or -> Mlist(
          Mobj( "lpt" -> Mobj( $lte -> cutoff ) ),
          Mobj(
            $and -> Mlist(
              Mobj( "lpt" -> null ),
              Mobj( "lit" -> Mobj( $lte -> cutoff ) )
            )
          )
        )
      )
    )
  }
}


/*
 * * *  WebSession
 *
 * This is used to keep a complete list of sessions in memory since the Java API doesn't provide that anymore.
 *
 * TODO:  Once we have sessions in MongoDB we can probably remove this.
 */

object WebSession {
  val IDLE_TIMEOUT = Time.OneHourMs
  
  val sessions = mutable.Map[String,HttpSession]()

  val HttpSessionKey = "tyrSess"
  val InvalidateKey  = "tyrInv"

  /*
  def visit( visitor: ( Session ) => Unit ) =
    for ( s <- sessions;
          httpSession = s._2;
          tyrSession = httpSession.getAttribute( WebSession.HttpSessionKey ).as[Session];
          if tyrSession != null )
      visitor( tyrSession )
  */
}

class WebSessionListener extends HttpSessionListener {
 
  def sessionCreated( e:HttpSessionEvent ) {
    val session = e.getSession
    WebSession.sessions( session.getId ) = session

    B.stackdriver.webSessionCount( WebSession.sessions.size )
  }
 
  def sessionDestroyed( e:HttpSessionEvent ) {
    val hs = e.getSession
    val hsid = hs.getId
    val tyrSession = hs.getAttribute( WebSession.HttpSessionKey ).as[org.tyranid.session.Session]
        
    val sess = WebSession.sessions.remove( hsid ).getOrElse( null )
    
    if ( sess != hs ) { 
      spam( "Sessions do not match: " + hsid )
      
      if ( sess == null )
        spam( "session was not in WebSession.sessions." )
    }
    
    if ( sess != null )
      sess.removeAttribute( WebSession.HttpSessionKey )

    B.SessionData.db.update( Mobj( "ss" -> hsid ), Mobj( $set -> Mobj( "exp" -> true ) ) )
    
    if ( tyrSession != null ) {
      val user = tyrSession.user
      tyrSession.user.clearCache()
      tyrSession.user = null
      
      if ( user != null ) {
        B.User.db.update( Mobj( "_id" -> user.id ), Mobj( $set -> Mobj( "ls" -> hsid ) ) )
        
        if ( !B.SHUTTINGDOWN ) {
          val now = System.currentTimeMillis
          val idle = now - hs.getLastAccessedTime()
          
          if ( idle > WebSession.IDLE_TIMEOUT ) {
            if ( !B.PRODUCTION )
              spam( "Timing out: " + hsid )
              
            Comet.timeout( hsid )
          }
        }
      }
    }
    
    B.stackdriver.webSessionCount( WebSession.sessions.size )
  }	
}


/*
 * * *  ThreadData
 */

object ThreadData {
  private var tested = false

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
  def updateQueryString( path:String, name:String, value:Any ) = {
   val re = "([?|&])" + name + "=.*?(&|$)".toPatternI
   val separator = ( path.indexOf('?') > -1 && path.indexOf('=') != -1 ) ? "&" | "?"

   path.matches( re ) ?
          path.replace( re, "$1" + name + "=" + value._s + "$2") |
          path + separator + name + "=" + value._s

  }

  def baseWebsite = "https://" + B.domainPort

  def ioUrl = {
    val cordova = T.web.b( 'cordova ) || T.session.isCordova
    
    if ( B.PRODUCTION )
      ( cordova |* "https:" ) + "//io.volerro.com";
    else
      cordova ? baseWebsite | "";
  }

  def ioWebsite( path:String ) = {
    val cordova = ( T.web != null && T.web.b( 'cordova ) ) || ( T.session != null && T.session.isCordova )
    ioUrl + ( cordova ? updateQueryString( path, "cordova", 1 ) | path )
  }
  
  def website( path:String = "", user:User = null, ssoMapping:SsoMapping = null, subdomain:String = null, forceLite:Boolean = false, forceMain:Boolean = false ):String = {
    val subdomainWebsite = ( !forceMain && forceLite ) ? ( "https://" + B.liteFullDomain + B.port ) | {
      ( session == null || forceMain ) ? baseWebsite | {
        val sd =
          if ( subdomain.notBlank )
            subdomain
          else
            session.s( 'subdomain ) 
        
        ( sd.isBlank ) ? baseWebsite | "https://" + sd + B.port
      }
    }
      
    if ( user == null ) 
      return subdomainWebsite  + path
        
    val ssoMappingImpl = ( ssoMapping == null ) ? {
      val ssoId = user.s( 'sso )
      ssoId.isBlank ? null | SsoMapping.getById( ssoId )
    } | ssoMapping
    
    if ( ssoMappingImpl == null )
      return subdomainWebsite + path
      
    return subdomainWebsite + "/sso/auth/" + ssoMappingImpl.id._s + "?startUrl=" + java.net.URLEncoder.encode( path, "UTF-8" )
  }

  def user:User =
    if ( session != null ) session.user
    else                   null
  

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
  
  def becomeSession( s:Session ) = {
    tyrSession = s
  }

  def session:Session = {
    if ( tyrSession == null ) {
      tyrSession =
        http match {
        case null =>
          B.newSession()

        case http =>
          http.getAttribute( WebSession.HttpSessionKey ) match {
          case s:Session => 
            s
          case _         =>
            val s = B.newSession()
            http.setAttribute( WebSession.HttpSessionKey, s )
            session.httpSessionId = http.getId
            session.id // ensure a SessionData record is recorded in the database
            s
          }
        }
    }

    tyrSession
  }

  def unlinkSession = {
    http.setAttribute( WebSession.HttpSessionKey, null )
    http.setAttribute( WebSession.InvalidateKey, Boolean.box( true ) )
    http.isLoggingOut = true
    tyrSession = null
  }

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

  def clearRequestCache = {
    tidCache.clear
    requestCache.clear
  }

  val requestCache = mutable.Map[String,Any]()

	def requestCached[ T ]( key:String )( block: => T ):T = requestCache.getOrElseUpdate( key, block ).as[T]

  
  /*
   * * *  Permissions Cache
   */

  def permissionCache = requestCached( "permissions" ) { mutable.Map[String,Boolean]() }
  
  
  /*
   * * *  WhiteLabel Cache
   */
  def whitelabelCache = requestCached( "whitelabel" ) { mutable.Map[String,org.tyranid.db.Record]() }

  /*
   * * *  Markdown
   */

  //lazy val actuariusTransformer = new eu.henkelmann.actuarius.ActuariusTransformer()

  //lazy val pegdown = new org.pegdown.PegDownProcessor( org.pegdown.Extensions.ALL )


  /*
   * * *  Mode
   */

  @volatile var background:Boolean = false
}



/*
 * * *  In-Memory Session
 */

trait SessionMeta {
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

  @volatile var httpSessionId:String = null
  
  private var userVar = B.newUser()
  def user:User           = userVar
  def user_=( user:User ) = userVar = user

  def orgId               = user.orgId
  def orgTid              = ( orgId == null ) ? null | B.Org.idToTid( orgId )

  var loggedEntry = false
  var loggedUser  = false
  
  // If the user tid is set in the session
  
  def isVerified = {
    val verified = user.s( 'activationCode ).notBlank ? isLiteVerified | isAuthenticated
    
    !user.isNew && verified
  }
  
  def isHttpSession = httpSessionId != null

  def isCordova = b( 'cordova )
  
  var debug       = B.DEV
  var trace       = false

  var passedCaptcha = !B.requireReCaptcha

  def get( key:String ) = getV( key )
  def getOrElse( key:String, any:java.io.Serializable ) = getVOrElse( key, any )
  def getOrElseUpdate( key:String, any:java.io.Serializable ) = getVOrElseUpdate( key, any )
  def clear( key:String ) = clearCache( key )

  def put( key:String, value:java.io.Serializable ) = putV( key, value )

  def b( key:String ) = getVOrElse( key, false ).as[Boolean]
  def s( key:String ) = getVOrElse( key, null ).as[String]
  def i( key:String ) = getVOrElse( key, null ).as[Integer]
  
  def ua( web: WebContext ) = {
    var tUa:UserAgent = get( Session.UA_KEY ).as[UserAgent]
    
    if ( tUa == null ) {
      tUa = UserAgent.getById( if ( web == null ) 1 else web.userAgentId )
      
      try {
        tUa.updateIfNeeded
      } catch {
        case e:Throwable =>
          e.printStackTrace()
      }
      
      put( Session.UA_KEY, tUa )

      record( "ua" -> tUa.id )
    }
      
    tUa
  } 

  lazy val googleCrossSiteRequestForgeryToken = org.tyranid.social.google.Google.createCrossSiteAntiForgeryToken


  /*
   * * *   SessionData
   */

  private var _id:ObjectId = null

  // Id of the SessionData record corresponding to this in-memory session
  def id:ObjectId = {
    if ( _id == null )
      _id = B.SessionData.idFor( this )

    _id
  }

  def tid:String = return B.SessionData.idToTid( id )

  def data:SessionData = {
    val d = B.SessionData.getById( id )
    if ( d == null ) {
       // this means that user still has an old id around that we deleted the sessiondata for ... recreate a new one
      _id = null
      data
    } else {
      d
    }
  }

  def record( values:Pair[String,Any]* ):Unit = {
    val http = isHttpSession

    for ( pair <- values ) {
      // TODO:  probably should only be recording this to data and not putV, but some data is still looking at the QuickCache stuff ... notably Presence.Key stuff
      putV( pair._1, pair._2.as[java.io.Serializable] )

      if ( http )
        data.obj( pair._1 ) = pair._2
    }

    if ( http ) {
      var seto = Mobj()

      for ( pair <- values )
        seto( pair._1 ) = pair._2

      record( Mobj( $set -> seto ) )
    }
  }

  def record( update:DBObject ):Unit =
    if ( isHttpSession )
      B.SessionData.db.update( Mobj( "_id" -> id ), update )


  /*
   * * *   Login
   */

  def login( user:User, incognito:Boolean = false, sso:SsoMapping = null, setAuth:Boolean = false ) = {
    this.user = user
    put( "lastLogin", user.t( 'lastLogin ) )

    val now = new Date
    
    if ( !incognito ) {
      clear( "incognito" )
      
      val updates = Mobj( "lastLogin" -> now )

      if ( tz != null && tz != user.timeZone ) {
        var id = tz.getID
        updates( 'tz ) = id
        user( 'tz ) = id
      }
      
      if ( sso != null )
        updates( 'sso ) = sso.id
      
      B.User.db.update( Mobj( "_id" -> user.id ), Mobj( $set -> updates ) )

      log( Event.Login, "bid" -> TrackingCookie.get )

      B.loginListeners.foreach( _( user ) )
      
      if ( user.b( 'monitored ) )
        log( Event.Alert, "m" -> ( "User " + user.s( 'email ) + " just logged in." ) )
        
      if ( sso != null )
        put( "sso", sso )        
    } else {
      put( "incognito", true )
      
      val ssoCode = user.s( 'sso )
      
      if ( ssoCode.notBlank ) {
        val mapping = SsoMapping.getById( ssoCode )
        
        if ( mapping != null )       
          put( "sso", mapping )
      }

      B.stackdriver.sendLiteCount
    }
      
    val onLogin = B.onLogin
    
    if ( onLogin != null )
      onLogin( this )
      
    val web = T.web
    var userAgent:UserAgent = null
    
    if ( web != null ) {
      val req = web.req
      
      if ( req != null )
        userAgent = ua( web )
      
      T.requestCache.put( "req-common", true )      
    }

    if ( setAuth || incognito )
      setAuthenticated // verified login
      
    record( "u" -> user.id, "lit" -> now, "incognito" -> incognito )
  }
  
  def clearAuthenticated = clear( "auth" )
  def isAuthenticated = b( "auth" )
  def setAuthenticated = put( "auth", true )
  
  def clearLiteVerified = clear( "lite-v" )
  def setLiteVerified = put( "lite-v", true )
  def isLiteVerified = b( "lite-v" )
  
  def isIncognito = b( "incognito" )

  def isAllowingEmail = !isIncognito || b( "allowEmail" )

  def setAllowEmail = put( "allowEmail", true )
  def clearAllowEmail = clear( "allowEmail" )

  def logout( unlink:Boolean = true, removeCookies:Boolean = true ) = {
    if ( removeCookies ) {      
      if ( !isIncognito )
        LoginCookie.remove
        
      Social.removeCookies
    }
    
    if ( unlink ) T.unlinkSession

    val u = user

    if ( u != null ) {
      record( "u" -> null, "lit" -> null )

      if ( !isIncognito )
        B.logoutListeners.foreach( _( u ) )    
    }
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
    if ( u != null && isVerified )
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
    queryName.isBlank ? null | reports.getOrElseUpdate( queryName, Query.byName( queryName ).newReport )
  }


  /*
   * * *   Editing
   */

  private val editings = mutable.Map[ Class[_], org.tyranid.db.Record ]()

  def edit[ T: Manifest ]( v:org.tyranid.db.Record ) = editings( manifest[T].runtimeClass ) = v
  def editing[ T: Manifest ]( gen: => org.tyranid.db.Record ):T = editings.getOrElseUpdate( manifest[T].runtimeClass, gen ).asInstanceOf[T]

  def editing[ T: Manifest ]:T                   = editings( manifest[T].runtimeClass ).asInstanceOf[T]
  
  // Note that T and clz are not usually the same ... T might be org.tyranid.profile.User while clz represents com.company.profile.User
  def editing2[ T:Manifest ]( clz:Class[_], gen: => org.tyranid.db.Record ):T = editings.getOrElseUpdate( clz, gen ).asInstanceOf[T]
  
  def doneEditing[ T: Manifest ] = editings.remove( manifest[T].runtimeClass )

  def clearAllEditing = editings.clear


  /*
   * * *   Notifications
   */

  @volatile private var notes:List[Notification] = Nil
  
  def notice( msg:AnyRef, extra:NodeSeq = null, deferred:String = null ) = notes ::= Notification( Notification.NOTICE, msg.toString, cssClass = "alert-success", extra, deferred )
  def warn( msg:AnyRef, extra:NodeSeq = null, deferred:String = null )   = notes ::= Notification( Notification.WARN, msg.toString, cssClass= "alert", extra, deferred )
  def error( msg:AnyRef = "", extra:NodeSeq = null, deferred:String = null )  = notes ::= Notification( Notification.ERROR, msg.toString, cssClass= "alert-error", extra, deferred )

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


/*
 * * *   SessionData
 *
 * Session represents the local Session in RAM in the HttpSession
 * SessionData represents the global Session in MongoDB ... a Session can be reconstituted from a SessionData if a session needs to be migrated
 */

class SessionDataMeta extends MongoEntity( "a04t" ) {
  type RecType >: Null <: SessionData
  override def convert( obj:DBObject, parent:MongoRecord ):RecType = throw new UnsupportedOperationException()

  "_id"                is DbMongoId         is 'id;

  /*
   * IDs
   *
   *
   *   mongo id here
   *
   *   per-server HttpSession id
   *
   *   in-memory tyranid Session id
   *      used for Scribd and NewRelic
   *
   *   ?. merge ObjectId and tyranid session id ?
   *
   */



  "sv"                 is DbChar(32)        as "Server ID";


  // http session or tyranid session ?
  "ss"                 is DbChar(32)        as "Session ID";

  "rh"                 is DbChar(32)        as "Remote Host";
  "ra"                 is DbChar(32)        as "Remote Address";

  "ct"                 is DbDateTime        as "Session Creation Time";
  "lit"                is DbDateTime        as "Login Time";

  "lp"                 is DbChar(64)        as "Last Path";
  "lpt"                is DbDateTime        as "Last Path Time";
  "dom"                is DbChar(32)        as "Website Domain";

  "ua"                 is DbLink(UserAgent) ;

  "exp"                is DbBoolean         as "Expired";

  "incognito"          is DbBoolean         ;

  "unshownPosts"       is DbInt             ;

  override def init = {
    super.init

    "u"                is DbLink(B.User)    as "User";
  }

  def idFor( session:Session ) = {
    val hsid = session.httpSessionId

    assert( hsid != null )

    var rec:SessionData = B.SessionData( B.SessionData.db.findOne( Mobj( "ss" -> hsid ) ) )

    if ( rec == null ) {
      rec = B.newSessionData()

      val http = T.http
      if ( http != null )
        rec( 'ct ) = new Date( http.getCreationTime )
      else
        rec( 'ct ) = new Date

      rec( 'ss ) = hsid
      rec( 'sv ) = Ip.Host.toString
      rec.save
    }

    rec.oid
  }

  /*
     TODO:


    
    
     X.  sync MongoDB-SessionData with In-memory-tyranid.Session

         X.  when http is assigned to ThreadData

         X.  assign "sv" variable

             X. get local ip from somewhere ... where ?

         X.  update the session data in MongoDB as it is updated locally

             X. login/logout

             X. login time

             X. last path access time ?


     X.  change the session list in admin to use this table rather than the local session list
    
     X.  clean up mongo-sessions in SessionCleaner
    
         X.  expired lastModifiedDate on session
    
     X.  change Comet.visit to use the CometQueue

         X.  NAME:  come up with way to name local server that can be encoded in a mongodb collection name

         X.  create capped collection comet_$NAME

         X.  figure out read loop


     X.  scheduled tasks

         X. session cleaner needs to be both server-local (HttpSessions) and one-server (SessionData)

    
    
   */

  // index on "ss" ?
}

trait SessionData extends MongoRecord {
  

  def user = B.User.getById( oid( 'u ) )

  def ua = UserAgent.getById( i( 'ua ) )

  def isIncognito = b( 'incognito )
}


/*
 * * *   Notification
 */

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


