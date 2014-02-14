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

package org.tyranid.web

import com.newrelic.api.agent.NewRelic

import java.io.IOException
import java.util.Date

import javax.servlet.{ Filter, FilterChain, FilterConfig, GenericServlet, ServletException, ServletRequest, ServletResponse, ServletContext }
import javax.servlet.http.{ HttpServletRequest, HttpServletResponse }

import scala.collection.mutable
import scala.util.control.ControlThrowable
import scala.xml.{ Elem, Node, NodeSeq, Text, TopScope }

import org.tyranid.Imp._
import org.tyranid.boot.Bootable
import org.tyranid.db.mongo.Imp._
import org.tyranid.http.UserAgent
import org.tyranid.json.{ JsCmd, JsCmds, Js, JsData, JsModel, JqHtml, JsNop, JsValue }
import org.tyranid.math.Base64
import org.tyranid.profile.{ LoginCookie, User }
import org.tyranid.session.{ AccessLog, Session, ThreadData, Notification }

case class WebException( message:String )          extends Exception

case class WebForwardException( forward:String )   extends ControlThrowable
case class WebRedirectException( redirect:String ) extends ControlThrowable

/*
 * Throw this when you want to forward the handling on to the next weblet.
 */
case class WebIgnoreException()                    extends ControlThrowable

case class WebHandledException()                   extends ControlThrowable

/*
 * Throw this when you want to trigger a 404, and not pass the handling on to another weblet.
 */
case class Web404Exception()                       extends ControlThrowable


object WebFilter {
  val versionPattern = "^/v[0-9]+/.*".r
  val maintPattern = "/maintenance.html".r
  val robotsPattern = "/robots.txt".r

  val assetPattern = java.util.regex.Pattern.compile( "((([^\\s]+(\\.(?i)(ico|jpeg|jpg|png|gif|bmp|js|css|ttf|eot|woff|svg|html|map|htc|vtt|odt|wav|map)))|.*robots\\.txt)$)|.*io/thumb.*|.*/sso/.*" )

  def notAsset( path:String ) = !assetPattern.matcher( path ).matches

  def setSessionVars( web:WebContext ) {
    val sess = T.session

    sess.record( 
     "rh" -> web.req.getRemoteHost,
     "ra" -> T.ip        
    )

    sess.put( "subdomain", web.req.getServerName )
  }
}

trait TyrFilter extends Filter {
  var filterConfig:FilterConfig = _

  def init( filterConfig:FilterConfig ) {
    this.filterConfig = filterConfig
    org.tyranid.boot.Boot.boot
  }

  def secureRedirect( web:WebContext ) {
    val qs = web.req.getQueryString

    val sb = new StringBuilder

    sb ++= "https://"
    sb ++= web.req.getServerName
    if ( web.req.getServerPort == 8080 )
      sb ++= ":8443"
    sb ++= web.path
    if ( qs.notBlank )
      sb += '?' ++= qs

    web.res.sendRedirect( sb.toString )
  }

  def destroy {
    this.filterConfig = null
  }

  def ensureSession( thread:ThreadData, web:WebContext ) {
    if ( thread.http == null ) {
      thread.http = web.req.getSession( true )
      WebFilter.setSessionVars( web )
      LoginCookie.autoLogin
    }
  }
  
  def completeFilter( boot:Bootable, web:WebContext, chain:FilterChain, thread:ThreadData ): Unit

  def doFilter( request:ServletRequest, response:ServletResponse, chain:FilterChain ):Unit = try {
    var web = new WebContext( request.asInstanceOf[HttpServletRequest],
                              response.asInstanceOf[HttpServletResponse], filterConfig.getServletContext() )

    val boot = B

    val removeFromPath = web.req.getAttribute( "removeFromPath" )._s

    if ( removeFromPath.notBlank )
      web.setPath( web.path.replaceAll( removeFromPath, "" ) )

    if ( boot.requireSsl )
      web.req.getServerPort match {
      case 80 | 8080 =>
        if ( request.getServerName.contains( B.domain ) && !B.secureBypass( web.fullPath ) )
          return secureRedirect( web )
      case _ =>
      }

    println( "  | " + web.path + ( !B.DEV |* ", referer: " + web.req.getHeader( "referer" ) ) )

    val thread = T
    thread.http = web.req.getSession( false )
    thread.web = web

    completeFilter( boot, web, chain, thread )
  } catch {
  case t:ControlThrowable =>
    throw t
  case t:Throwable =>
    t.log
  }
}

class BasicAuthFilter extends TyrFilter {

  def getUser( email:String, pw:String ): User = {
    val users = B.User.db.find( Mobj( "email" -> ("^" + email.encRegex + "$").toPatternI,
                   $or -> Array(
                     Mobj( "inactive" -> Mobj( $exists -> false ) ),
                     Mobj( "inactive" -> false ) )
                   ) ).toSeq

    for ( u <- users; dbPw = u.s( 'password ) )
      if ( dbPw.notBlank && pw.checkShash( dbPw ) )
        return B.User( u )

    return null
  }

  override def completeFilter( boot:Bootable, web:WebContext, chain:FilterChain, thread:ThreadData ):Unit = {
    if ( thread.http == null || !T.session.isAuthenticated ) {
      val header = web.req.getHeader( "Authorization" )

      if ( header.isBlank ) {
        web.res.setHeader("WWW-Authenticate","Basic realm=\"" + B.applicationName + " Authorization\"")
        web.res.sendError( HttpServletResponse.SC_UNAUTHORIZED )
        return
      }

      if ( header.substring(0, 6) != "Basic " )
        throw new IOException( "Basic Authentication is the only authentication supported.  Found: " + header.substring( 0, 6 ) )

      val basicAuthEncoded = header.substring(6)
      val basicAuthAsString = Base64.decode( basicAuthEncoded.getBytes() ) // user:pass
      val email = basicAuthAsString.prefix( ':' )
      val password = basicAuthAsString.substring( basicAuthAsString.indexOf( ':' ) + 1 )
      val user = getUser( email, password )

      if ( user == null ) {
        web.res.setStatus( HttpServletResponse.SC_UNAUTHORIZED )
        return
      }

      ensureSession( thread, web )
      T.session.login( user, setAuth = true )
    }

    //web.req.setAttribute( "removeFromPath", "/api" )
    web.req.setAttribute( "api", true )

    chain.doFilter( web.req, web.res )

    T.session.logout()
  }
}

class WebFilter extends TyrFilter {
  override def completeFilter( boot:Bootable, webr:WebContext, chain:FilterChain, thread:ThreadData ):Unit = {
    var web = webr
    val isAsset = !WebFilter.notAsset( web.path )

    if ( !isAsset ) {
      val session = T.session

      if ( session != null && T.http != null && !isAsset ) {
        val now = new Date
        val path = web.path
        val subdomain = web.req.getServerName

        session.put( "lite", subdomain.startsWith( B.liteDomainPart ) )
        session.record( "lp" -> path, "lpt" -> now, "dom" -> subdomain )
        session.put( "subdomain", subdomain )
        //sess.put( "subdomain", web.req.getServerName )
      }
    }

    var first = true
    val path = web.path

    if ( B.maintenanceMode && !path.matches( WebFilter.maintPattern ) & !isAsset ) {
      web.ctx.getRequestDispatcher( "/maintenance.html" ).forward( web.req, web.res )
      return
    } else if ( isAsset && path.matches( WebFilter.robotsPattern ) ) {
      if ( web.req.getServerName.startsWith( B.liteDomainPart ) ) {
        web.ctx.getRequestDispatcher( "/robots_" + B.liteDomainPart + ".txt" ).forward( web.req, web.res )
        return
      }
    }

    if ( path.matches( WebFilter.versionPattern ) ) {
      val slash = path.indexOf( '/', 1 )

      if ( slash != -1 ) {
        web.ctx.getRequestDispatcher( path.substring( slash ) ).forward( web.req, web.res )
        return
      }
    } else if ( web.path == "/null" ) {
      // handle errant requests (the swf from video.js does this)
      web.res.setStatus( HttpServletResponse.SC_OK )
      return
    }

    def handle( webloc:Webloc ):Boolean = {
      for ( cwebloc <- webloc.children if web.matches( cwebloc.weblet.wpath ) && cwebloc.weblet.matches( web ) )
        if ( handle( cwebloc ) )
          return true

      try {
        webloc.weblet.handle( web )
      } catch {
      case he:WebHandledException =>
        return true
      case ie:WebIgnoreException =>
        return false
      case re:WebRedirectException =>
        if ( !web.res.isCommitted )
          web.res.sendRedirect( re.redirect )
      case fe:WebForwardException =>
        web.ctx.getRequestDispatcher( fe.forward ).forward( web.req, web.res )
      case fe:Web404Exception =>
        println( "404: " + webloc.path )
        // "" means multiple weblets can handle this path
        if ( webloc.path == "" )
          return false

        // If it is an asset, then just return forbidden (404 will just redirect to our error weblet and we don't want that)
        if ( isAsset ) {
          web.res.sendError( HttpServletResponse.SC_FORBIDDEN )
          return true
        }

        web.ctx.getRequestDispatcher( "/404" ).forward( web.req, web.res )
      case re:org.tyranid.secure.SecureException =>
        if ( thread.session != null ) {
          thread.session.warn( "Access denied (3)." )
          val redirectUrl = web.req.uriAndQueryString.replaceAll( "&?xhr=1", "" )
          web.forward( path = redirectUrl.encUrl )
        }
      case e:Exception =>
        println( "GOT EXCEPTION: " + e.getMessage() )
        e.log

        T.session.error( "Sorry, there seems to be some difficulties with this page.  Our technical staff have been notified that you have encountered this issue. Please visit <a href='http://rbsupport.volerro.com/' target='_blank'>http://rbsupport.volerro.com</a> if this is an urgent issue." )
        web.xhr ? web.jsRes() | web.forward( "/error?full=1" )
      }

      return true
    }

    val start = System.currentTimeMillis

    try {
      for ( webloc <- boot.weblocs;
            if web.matches( webloc.weblet.wpath ) && webloc.weblet.matches( web ) ) {
        
        val comet = web.fullPath.startsWith( "/comet" )
        
        if ( comet ) {
          Errorlet.stopComet( web )
          return
        }
        
        if ( !isAsset ) ensureSession( thread, web )

        val t = T
        val sess = t.session
        val user = t.user

        if ( sess.trace ) {
          val hasUser = user != null
          NewRelic.setUserName( hasUser ? user.fullName | "[Unknown]" )
          NewRelic.setAccountName( hasUser ? user.tid | "[None]" )
          NewRelic.setProductName( sess.id.toString )
        }

        /*
        println( sess.id + ":" + sess.isLite )

        if ( !isAsset ) {
          println( "asp: " + web.b( 'asp ) )
          println( "xhr: " + web.b( 'xhr ) )
          println( "user: " + T.user )
          if ( T.user != null ) println( "user li: " + sess.isLoggedIn )
        }
        println( "isAsset: " + isAsset )
        */

        if ( ( !web.b( 'xhr ) && ( !isAsset && ( T.user == null || !sess.isVerified ) && webloc.weblet.requiresLogin ) )
            && web.req.getAttribute( "api" )._s.isBlank ) {

          if ( isAsset ) spam( "isAsset matching on " + web.path )

          web.checkDebug( sess )
                
          web.forward()
          return
        }

        if ( first ) {
          web = FileUploadSupport.checkContext( web )
          t.web = web
          first = false
        }

        if ( handle( webloc ) ) {
          //sp am( "CACHE: CLEARING" )
          t.tidCache.clear
          t.requestCache.clear
          return
        }
      }
    } finally {
      AccessLog.log( web, thread, System.currentTimeMillis - start )
    }

    chain.doFilter( web.req, web.res )
  }
  
  override def destroy {
    super.destroy
    B.SHUTTINGDOWN = true
  }
}

class WebResponse( web:WebContext, sess:Session ) {

  var redirect:String = null
  val EMPTY_LIST = List[Notification]()
  var notifications = true

  lazy val notices = ( notifications ? sess.popNotices | EMPTY_LIST )
  lazy val warnings = ( notifications ? sess.popWarnings | EMPTY_LIST )
  lazy val errors = ( notifications ? sess.popErrors | EMPTY_LIST )

  def hasWarnings = warnings.length > 0
  def hasErrors = errors.length > 1

  val cmds = mutable.Buffer[JsCmd]()

  var extraJS:String = null
  var htmlMap:collection.Map[String,Any] = null
  var variables:Map[String,Any] = null

  def toJsonStr = toPJsonStr( false )
  def toPJsonStr( pretty:Boolean ) = {
    joinJsCmds

    val main =
      Map( "notices" -> notices,
           "warnings" -> warnings,
           "errors" -> errors,
           "redirect" -> redirect,
           "html" -> htmlMap,
           "extraJS" -> extraJS,
           "vars" -> variables )

    val printPretty = pretty || ( web.req != null && web.req.getAttribute( "json.pretty" )._b )
    val jsonStr = (
        cmds.isEmpty ? main | Array( cmds.filter( _ != null ).map( _.toMap ).filter( _ != null ) += main )
      ).toJsonStr( pretty = printPretty, client = true )

    if ( printPretty )
      println( jsonStr )

    jsonStr
  }

  private def joinJsCmds {
    val req = web.req

    if ( req != null ) {
      // If I have requested common and I have not sent it, then get it now and send it.
      if ( T.requestCache.getOrElse( "req-common", false ).as[Boolean] && !T.requestCache.getOrElse( "sent-common", false ).as[Boolean] ) {
        req.addJsCmd( JsData( T.user ) )
        req.addJsCmd( JsModel( T.user.toClientCommonMap( true ), "common" ) )
      }

      val jsCmds = req.getAttribute( "jscmds" ).as[mutable.Buffer[JsCmd]]

      if ( jsCmds != null ) {
        cmds ++= jsCmds
        req.removeAttribute( "jscmds" )
      }
    }
  }
}

case class WebContext( req:HttpServletRequest, res:HttpServletResponse, ctx:ServletContext ) {
  var upload = false
  def jsonRes( sess:Session ) = new WebResponse( this, sess )

  // TODO:  eliminate "def js" and "tyr.js" since it is redundant with this way of doing it, then rename this to "js"
  def jsRes( cmds:JsCmd* ) = {
    val res = jsonRes( T.session )

    def addCmds( cmds:Seq[JsCmd] ) {
      for ( cmd <- cmds )
        cmd match {
        case cmds:JsCmds =>
          addCmds( cmds.cmds )

        case _ =>
          res.cmds += cmd
        }
    }

    addCmds( cmds )

    json( res )
  }

  def addJsCookie( js:String, name:String = "execJS" ) {
    val cookie = new javax.servlet.http.Cookie( name, js.encUrl )
    cookie.setPath("/")
    cookie.setSecure( true )
    res.addCookie( cookie )
  }

  def forward( path:String = "/index.html", js:String = null ) = {
    if ( xhr ) {
      println( "js: " + js )
      new RuntimeException().printStackTrace();
      if ( js.isBlank )
        new RuntimeException( "Have XHR request, but don't have JS to respond with." ).printStackTrace()

      jsRes( Js( js ) )
    } else {
      if ( js.notBlank )
        addJsCookie( js )

      if ( path == "/index.html" )
        res.setNoCacheHeaders

      ctx.getRequestDispatcher( path ).forward( req, res )
    }
  }

  /*
  def forwardRes( path:String = "/index.html", cmds:Seq[JsCmd] = null ) = {
    val res = jsonRes( T.session )

    def addCmds( cmds:Seq[JsCmd] ) {
      for ( cmd <- cmds )
        cmd match {
        case cmds:JsCmds =>
          addCmds( cmds.cmds )

        case _ =>
          res.cmds += cmd
        }
    }

    if ( cmds != null )
      addCmds( cmds )

    forwardJson( res, path = path )
  }

  def forwardJson( json:Any, status:Int = 200, headers:Map[String,String] = null, cache:Boolean = false, path:String = "/index.html" ) = {
    res.setStatus( status )

    if ( !cache ) res.setNoCacheHeaders

    if ( headers != null )
      for ( h <- headers )
        res.setHeader( h._1, h._2 )

    val pretty = {
      val prettyAttr = ( req == null ) ? null | req.getAttribute( "json.pretty" )
      ( prettyAttr == null ) ? false | prettyAttr._b
    }

    val outputJson = if ( json == null ) "{}" else json.toJsonStr( pretty = pretty, client = true )

//    res.setContentLength( if ( jsonpCallback != null ) ( jsonpCallback.length + 2 + outputJson.length ) else outputJson.length )

    val cookie = new javax.servlet.http.Cookie( "json", outputJson )
    cookie.setPath("/")
    cookie.setSecure( true )
    res.addCookie( cookie )
    ctx.getRequestDispatcher( path ).forward( req, res )
  }
  */

  def jsRes( value:collection.Map[String,Any], cmds:JsCmd* ) {
    val res = jsonRes( T.session )

    res.cmds += JsValue( value )

    for ( cmd <- cmds )
      cmd match {
      case cmds:JsCmds =>
        res.cmds ++= cmds.cmds

      case _ =>
        res.cmds += cmd
      }

    json( res )
  }

  def xhr = b( 'xhr )

  def isApi = req.getAttribute( "api" ) != null

  def jsResNoNotifications( js:JsCmd* ) = {
    val res = jsonRes( T.session )
    res.notifications = false

    for ( cmd <- js )
      cmd match {
      case cmds:JsCmds =>
        res.cmds ++= cmds.cmds

      case _ =>
        res.cmds += cmd
      }

    json( res )
  }

  def matches( p:String ) = {
    // TODO:  check for path separators ... i.e. "/foo" should not match "/foobar" but should match "/foo/bar"
    path.startsWith( p ) && {
       val plen = p.length
       ( path.length == plen || path.charAt( plen ) == '/' )
    }
  }

  def ip = {
    val ra = req.getHeader( "X-Forwarded-For" )
    ra.isBlank ? req.getRemoteAddr | ra
  }

  var _path:String = null

  def setPath( p:String ) {
    _path = p
  }

  def path = {
    if ( _path.isBlank )
      _path = req.getServletPath

    _path
  }

  def fullPath = path + ( req.getQueryString != null |* "?" + req.getQueryString )

  //def forward( url:String )  = throw WebForwardException( url )

  def redirect( url:String ) = {
    assert( !url.endsWith( "/null" ) )
    throw WebRedirectException( url )
  }

  def checkDebug( sess:Session ) {
    val dbg = s( 'debugit )

    if ( dbg.notBlank )
      sess.debug = dbg._b

    val trace = s( 'trace )

    if ( trace.notBlank )
      sess.trace = trace._b
  }

  def s( param:String ):String        = req.s( param )
  def i( param:String ):Int           = req.i( param )
  def d( param:String ):Double        = req.d( param )
  def l( param:String ):Long          = req.l( param )
  def b( param:String ):Boolean       = req.b( param )
  def t( param:String ):Date          = req.t( param )
  def oid( param:String )             = req.oid( param  )
  def a( param:String ):Seq[String]   = req.a( param )
  def a_?( param:String ):Seq[String] = req.a_?( param )

  def jsobj( param:String )           = req.s( param ).parseJson.asJsonObject
  def dbobj( param:String ):com.mongodb.DBObject = req.s( param ).parseJson.asJsonObject.toDBObject.as[com.mongodb.DBObject]

  def sOpt( param:String ) = {
    val v = s( param )
    v.notBlank |* Some( v )
  }

  def file( param:String ):org.apache.commons.fileupload.FileItem = req.file( param )

  def json( json:Any, status:Int = 200, jsonpCallback:String = null, headers:Map[String,String] = null, cache:Boolean = false ) =
    res.json( json, status, jsonpCallback, headers, req, cache )

  def js( js:JsCmd* ) = res.json( js, req = req, cache = false )

  def html( xml:NodeSeq, status:Int = 200, headers:Map[String,String] = null, noCache:Boolean = false ) = res.html( xml, status, headers, req, noCache )
  def rss ( xml:NodeSeq, status:Int = 200, headers:Map[String,String] = null, noCache:Boolean = false ) = res.rss ( xml, status, headers, req, noCache )

  def userAgentStr = req.getHeader( "User-Agent" )
  def userAgentId = UserAgent.idFor( userAgentStr )
}


case class WebPath( path:String, logParams:Seq[String] )

case class Webloc( path:String, weblet:Weblet, children:Webloc* ) {

  weblet.init( path )
  children.foreach { _.weblet.initParent( weblet ) }
}

trait Weblet {
  val rootPath = "/"

  def redirectIfNotAuthenticated( web:WebContext ) = {
    val sess = Session()

    if ( !sess.isAuthenticated ) {
      if ( sess.isVerified ) {
        val subdomain = web.req.getServerName
        val lite = subdomain.startsWith( B.liteDomainPart )

        if ( lite ) {
          val js = "mainLoad( function() { router.navigate( '#lite', { error : 'Access Denied (1).' } ); } );"

          if ( web.xhr ) {
            web.jsRes( Js( js ) )
          } else {
            web.forward( js = js  )
          }

          throw WebHandledException()
        }
      }

      web.redirect( "/log/in?l=" + web.req.uriAndQueryString.encUrl + ( web.b( 'xhr ) ? "&xhr=1" | "" ) )
    }
  }

  def redirectIfNotVerified( web:WebContext ) = {
    val sess = Session()

    if ( !sess.isVerified ) {
      val subdomain = web.req.getServerName
      val lite = subdomain.startsWith( B.liteDomainPart )

      if ( lite ) {
        val js = "mainLoad( function() { router.navigate( '#lite', { error : 'Access Denied (2).' } ); } );"

        if ( web.xhr ) {
          web.jsRes( Js( js ) )
        } else {
          web.forward( js = js  )
        }

        throw WebHandledException()
      } else {
        web.redirect( "/log/in?l=" + web.req.uriAndQueryString.encUrl + ( web.b( 'xhr ) ? "&xhr=1" | "" ) )
      }
    }
  }

  def redirectIfNotHasOrg( web:WebContext ) = {
    redirectIfNotVerified( web )

    if ( !org.tyranid.session.Session().user.has( 'org ) )
      web.redirect( "/" + ( web.b( 'xhr ) ? "?xhr=1" | "" ) )
  }

  val requiresLogin = true

  def matches( web:WebContext ) = true

  def handle( web:WebContext ):Unit

  def init( webletPath:String ) {
    this.webletPath = webletPath
  }

  def initParent( weblet:Weblet ) {
    this.parentWeblet = weblet
    this.webletPath = parent.wpath + this.webletPath
  }

  private var webletPath:String = _
  private var parentWeblet:Weblet = _

  def parent = parentWeblet

  /*
   * Weblet path
   */
  def wpath = webletPath

  /*
   * Relative path.  Full path = wpath + rpath
   */
  def rpath = {
    val path = T.web.path

    if ( path.startsWith( webletPath ) ) {
      val p = path.substring( webletPath.length )
      if ( p == "" )
        "/"
      else
        p
    } else {
      null
    }
  }

  def _404 = throw new Web404Exception

  def getFormParam( formParams: collection.Map[String, Seq[String]], n: String, default: String = null ) = {
    if ( formParams == null ) {
      null
    } else {
      val list = formParams.get(n)

      if ( list == null || list.size == 0 ) {
        default
      } else {
        val v = list.get( 0 )

        if ( v.notBlank ) v else default
      }
    }
  }
}


