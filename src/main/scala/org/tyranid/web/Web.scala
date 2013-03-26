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

package org.tyranid.web

import java.io.IOException
import javax.servlet.{ Filter, FilterChain, FilterConfig, GenericServlet, ServletException, ServletRequest, ServletResponse, ServletContext }
import javax.servlet.http.{ HttpServlet, HttpServletRequest, HttpServletResponse }

import scala.collection.mutable
import scala.util.control.ControlThrowable
import scala.xml.{ Elem, Node, NodeSeq, Text, TopScope }

import org.cometd.bayeux.server.BayeuxServer

import org.tyranid.Imp._
import org.tyranid.boot.Bootable
import org.tyranid.db.mongo.Imp._
import org.tyranid.http.UserAgent
import org.tyranid.json.{ JsCmd, Js, JsData, JsModel, JqHtml, JsNop }
import org.tyranid.math.Base64
import org.tyranid.profile.{ LoginCookie, User }
import org.tyranid.session.{ AccessLog, Session, ThreadData, Notification }
import org.tyranid.ui.LnF


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
  
  val assetPattern = java.util.regex.Pattern.compile( "((([^\\s]+(\\.(?i)(ico|jpeg|jpg|png|gif|bmp|js|css|ttf|eot|woff|svg|html|htc|vtt|odt)))|.*robots\\.txt)$)|.*io/thumb.*|.*/sso/.*" )
  
  def notAsset( path:String ) = !assetPattern.matcher( path ).matches
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
      val lnf = LnF.byDomain( web.req.getServerName )
      thread.http = web.req.getSession( true )
      T.session.put( Session.LnF_KEY, lnf )
      LoginCookie.autoLogin          
    }
  }
  
  def completeFilter( boot:Bootable, web:WebContext, chain:FilterChain, thread:ThreadData, comet:Boolean ): Unit
  
  def doFilter( request:ServletRequest, response:ServletResponse, chain:FilterChain ):Unit = try {
    val boot = B
    
    var web = new WebContext( request.asInstanceOf[HttpServletRequest],
                              response.asInstanceOf[HttpServletResponse], filterConfig.getServletContext() )
    
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

    val comet = web.path.endsWith( "/cometd" )
    
    if ( !comet )
      println( "  | " + web.path + ( !B.DEV |* ", referer: " + web.req.getHeader( "referer" ) ) )

    val thread = T
    thread.http = web.req.getSession( false )
    thread.web = web

    completeFilter( boot, web, chain, thread, comet )
  } catch {
  case t:ControlThrowable =>
    throw t
  case t =>
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

  override def completeFilter( boot:Bootable, web:WebContext, chain:FilterChain, thread:ThreadData, comet:Boolean ):Unit = {  
    if ( !comet && ( thread.http == null || !B.User.isLoggedIn ) ) {
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
      T.session.login( user )
    }
    
    //web.req.setAttribute( "removeFromPath", "/api" )
    web.req.setAttribute( "api", true )
    
    chain.doFilter( web.req, web.res )
    
    if ( T.session != null ) T.session.logout()
  }
}

class WebFilter extends TyrFilter {
  override def completeFilter( boot:Bootable, webr:WebContext, chain:FilterChain, thread:ThreadData, comet:Boolean ):Unit = {
    var web = webr
    val isAsset = !comet && !WebFilter.notAsset( web.path )

    if ( !comet && thread.http != null ) {
      val session = T.session
      
      if ( session != null && !isAsset ) {
        session.put( "lastPath", web.path )
        session.put( "lastPathTime", new java.util.Date() )
        session.put( Session.LnF_KEY, LnF.byDomain( web.req.getServerName ) )
      }
    }
    
    var first = true

    val path = web.path
    
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
        
        // If it is an asset, then just return 404 
        if ( isAsset ) {
          web.res.sendError( HttpServletResponse.SC_NOT_FOUND )
          return false
        }
        
        web.ctx.getRequestDispatcher( "/404" ).forward( web.req, web.res )
      case re:org.tyranid.secure.SecureException =>
        if ( !B.User.isLoggedIn ) {
          val redirectUrl = web.req.uriAndQueryString.replaceAll( "&?xhr=1", "" )
          web.res.sendRedirect( "/log/in?l=" + redirectUrl.encUrl + ( web.b( 'xhr ) ? "&xhr=1" | "" ) )
        } else {
          thread.session.warn( "Access denied." )
          web.res.sendRedirect( "/" + ( web.b( 'xhr ) ? "?xhr=1" | "" ) )
        }
      case e:Exception =>
        println( "GOT EXCEPTION: " + e.getMessage() )
        e.log
        
        T.session.error( "Sorry, there seems to be some difficulties with this page.  Our technical staff have been notified that you have encountered this issue. Please visit <a href='http://rbsupport.volerro.com/' target='_blank'>http://rbsupport.volerro.com</a> if this is an urgent issue." )
        web.b( 'xhr ) ? web.jsRes() | web.template( <tyr:errorPage/> )
      }

      return true
    }

    val start = System.currentTimeMillis

    try {
      for ( webloc <- boot.weblocs;
            if web.matches( webloc.weblet.wpath ) && webloc.weblet.matches( web ) ) {
        
        if ( !comet ) ensureSession( thread, web )
        
        //println( !web.b( 'xhr ) ) 
        //println( !isAsset )
        //println( T.user == null || !T.user.loggedIn )
        //println( T.LnF == LnF.RetailBrand )
        //println( comet )
        
        if ( web.b( 'asp ) || ( !web.b( 'xhr ) && !isAsset && ( T.user == null || !T.user.loggedIn ) ) && T.LnF == LnF.RetailBrand && !comet && web.req.getAttribute( "api" )._s.isBlank ) {
          //println( "full shell page!" )
          
          web.template( B.appShellPage( web ) )
          return
        }

        if ( first ) {
          web = FileUploadSupport.checkContext( web )
          thread.web = web
          first = false
        }

        if ( handle( webloc ) ) {
          //spam( "CACHE: CLEARING" )
          thread.tidCache.clear
          thread.requestCache.clear
          return
        }
      }
    } finally {
      AccessLog.log( web, thread, System.currentTimeMillis - start )
    }

    chain.doFilter( web.req, web.res )
  }
}

class WebResponse( web:WebContext, sess:Session ) {
  var redirect:String = null
  val EMPTY_LIST = List[Notification]()
  
  lazy val notices = ( notifications ? sess.popNotices | EMPTY_LIST )
  lazy val warnings = ( notifications ? sess.popWarnings | EMPTY_LIST )
  lazy val errors = ( notifications ? sess.popErrors | EMPTY_LIST )
  
  def hasWarnings = warnings.length > 0 
  def hasErrors = errors.length > 1 

  val cmds = mutable.Buffer[JsCmd]()
  
  var notifications = true
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
        cmds.isEmpty ? main | Array( cmds.map( cmdToMap ).filter( _ != null ) += main )
      ).toJsonStr( pretty = printPretty, client = true )
    
    if ( printPretty )
      println( jsonStr )
      
    jsonStr
  }

  private def joinJsCmds {
    val req = web.req
    
    if ( req != null ) {
      val jsCmds = req.getAttribute( "jscmds" ).as[mutable.Buffer[JsCmd]]
      
      if ( jsCmds != null ) {
        cmds ++= jsCmds
        req.removeAttribute( "jscmds" )
      }
    }
  }
  
  private def cmdToMap( cmd:JsCmd ) =
    cmd match {
    case cmd:JqHtml =>
      val htmlMap = mutable.Map[String,Any]()

      htmlMap( "html" )   = cmd.html
      htmlMap( "target" ) = cmd.target

      if ( cmd.modal.notBlank )
        htmlMap( "modal" ) = cmd.modal

      if ( cmd.transition.notBlank ) {
        htmlMap( "transition" ) = cmd.transition
        htmlMap( "duration" )   = cmd.duration.toString
      }

      if ( cmd.handler.notBlank )
        htmlMap( "handler" ) = cmd.handler

      Map( "html" -> htmlMap )

    case cmd:Js      => Map( "extraJS" -> cmd.js )
    case cmd:JsData  => Map( "data" -> cmd.data )
    case cmd:JsModel => if ( cmd.name.notBlank ) Map( "model" -> cmd.map, "modelName" -> cmd.name )
                        else                     Map( "model" -> cmd.map )
    case JsNop | null => null
    }
}

case class WebContext( req:HttpServletRequest, res:HttpServletResponse, ctx:ServletContext ) {
  var upload = false
  def jsonRes( sess:Session ) = new WebResponse( this, sess )

  // TODO:  eliminate "def js" and "tyr.js" since it is redundant with this way of doing it, then rename this to "js"
  def jsRes( js:JsCmd* ) = {
    val res = jsonRes( T.session )
    res.cmds ++= js
    json( res )
  }

  def isApi = req.getAttribute( "api" ) != null
  
  def jsResNoNotifications( js:JsCmd* ) = {
    val res = jsonRes( T.session )
    res.notifications = false
    res.cmds ++= js
    json( res )
  }

  def matches( p:String ) = {
    // TODO:  check for path separators ... i.e. "/foo" should not match "/foobar" but should match "/foo/bar"
    path.startsWith( p ) && {
       val plen = p.length
       ( path.length == plen || path.charAt( plen ) == '/' )
    }
  }

  def ip = req.getRemoteAddr

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

  def forward( url:String )  = throw WebForwardException( url )

  def redirect( url:String ) = {
    assert( !url.endsWith( "/null" ) )
    throw WebRedirectException( url )
  }
  
  def template( template:NodeSeq, status:Int = 200 ) =
    res.html( WebTemplate( template ), status )
    
  def s( param:String ):String        = req.s( param )
  def i( param:String ):Int           = req.i( param )
  def d( param:String ):Double        = req.d( param )
  def l( param:String ):Long          = req.l( param )
  def b( param:String ):Boolean       = req.b( param )
  def oid( param:String )             = req.oid( param  )
  def a( param:String ):Seq[String]   = req.a( param )
  def a_?( param:String ):Seq[String] = req.a_?( param )

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
  
  def redirectIfNotLoggedIn( web:WebContext ) = 
    if ( !B.User.isLoggedIn )
      web.redirect( "/log/in?l=" + web.req.uriAndQueryString.encUrl + ( web.b( 'xhr ) ? "&xhr=1" | "" ) )

  def redirectIfNotHasOrg( web:WebContext ) = {
    redirectIfNotLoggedIn( web )
    
    if ( !org.tyranid.session.Session().user.has( 'org ) )
      web.redirect( "/" + ( web.b( 'xhr ) ? "?xhr=1" | "" ) )
  }
     
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

  def shell( content:NodeSeq ):Unit =
    if ( parent != null )
      parent.shell( content )
    else
      T.web.template(
        content
      )

  def _404 = throw new Web404Exception
  
  def getFormParam( formParams: collection.Map[String, Seq[String]], n: String, default: String = null ) = {
    val list = formParams.get(n)

    if ( list == null || list.size == 0 ) {
      default
    } else {
      val v = list.get( 0 )

      if ( v.notBlank ) v else default
    }
  }
}


object WebTemplate {
  /*

     +.  error message substitution

     +.  intelligent javascript includes

     +.  better caching / performance:
     
         static vs. dynamic
         has templates vs. template-less

   */

  def apply( xml:NodeSeq, content:NodeSeq = NodeSeq.Empty ):NodeSeq =
    new WebTemplate().finish( xml, content )
}

class WebTemplate {

  private val heads = new mutable.ArrayBuffer[Node]()
  private val tops  = new mutable.ArrayBuffer[Node]()
  private val tails = new mutable.ArrayBuffer[Node]()

  private def hasTemplates( nodes:NodeSeq ):Boolean = nodes exists hasTemplates
  private def hasTemplates(  node:Node    ):Boolean = ( node.label == "head" || node.label == "top" || node.label == "tail" || node.prefix == "tyr" ) || hasTemplates( node.child )

  private def bindNode( node:Node, content:NodeSeq ):NodeSeq =
    node match {
    case e:Elem if node.prefix == "tyr" =>
    
      if ( node.label == "content" ) {
        process( content )
      } else {
        val template = B.templates.find( p => p._1 == node.label ).map( _._2 ) getOrElse ( throw new WebException( "Missing template " + node.label ) )
        process( template( node ), e.child )
      }

    case e:Elem if node.label == "head" =>
      val id = node.\( "@id" ).text
      if ( id.isBlank || !heads.exists( _.\( "@id" ).text == id ) )
        heads ++= node.child.flatMap( node => bindNode( node, content ) )
        //heads ++= ( node.child map { case e:Elem => e.copy(scope = TopScope) case n => n } )
      NodeSeq.Empty

    case e:Elem if node.label == "top" =>
      val id = node.\( "@id" ).text
      if ( id.isBlank || !tops.exists( _.\( "@id" ).text == id ) )
        tops ++= ( node.child map { case e:Elem => e.copy(scope = TopScope) case n => n } )
      NodeSeq.Empty

    case e:Elem if node.label == "tail" =>
      val id = node.\( "@id" ).text
      if ( id.isBlank || !tails.exists( _.\( "@id" ).text == id ) )
        tails ++= ( node.child map { case e:Elem => e.copy(scope = TopScope) case n => n } )
      NodeSeq.Empty

    case e:Elem =>
      if ( hasTemplates( node ) )
        e.copy( child = process( node.child, content ) ) 
      else
        node

    case n =>
      n
    }

  def process( xml:NodeSeq, content:NodeSeq = NodeSeq.Empty ):NodeSeq = {
    if ( hasTemplates( xml ) )
      xml.flatMap( node => bindNode( node, content ) )
    else
      xml
  }

  def finish( xml:NodeSeq, content:NodeSeq = NodeSeq.Empty ):NodeSeq = {
    val pxml = process( xml, content )

    if ( heads.nonEmpty || tops.nonEmpty || tails.nonEmpty ) {
      pxml.flatMap { node =>
        node match {
        case e:Elem if node.label == "html" =>
          val content =
            ( heads.nonEmpty |* <head>{ heads }</head> ) ++
            ( if ( tops.nonEmpty || tails.nonEmpty )
                node.child.flatMap { node =>
                  node match {
                  case e:Elem if node.label == "body" => e.copy( child = ( tops ++ node.child ++ tails ) )
                  case n                              => n
                  }
                }
              else
                node.child )

          e.copy( child = content )

        case n => n
        }
      }

    } else {
      pxml
    }
  }
}


/*
 * This initialization is done inside of a servlet because we need to make sure that the CometServlet has initialized already and
 * using a servlet allows us to use the web.xml load-on-startup mechanism to achieve this ordering.  Another solution would be to set
 * up a web.xml listener.
 */
class WebInit extends GenericServlet {

  override def init {
    val bayeux = getServletContext().getAttribute(BayeuxServer.ATTRIBUTE).as[BayeuxServer]
    B.bayeux = bayeux
    B.comets.foreach { _.init( bayeux ) }
  }

  def service( req:ServletRequest, res:ServletResponse ) = {
    throw new ServletException
  }
}

