
package org.tyranid.web

import javax.servlet.{ Filter, FilterChain, FilterConfig, GenericServlet, ServletException, ServletRequest, ServletResponse, ServletContext }
import javax.servlet.http.{ HttpServlet, HttpServletRequest, HttpServletResponse }

import scala.collection.mutable
import scala.util.control.ControlThrowable
import scala.xml.{ Elem, Node, NodeSeq, Text, TopScope }

import org.cometd.bayeux.server.BayeuxServer

import org.tyranid.Imp._
import org.tyranid.profile.User
import org.tyranid.session.{ AccessLog, ThreadData }


case class WebException( message:String )          extends Exception

case class WebForwardException( forward:String )   extends ControlThrowable
case class WebRedirectException( redirect:String ) extends ControlThrowable
case class WebIgnoreException                      extends ControlThrowable

trait WebLock {

  def open( ctx:WebContext, td:ThreadData ):Boolean
  def block( ctx:WebContext ):Unit
}

class WebFilter extends Filter {

  var filterConfig:FilterConfig = _


  def init( filterConfig:FilterConfig ) {
    this.filterConfig = filterConfig
    org.tyranid.boot.Boot.boot
  }

  def destroy {
    this.filterConfig = null
  }

  def secureRedirect( ctx:WebContext ) {
    val req = ctx.req
    val qs = req.getQueryString

    val sb = new StringBuilder

    sb ++= "https://"
    sb ++= req.getServerName
    if ( ctx.req.getServerPort == 8080 )
      sb ++= ":8443"
    sb ++= req.getServletPath
    if ( qs.notBlank )
      sb += '?' ++= qs

    ctx.res.sendRedirect( sb.toString )
  }

  def doFilter( request:ServletRequest, response:ServletResponse, chain:FilterChain ):Unit = try {
    val boot = B

    var web = new WebContext( request.asInstanceOf[HttpServletRequest],
                              response.asInstanceOf[HttpServletResponse], filterConfig.getServletContext() )
spam( "filter entered, path=" + web.path )

    if ( boot.requireSsl )
      web.req.getServerPort match {
      case 80 | 8080 => return secureRedirect( web )
      case _ =>
      }

    val thread = T
    thread.http = web.req.getSession( false )
    thread.web = web

    AccessLog.log( web, thread )

    var multipartHandled = false

    for ( pair <- boot.weblets;
          if web.matches( pair._1 ) && pair._2.matches( web ) ) {
      val ( path, weblet ) = pair

      try {
        for ( lock <- weblet.locks )
          if ( !lock.open( web, thread ) )
            return lock.block( web )

        if ( thread.http == null )
          thread.http = web.req.getSession( true )

        if ( !multipartHandled ) {
          web = FileUploadSupport.checkContext( web )
          thread.web = web
          multipartHandled = true
        }

        try {
          weblet.handle( web )
          return // return if it was handled

        } catch {
        case ie:WebIgnoreException =>
          ; // continue on and try the next servlet and/or continue to the chaining
        case re:WebRedirectException =>
          web.res.sendRedirect( re.redirect )
          return
        case fe:WebForwardException =>
          web.ctx.getRequestDispatcher( fe.forward ).forward( web.req, web.res )
          return
        case re:org.tyranid.secure.SecureException =>
          if ( !User.isLoggedIn ) {
            web.res.sendRedirect( "/log/in?l=" + web.req.uriAndQueryString.encUrl )
          } else {
            thread.session.warn( "Access denied." )
            web.res.sendRedirect( "/" )
          }

          return
        case e:Exception =>
          e.log
          return
        }
      }
    }

    chain.doFilter( request, response )
  } catch {
  case t:ControlThrowable =>
    throw t
  case t =>
    t.log
  }
}


case class WebContext( req:HttpServletRequest, res:HttpServletResponse, ctx:ServletContext ) {
  def matches( path:String ) = {
    // TODO:  check for path separators ... i.e. "/foo" should not match "/foobar" but should match "/foo/bar"
    req.getServletPath.startsWith( path )
  }

  def path = req.getServletPath

  def forward( url:String )  = throw WebForwardException( url )

  def redirect( url:String ) = throw WebRedirectException( url )
  
  def template( template:NodeSeq, status:Int = 200 ) =
    res.html( WebTemplate( template ), status )
}

trait Weblet {
  def matches( web:WebContext ) = true

  val locks:List[WebLock] = Nil

  def handle( web:WebContext ):Unit
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
  private var headFound = false

  private def hasTemplates( nodes:NodeSeq ):Boolean = nodes exists hasTemplates
  private def hasTemplates(  node:Node    ):Boolean = ( node.label == "head" || node.prefix == "tyr" ) || hasTemplates( node.child )

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
      heads ++= ( node.child map { case e: Elem => e.copy(scope = TopScope) case n => n } )
      headFound = true

      NodeSeq.Empty

    case t:Text =>
      t

    case other =>
      if ( hasTemplates( node ) )
        new Elem( node.prefix, node.label, node.attributes, node.scope, process( node.child, content ):_* ) 
      else
        node
    }

  def process( xml:NodeSeq, content:NodeSeq = NodeSeq.Empty ):NodeSeq = {
    if ( hasTemplates( xml ) )
      xml.flatMap( node => bindNode( node, content ) )
    else
      xml
  }

  def finish( xml:NodeSeq, content:NodeSeq = NodeSeq.Empty ):NodeSeq = {
    val pxml = process( xml, content )

    if ( headFound ) {

      pxml.flatMap { node =>
        node match {
        case e:Elem if node.label == "html" =>
          val head = <head>{ heads }</head>

          val htmlContents = head ++ node.child

          new Elem( node.prefix, node.label, node.attributes, node.scope, htmlContents:_* )

        case other =>
          other
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

