
package org.tyranid.web

import javax.servlet.{ Filter, FilterChain, FilterConfig, GenericServlet, ServletException, ServletRequest, ServletResponse, ServletContext }
import javax.servlet.http.{ HttpServlet, HttpServletRequest, HttpServletResponse }

import scala.xml.{ Elem, Node, NodeSeq, Text }

import org.cometd.bayeux.server.BayeuxServer

import org.tyranid.Imp._
import org.tyranid.session.{ AccessLog, ThreadData }


case class WebException( message:String ) extends Exception

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

  def doFilter( request:ServletRequest, response:ServletResponse, chain:FilterChain ) {

    val tyr = Tyr

    val ctx = new WebContext( request.asInstanceOf[HttpServletRequest],
                              response.asInstanceOf[HttpServletResponse], filterConfig.getServletContext() )

    if ( tyr.requireSsl )
      ctx.req.getServerPort match {
      case 80 | 8080 => return secureRedirect( ctx )
      case _ =>
      }

    val thread = ThreadData()
    thread.http = ctx.req.getSession( false )

    AccessLog.log( ctx, thread.http, thread.tyr )
    
    tyr.weblets.find( pair => ctx.matches( pair._1 ) && pair._2.matches( ctx ) ) match {
    case Some( ( path, weblet ) ) =>
      try {
        weblet.handle( FileUploadSupport.checkContext( ctx ) )
      } catch {
      case e =>
        e.log
      }

    case None =>
      chain.doFilter(request, response);
    }
  }
}


case class WebContext( req:HttpServletRequest, res:HttpServletResponse, servletCtx:ServletContext ) {
  def matches( path:String ) = {
    // TODO:  check for path separators ... i.e. "/foo" should not match "/foobar" but should match "/foo/bar"
    req.getServletPath.startsWith( path )
  }

  def path = req.getServletPath
}

trait Weblet {
  def matches( ctx:WebContext ) = true

  def handle( ctx:WebContext ):Unit
}


object WebTemplate {
  /*

     +.  error message substitution

     +.  head merge (or some equivalent)

     +.  intelligent javascript includes

     +.  better caching / performance:
     
         static vs. dynamic
         has templates vs. template-less

   */

  private def hasTemplates( nodes:NodeSeq ):Boolean = nodes exists hasTemplates
  private def hasTemplates(  node:Node    ):Boolean = node.prefix == "tyr" || hasTemplates( node.child )

  private def bindNode( node:Node, content:NodeSeq ):NodeSeq =
    node match {
    case e:Elem if node.prefix == "tyr" =>
    
      if ( node.label == "content" ) {
        apply( content )
      } else {
        val template = Tyr.templates.find( p => p._1 == node.label ).map( _._2 ) getOrElse ( throw new WebException( "Missing template " + node.label ) )
        apply( template( node ), e.child )
      }

    case t:Text =>
      t

    case other =>
      if ( hasTemplates( node ) )
        new Elem( node.prefix, node.label, node.attributes, node.scope, apply( node.child, content ):_* ) 
      else
        node
    }

  def apply( xml:NodeSeq, content:NodeSeq = NodeSeq.Empty ):NodeSeq =
    if ( hasTemplates( xml ) )
      xml.flatMap( node => bindNode( node, content ) )
    else
      xml
}


/*
 * This initialization is done inside of a servlet because we need to make sure that the CometServlet has initialized already and
 * using a servlet allows us to use the web.xml load-on-startup mechanism to achieve this ordering.  Another solution would be to set
 * up a web.xml listener.
 */
class WebInit extends GenericServlet {

  override def init {
    val bayeux = getServletContext().getAttribute(BayeuxServer.ATTRIBUTE).as[BayeuxServer]
    Tyr.bayeux = bayeux
    Tyr.comets.foreach { _.init( bayeux ) }
  }

  def service( req:ServletRequest, res:ServletResponse ) = {
    throw new ServletException
  }
}

