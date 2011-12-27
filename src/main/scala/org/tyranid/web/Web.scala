
package org.tyranid.web

import javax.servlet.ServletConfig
import javax.servlet.{ Filter, FilterChain, FilterConfig, ServletRequest, ServletResponse }
import javax.servlet.http.{ HttpServlet, HttpServletRequest, HttpServletResponse }

import scala.xml.{ Elem, Node, NodeSeq, Text }

import org.tyranid.Imp._


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

  def doFilter( request:ServletRequest, response:ServletResponse, chain:FilterChain ) {

    val ctx = new WebContext( request.asInstanceOf[HttpServletRequest],
                              response.asInstanceOf[HttpServletResponse] )

    Tyr.weblets.find( pair => ctx.matches( pair._1 ) && pair._2.matches( ctx ) ) match {
    case Some( ( path, weblet ) ) =>
      try {
        weblet.handle( ctx )
      } catch {
      case e =>
        e.printStackTrace
      }

    case None =>
      chain.doFilter(request, response);
    }
  }
}


case class WebContext( req:HttpServletRequest, res:HttpServletResponse ) {

  def matches( path:String ) = {
    // TODO:  check for path separators ... i.e. "/foo" should not match "/foobar" but should match "/foo/bar"
    req.getServletPath.startsWith( path )
  }

}

trait Weblet {

  def matches( ctx:WebContext ) = true

  def handle( ctx:WebContext ):Unit
}


object WebTemplate {
  /*

     +.  recursively process templates

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


