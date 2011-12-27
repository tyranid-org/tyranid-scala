
package org.tyranid.web

import javax.servlet.ServletConfig
import javax.servlet.{ Filter, FilterChain, FilterConfig, ServletRequest, ServletResponse }
import javax.servlet.http.{ HttpServlet, HttpServletRequest, HttpServletResponse }

import scala.xml.{ Elem, Node, NodeSeq, Text }

import org.tyranid.Imp._
import org.tyranid.boot.Boot


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

    Boot.instance.weblets.find( pair => ctx.matches( pair._1 ) && pair._2.matches( ctx ) ) match {
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

     +.  simple replace replacement

         <tyr:surround with="shell">

         <tyr:shell>


     +.  head merge (or some equivalent)

     +.  intelligent javascript includes

     +.  error message substitution

     +.  caching:
     
         static vs. dynamic
         has templates vs. template-less

   */

  def hasTemplates( nodes:NodeSeq ):Boolean = nodes exists hasTemplates
  def hasTemplates(  node:Node    ):Boolean = node.prefix == "tyr" || hasTemplates( node.child )

  def processNode( node:Node ):NodeSeq =
    node match {
    case e:Elem if node.prefix == "tyr" =>
      val template = org.tyranid.boot.Boot.instance.templates.find( p => p._1 == node.label ).map( _._2 ) getOrElse ( throw new WebException( "Missing template " + node.label ) )
      bind( template( node ), e.child )

    case t:Text =>
      t

    case other =>
      if ( hasTemplates( node ) )
        new Elem( node.prefix, node.label, node.attributes, node.scope, apply( node.child ):_* ) 
      else
        node
    }

  def apply( xml:NodeSeq ) =
    if ( hasTemplates( xml ) )
      xml flatMap processNode
    else
      xml

  def bindNode( node:Node, replace:NodeSeq ):NodeSeq =
    node match {
    case e:Elem if node.prefix == "tyr" && node.label == "content" =>
      replace

    case t:Text =>
      t

    case other =>
      if ( hasTemplates( node ) )
        new Elem( node.prefix, node.label, node.attributes, node.scope, bind( node.child, replace ):_* ) 
      else
        node
    }

  def bind( xml:NodeSeq, replace:NodeSeq ):NodeSeq =
    if ( hasTemplates( xml ) )
      xml.flatMap( node => bindNode( node, replace ) )
    else
      xml
}


