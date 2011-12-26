
package org.tyranid.web

import javax.servlet.ServletConfig
import javax.servlet.{ Filter, FilterChain, FilterConfig, ServletRequest, ServletResponse }
import javax.servlet.http.{ HttpServlet, HttpServletRequest, HttpServletResponse }

import org.tyranid.Imp._
import org.tyranid.boot.Boot


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

spam( "matching: " + ctx.req.getServletPath )
    Boot.instance.weblets.find( _.matches( ctx ) ) match {
    case Some( weblet ) => weblet.handle( ctx )
    case None           => chain.doFilter(request, response);
    }
  }
}


case class WebContext( req:HttpServletRequest, res:HttpServletResponse ) {

  def matches( path:String ) = {
    spam( "matching: " + req.getServletPath + " vs. " + path )
    // TODO:  check for path separators ... i.e. "/foo" should not match "/foobar" but should match "/foo/bar"
    req.getServletPath.startsWith( path )
  }

}

trait Weblet {

  def matches( ctx:WebContext ):Boolean
  def handle( ctx:WebContext ):Unit
}


