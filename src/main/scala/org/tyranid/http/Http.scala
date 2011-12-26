
package org.tyranid.servlet

import javax.servlet.ServletConfig
import javax.servlet.{ Filter, FilterChain, FilterConfig, ServletRequest, ServletResponse }
import javax.servlet.http.{ HttpServlet, HttpServletRequest, HttpServletResponse }

import org.tyranid.Imp._


case class RestException( code:String, message:String ) extends Exception

case class HttpServletRequestOps( req:HttpServletRequest ) {

  def s( param:String ) = req.getParameter( param )

  def sReq( param:String ) = {
    val s = req.getParameter( param )

    if ( s.isBlank )
      throw new RestException( code = "missing-" + param, message = "The '" + param + "' parameter is required." )

    s
  }
}

case class HttpServletResponseOps( res:HttpServletResponse ) {

  def json( json:Any, status:Int = 200, jsonpCallback:String = null ) = {
    res.setContentType( if ( jsonpCallback != null ) "text/javascript" else "application/json" )
    res.setStatus( status )

    out( 
      if ( jsonpCallback != null ) {
        jsonpCallback + "(" + json.toJsonStr + ")"
      } else {
        json.toJsonStr 
      }
    )
  }

  def out( s:String ) = {
    val slen = s.length

    res.setContentLength( slen )
            
    val out = res.getOutputStream
    out.write( s.getBytes, 0, slen )
    out.close

    res.getOutputStream.close
  }
}


class TyrFilter extends Filter {

  var filterConfig:FilterConfig = _


  def init( filterConfig:FilterConfig ) {
spam( "*** initializing filter" )
    this.filterConfig = filterConfig;
    org.tyranid.boot.Boot.boot
  }

  def destroy {
    this.filterConfig = null
  }

  def doFilter( request:ServletRequest, response:ServletResponse, chain:FilterChain ) {

    val req = request.asInstanceOf[HttpServletRequest]
    val res = response.asInstanceOf[HttpServletResponse]

    spam( "pathInfo:" + req.getPathInfo )

    chain.doFilter(request, response);
  }
}


