
package org.tyranid.http

import javax.servlet.{ Filter, FilterChain, FilterConfig, ServletRequest, ServletResponse }
import javax.servlet.http.{ HttpServlet, HttpServletRequest, HttpServletResponse }
import org.tyranid.Imp._
import scala.xml.NodeSeq


case class RestException( code:String, message:String ) extends Exception

case class HttpServletRequestOps( req:HttpServletRequest ) {

  def s( param:String ):String      = req.getParameter( param )
  def a( param:String ):Seq[String] =
    // jQuery appends a [] to all arrays, check to see if that exists
    req.getParameterValues( param + "[]" ) match {
    case null => req.getParameterValues( param )
    case arr  => arr
    }

  def sReq( param:String ) = {
    val s = req.getParameter( param )

    if ( s.isBlank )
      throw new RestException( code = "missing-" + param, message = "The '" + param + "' parameter is required." )

    s
  }
}

case class HttpServletResponseOps( res:HttpServletResponse ) {

  def ok = {
    res.setStatus( 200 )
  }

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

  def html( xml:NodeSeq, status:Int = 200 ) = {
    res.setContentType( "text/html" )
    res.setStatus( status )
    out( xml.toString() )
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

