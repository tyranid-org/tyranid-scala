
package org.tyranid.servlet

import javax.servlet.ServletConfig
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

  def json( json:Any, status:Int = 200 ) = {

    res.setContentType( "application/json" )
    res.setStatus( status )
    out( json.toJsonStr )
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


class TyrServlet extends HttpServlet {
	
	override def init( config:ServletConfig ) {}

	override def doGet( req:HttpServletRequest, res:HttpServletResponse ) {
	  
    res.setContentType( "text/plain" )
	        
    val output = "test"
    val outputLength = output.length

    res.setContentLength( outputLength )
	        
    val out = res.getOutputStream
    out.write( output.getBytes, 0, outputLength )
    out.close
  }
}
