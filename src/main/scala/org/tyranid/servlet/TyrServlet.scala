
package org.tyranid.servlet

import javax.servlet.ServletConfig
import javax.servlet.http.{ HttpServlet, HttpServletRequest, HttpServletResponse }

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
