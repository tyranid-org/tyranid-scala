
package org.tyranid.http

import scala.xml.NodeSeq

import javax.servlet.{ Filter, FilterChain, FilterConfig, ServletRequest, ServletResponse }
import javax.servlet.http.{ HttpServlet, HttpServletRequest, HttpServletResponse }

import org.apache.http.NameValuePair
import org.apache.http.client.methods.{ HttpRequestBase, HttpDelete, HttpGet, HttpPost }
import org.apache.http.entity.StringEntity
import org.apache.http.client.entity.UrlEncodedFormEntity
import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http.message.BasicNameValuePair
import org.apache.http.util.EntityUtils

import org.tyranid.Imp._


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
  
  def dump = {
    var enumer = req.getAttributeNames()

    println( "--Attributes--" )
      
    while ( enumer.hasMoreElements() ) {
      val key = enumer.nextElement()
      val value = req.getAttribute( key.asInstanceOf[String] )
      println( key + " : " + value )
    }

    println( "--Parameters--" )
    enumer = req.getParameterNames();

    while ( enumer.hasMoreElements() ) {
      val key = enumer.nextElement()
      val value = req.getParameter( key.asInstanceOf[String] )
      println( key + " : " + value )
    }
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



object Http {

  def makeUrl( url:String, query:Map[String,String] = null ) =
    if ( query != null && query.size > 0 ) {
      url +
        ( if ( url.contains( '?' ) ) '&' else '?' ) +
        query.map( p => p._1 + '=' + p._2.encUrl ).mkString( "&" )
    } else {
      url
    }

  private def execute( request:HttpRequestBase ) = {
    val client = new DefaultHttpClient
    val response = client.execute( request )
    val entity = response.getEntity

    entity != null |* EntityUtils.toString( entity )
  }

  def GET( url:String, query:Map[String,String] = null ) =
    execute( new HttpGet( makeUrl( url, query ) ) )

  def POST( url:String, content:String, form:Map[String,String] ) = {
    val request = new HttpPost( url )

    request.setEntity {
      if ( form != null ) {
        assert( content == null )

        val formparams = new java.util.ArrayList[NameValuePair]()

        form foreach { p => formparams.add( new BasicNameValuePair( p._1, p._2 ) ) }
        new UrlEncodedFormEntity(formparams, "UTF-8")
      } else {
        new StringEntity( content )
      }
    }

    execute( request )
  }

  def DELETE( url:String, query:Map[String,String] = null ) =
    execute( new HttpDelete( makeUrl( url, query ) ) )

}


