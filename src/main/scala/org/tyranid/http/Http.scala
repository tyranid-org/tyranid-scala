
package org.tyranid.http

import scala.collection.mutable
import scala.xml.NodeSeq

import javax.servlet.{ Filter, FilterChain, FilterConfig, ServletRequest, ServletResponse }
import javax.servlet.http.{ Cookie, HttpServlet, HttpServletRequest, HttpServletResponse }

import org.apache.http.{ Header, NameValuePair }
import org.apache.http.client.methods.{ HttpRequestBase, HttpDelete, HttpGet, HttpPost }
import org.apache.http.entity.StringEntity
import org.apache.http.client.entity.UrlEncodedFormEntity
import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http.message.{ BasicHeader, BasicNameValuePair }
import org.apache.http.util.EntityUtils

import org.tyranid.Imp._


case class RestException( code:String, message:String ) extends Exception

case class HttpServletRequestOps( req:HttpServletRequest ) {

  def s( param:String ):String = req.getParameter( param )
  def i( param:String ):Int = {
    val s = req.getParameter( param )
    s != null |* s.toLaxInt
  }
  def l( param:String ):Long = {
    val s = req.getParameter( param )
    s != null |* s.toLaxLong
  }
  def b( param:String ):Boolean = {
    val s = req.getParameter( param )
    s != null && s.toLaxBoolean
  }

  def oid( param:String ) = new org.bson.types.ObjectId( s( param ) )
  
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

  def path = req.getServletPath

  def uriAndQueryString = {
    val qs = req.getQueryString

    val sb = new StringBuilder
    sb ++= req.getServletPath
    if ( qs.notBlank )
      sb += '?' ++= qs

    sb.toString
  }

  def cookie( name:String ):Option[Cookie] = {
    val cookies = req.getCookies
    cookies != null |* cookies.find( _.getName == name )
  }

  def cookieValue( name:String ) = cookie( name ).map( _.getValue ) getOrElse null
}

case class HttpServletResponseOps( res:HttpServletResponse ) {

  def ok = {
    res.setStatus( 200 )
  }

  def json( json:Any, status:Int = 200, jsonpCallback:String = null, headers:Map[String,String] = null ) = {
    res.setContentType( if ( jsonpCallback != null ) "text/javascript" else "application/json" )
    res.setStatus( status )

    if ( headers != null )
      for ( h <- headers ) 
        res.setHeader( h._1, h._2 )
    
    val outputJson = if ( json == null ) "{}" else json.toJsonStr
    
    out( 
      if ( jsonpCallback != null ) {
        jsonpCallback + "(" + outputJson + ")"
      } else {
        outputJson
      }
    )
  }

  def html( xml:NodeSeq, status:Int = 200, headers:Map[String,String] = null ) = {
    res.setContentType( "text/html" )
    res.setStatus( status )
    
    if ( headers != null )
      for ( h <- headers ) 
        res.setHeader( h._1, h._2 )
        
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

  def deleteCookie( name:String, path:String = "/" ) = {
    val cookie = new Cookie( name, "" )
    cookie.setMaxAge( 0 )
    cookie.setPath( path )
    res.addCookie( cookie )
  }
}



object Http {

  def extractQueryString( url:String, params:mutable.Map[String,String], oauth:Boolean = false ) = {
    val idx = url.indexOf( '?' )

    if ( idx != -1 ) {

      val pairs = url.substring( idx+1 ).splitAmp
      for ( pair <- pairs ) {
        val eq = pair.indexOf( '=' )
        if ( eq == -1 ) {
          params( pair ) = ""
        } else {
          params( pair.substring(0, eq) ) = {
            val s = pair.substring(eq + 1)
            if ( oauth )
              s.decOAuthUrl
            else
              s.decUrl
          }
        }
      }

      url.substring( 0, idx )
    } else {
      url
    }
  }

  def makeUrl( url:String, query:collection.Map[String,String] = null ) =
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

  private def convertHeaders( headers:collection.Map[String,String] ) =
    headers.toSeq.map( p => new BasicHeader( p._1, p._2 ) ).toArray[Header]

  def GET( url:String, query:collection.Map[String,String] = null, headers:collection.Map[String,String] = null ) = {
    val get = new HttpGet( makeUrl( url, query ) )
    if ( headers != null )
      get.setHeaders( convertHeaders( headers ) )
    execute( get )
  }

  def POST( url:String, content:String, form:collection.Map[String,String], contentType:String = null, headers:collection.Map[String,String] = null ) = {
    val request = new HttpPost( url )

    if ( headers != null )
      request.setHeaders( convertHeaders( headers ) )

    request.setEntity {
      if ( form != null ) {
        assert( content == null )

        val formparams = new java.util.ArrayList[NameValuePair]()

        form foreach { p => formparams.add( new BasicNameValuePair( p._1, p._2 ) ) }
        new UrlEncodedFormEntity(formparams, "UTF-8")
      } else {
        val se = new StringEntity( content )
        if ( contentType != null )
          se.setContentType( contentType )
        se
      }
    }

    execute( request )
  }

  def DELETE( url:String, query:collection.Map[String,String] = null ) =
    execute( new HttpDelete( makeUrl( url, query ) ) )

}


