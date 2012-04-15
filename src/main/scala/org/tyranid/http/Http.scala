/**
 * Copyright (c) 2008-2012 Tyranid <http://tyranid.org>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.tyranid.http

import java.net.URL
import java.util.Date

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.xml.NodeSeq

import javax.servlet.{ Filter, FilterChain, FilterConfig, ServletRequest, ServletResponse }
import javax.servlet.http.{ Cookie, HttpServlet, HttpServletRequest, HttpServletResponse }

import org.bson.types.ObjectId

import org.apache.http.{ Header, NameValuePair, HttpHost, HttpResponse }
import org.apache.http.client.entity.UrlEncodedFormEntity
import org.apache.http.client.methods.{ HttpRequestBase, HttpDelete, HttpGet, HttpPost, HttpUriRequest }
import org.apache.http.entity.StringEntity
import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http.message.{ BasicHeader, BasicNameValuePair }
import org.apache.http.params.{ BasicHttpParams, HttpConnectionParams }
import org.apache.http.protocol.{ ExecutionContext, HttpContext, BasicHttpContext }
import org.apache.http.util.EntityUtils


import org.tyranid.Imp._


case class RestException( code:String, message:String ) extends Exception

case class Http403Exception( response:HttpResponse ) extends Exception


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

  def oid( param:String ):ObjectId = { 
    val oid = s( param )
    ( oid == null ) ? null | new ObjectId( oid )
  }

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

    println( "** requestURI=" + T.web.req.getRequestURL )

    println( "** attributes" )
    for ( n <- req.getAttributeNames )
      println( "  " + n + " = " + req.getAttribute( n.as[String] ) )

    println( "** parameters" )
    for ( n <- req.getParameterNames )
      println( "  " + n + " = " + req.getParameter( n.as[String] ) )

    println( "** headers" )
    for ( n <- req.getHeaderNames )
      for ( v <- req.getHeaders( n.as[String] ) )
        println( "  " + n + " = " + v )

    println( "** cookies" )
    for ( c <- cookies )
      println( "  " + c.getName + " = " + c.getValue.literal +
            ( c.getDomain != null |* "\n    Domain = " + c.getDomain ) +
            ( c.getPath != null |* "\n    Path = " + c.getPath ) +
            ( c.getMaxAge != -1 |* "\n    MaxAge = " + c.getMaxAge ) +
            ( c.getSecure |* "\n    Secure = true" ) )
  }

  def path = req.getServletPath

  def cookies:Array[Cookie] =
    req.getCookies match {
    case null => Array()
    case arr  => arr
    }

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

  def cookieValue( name:String ):String = cookie( name ).map( _.getValue ) getOrElse null
}

case class HttpServletResponseOps( res:HttpServletResponse ) {
  def ok = {
    res.setStatus( 200 )
  }

  def json( json:Any, status:Int = 200, jsonpCallback:String = null, headers:Map[String,String] = null, req:HttpServletRequest = null, noCache:Boolean = false ) = {
    var jsonContentType = "application/json"
      
    if ( req != null ) {
      val acceptHeader = req.getHeader( "Accept" )
      
      // This is necessary because IE and Opera do not yet suuport XHR file upload, so an internal 
      // iframe is used with json in the response 
      if ( acceptHeader.notBlank && acceptHeader.indexOf( "json" ) == -1 )
        jsonContentType = "text/plain"
    }
    
    res.setContentType( if ( jsonpCallback != null ) "text/javascript" else jsonContentType )
    res.setStatus( status )

    if ( noCache ) setNoCacheHeaders( res )
    
    if ( headers != null )
      for ( h <- headers ) 
        res.setHeader( h._1, h._2 )
    
    val outputJson = if ( json == null ) "{}" else json.toJsonStr
    
//    res.setContentLength( if ( jsonpCallback != null ) ( jsonpCallback.length + 2 + outputJson.length ) else outputJson.length )
    
    out( 
      if ( jsonpCallback != null ) {
        jsonpCallback + "(" + outputJson + ")"
      } else {
        outputJson
      }
    )
  }

  def html( xml:NodeSeq, status:Int = 200, headers:Map[String,String] = null, req:HttpServletRequest = null, noCache:Boolean = false ) = {
    res.setContentType( "text/html" )
    res.setStatus( status )

    if ( noCache ) setNoCacheHeaders( res )
    if ( headers != null )
      for ( h <- headers ) 
        res.setHeader( h._1, h._2 )
        
    out( xml.toString() )
  }

  def setNoCacheHeaders( res:HttpServletResponse ) {
    res.setHeader( "Cache-Control", "no-cache" )
    res.setHeader( "Pragma", "no-cache" )
    res.setHeader( "Expires", "-1" )
  }
  
  def out( s:String ) = {
    val bytes = s.getBytes
    val blen = bytes.length

    res.setContentLength( blen )
            
    val out = res.getOutputStream
    out.write( bytes, 0, blen )
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


// named HttpResult instead of HttpResponse to not conflict with Apache, can't simply pimp HttpResponse since we need the HttpContext as well
case class HttpResult( response:HttpResponse, context:HttpContext ) {

  def s = {
    val entity = response.getEntity
    entity != null |* EntityUtils.toString( entity )
  }

  override def toString = s

  // This is the final, actual URL, after redirects have been processed
  def url = {
    val currentReq  = context.getAttribute( ExecutionContext.HTTP_REQUEST ).as[HttpUriRequest]
    val currentHost = context.getAttribute( ExecutionContext.HTTP_TARGET_HOST ).as[HttpHost]

    if ( currentReq.getURI.isAbsolute ) currentReq.getURI.toURL
    else                                new URL( currentHost.toURI + currentReq.getURI )
  }
}

object Http {

  def expireCacheControlHeaders( ageMs:Long ) =
    Map( "Pragma" -> "public",
         "Cache-Control" -> ( "max_age=" + ( ageMs / 1000 ) ),
         "Expires" -> new Date( System.currentTimeMillis + ageMs ).toRfc1123 )

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
    val httpParams = new BasicHttpParams
    HttpConnectionParams.setConnectionTimeout( httpParams, 30000 ) // 30s
    HttpConnectionParams.setSoTimeout( httpParams, 30000 ) // 30s
    val client = new DefaultHttpClient( httpParams )
    val context = new BasicHttpContext() 
    val response = client.execute( request, context )

    response.getStatusLine.getStatusCode match {
    case 403 => throw new Http403Exception( response )
    case _   => HttpResult( response, context )
    }
  }

  private def convertHeaders( headers:collection.Map[String,String] ) =
    headers.toSeq.map( p => new BasicHeader( p._1, p._2 ) ).toArray[Header]

  def GET( url:String, query:collection.Map[String,String] = null, headers:collection.Map[String,String] = null ):HttpResult = {
    val get = new HttpGet( makeUrl( url, query ) )
    if ( headers != null )
      get.setHeaders( convertHeaders( headers ) )

    execute( get )
  }

  def POST( url:String, content:String, form:collection.Map[String,String], contentType:String = null, headers:collection.Map[String,String] = null ):HttpResult = {
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

  def DELETE( url:String, query:collection.Map[String,String] = null ):HttpResult =
    execute( new HttpDelete( makeUrl( url, query ) ) )

}


