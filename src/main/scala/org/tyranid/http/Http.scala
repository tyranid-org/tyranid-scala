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

import java.io.{ InputStream, File, FileInputStream, IOException, OutputStream }
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
import org.apache.http.client.methods.{ HttpRequestBase, HttpDelete, HttpGet, HttpPost, HttpPut, HttpUriRequest, HttpEntityEnclosingRequestBase }
import org.apache.http.entity.{ StringEntity, InputStreamEntity }
import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http.entity.mime.MultipartEntity 
import org.apache.http.entity.mime.content.{ InputStreamBody, StringBody, FileBody } 
import org.apache.http.message.{ BasicHeader, BasicNameValuePair }
import org.apache.http.params.{ BasicHttpParams, HttpConnectionParams }
import org.apache.http.protocol.{ ExecutionContext, HttpContext, BasicHttpContext }
import org.apache.http.util.EntityUtils

import org.tyranid.Imp._
import org.tyranid.web.FileUploadSupport


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
  def a_?( param:String ):Seq[String] = {
    val arr = a( param )
    if ( arr != null ) arr else Nil
  }

  def file( param:String ):org.apache.commons.fileupload.FileItem = {
    val bodyParams:FileUploadSupport.BodyParams = req.getAttribute( FileUploadSupport.BodyParamsKey ).asInstanceOf[FileUploadSupport.BodyParams]

    if ( bodyParams != null )
      bodyParams.getFileItems( param ).headOption getOrElse null
    else 
      null
  }

  def sReq( param:String ) = {
    val s = req.getParameter( param )

    if ( s.isBlank )
      throw new RestException( code = "missing-" + param, message = "The '" + param + "' parameter is required." )

    s
  }
  
  def serializeParams( filter:Option[ ( String ) => Boolean ] = None ) =
    req.getParameterNames.toSeq.filter( name => filter.flatten( _( name.as[String] ), true ) ).map( n => ( n + "=" + s( n.as[String] ).encUrl ) ).mkString( "&" )
  
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
      println(
        "  " + c.getName + " = " + c.getValue.literal +
        ( c.getDomain != null |* "\n    Domain = " + c.getDomain ) +
        ( c.getPath != null   |* "\n    Path = " + c.getPath     ) +
        ( c.getMaxAge != -1   |* "\n    MaxAge = " + c.getMaxAge ) +
        ( c.getSecure         |* "\n    Secure = true"           )
      )
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

  def cookie( name:String, domain:String = null ):Option[Cookie] = {
dump
    val cookies = req.getCookies
    if ( cookies == null )
      return None

    ( domain.notBlank |* cookies.find( c => c.getName == name && c.getDomain == domain ) ) orElse
    cookies.find( _.getName == name )
  }

  def cookieValue( name:String, domain:String = null ):String = cookie( name, domain = domain ).map( _.getValue ) getOrElse null
}

case class HttpServletResponseOps( res:HttpServletResponse ) {
  def ok = {
    res.setStatus( 200 )
  }

  def json( json:Any, status:Int = 200, jsonpCallback:String = null, headers:Map[String,String] = null, req:HttpServletRequest = null, cache:Boolean = false ) = {
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

    if ( !cache ) setNoCacheHeaders( res )
    
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

  def html( html:NodeSeq, status:Int = 200, headers:Map[String,String] = null, req:HttpServletRequest = null, cache:Boolean = false ) =
    content( html.toString, "text/html", status, headers, req, cache )

  def rss( rssXml:NodeSeq, status:Int = 200, headers:Map[String,String] = null, req:HttpServletRequest = null, cache:Boolean = false ) =
    content( rssXml.toString, "application/rss+xml", status, headers, req, cache )

  def content( text:String, mimeType:String, status:Int = 200, headers:Map[String,String] = null, req:HttpServletRequest = null, cache:Boolean = false ) = {
    res.setContentType( mimeType )
    res.setStatus( status )

    if ( !cache ) setNoCacheHeaders( res )
    if ( headers != null )
      for ( h <- headers ) 
        res.setHeader( h._1, h._2 )
        
    try {
      out( text ) 
    } catch {
      case e:IOException => ; // bury
      case e if e.getClass.getSimpleName == "EofException" =>
      case e2 => throw e2
    }
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
    var out:OutputStream = null
    
    try {
      out = res.getOutputStream
      out.write( bytes, 0, blen )
    } catch {
      case e:IOException => ; // bury
      case e if e.getClass.getSimpleName == "EofException" =>
        println( "*** Broken pipe" )
      case e2 => throw e2
    } finally {
      if ( out != null )
        out.close
    }
  }

  def deleteCookie( name:String, path:String = "/", domain:String = null ) = {
    val cookie = new Cookie( name, "" )
    cookie.setMaxAge( 0 )
    cookie.setPath( path )
    if ( domain.notBlank )
      cookie.setDomain( domain )
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

  private def execute( request:HttpRequestBase, withParams:Boolean = true ) = {
    val httpParams = new BasicHttpParams
    HttpConnectionParams.setConnectionTimeout( httpParams, 30000 ) // 30s
    HttpConnectionParams.setSoTimeout( httpParams, 30000 ) // 30s
    val client = withParams ? new DefaultHttpClient( httpParams ) | new DefaultHttpClient( httpParams ) 
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

  def POST( url:String, content:String, form:collection.Map[String,String], contentType:String = null, headers:collection.Map[String,String] = null ):HttpResult =
    executePost( new HttpPost( url ), content, form, contentType, headers )

  def PUT( url:String, content:String, form:collection.Map[String,String], contentType:String = null, headers:collection.Map[String,String] = null ):HttpResult =
    executePost( new HttpPut( url ), content, form, contentType, headers )

  private def executePost( request:org.apache.http.client.methods.HttpEntityEnclosingRequestBase, content:String, form:collection.Map[String,String], contentType:String, headers:collection.Map[String,String] ):HttpResult = {
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
  
  def POST_FILE( url:String, file:File, contentLength: Long, filename:String, params:collection.Map[String,String] = null, headers:collection.Map[String,String] = null, filePartName:String = "file" ):HttpResult = {
        
    val request = new HttpPost( url )
    
    if ( headers != null )
      request.setHeaders( convertHeaders( headers ) )

    request.setEntity {
      if ( params != null ) {
        val multipart = new MultipartEntity()
        params.foreach{ p => multipart.addPart( p._1, new StringBody( p._2, java.nio.charset.Charset.forName( "UTF-8" ) ) ) }
        multipart.addPart( filePartName, new FileBody( file, org.tyranid.io.File.mimeTypeFor( filename ).or( "application/octet-stream" ) ) )
        multipart
      } else {
        new InputStreamEntity( new FileInputStream( file ), contentLength )
      }
    }
    
    execute( request, false )
  }
  
  def DELETE( url:String, query:collection.Map[String,String] = null ):HttpResult =
    execute( new HttpDelete( makeUrl( url, query ) ) )
}


