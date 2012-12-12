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

package org.tyranid.web

import java.util.{ List => JList, HashMap => JHashMap, Map => JMap }
import javax.servlet.http.{ HttpServletRequestWrapper, HttpServletRequest, HttpServletResponse }

import scala.collection.JavaConversions._
import scala.collection.mutable

import org.apache.commons.fileupload.servlet.ServletFileUpload
import org.apache.commons.fileupload.{ FileItemFactory, FileItem }
import org.apache.commons.fileupload.disk.{ DiskFileItem, DiskFileItemFactory }

import org.tyranid.Imp._
import org.tyranid.session.Session

object FileUploadSupport {

  val maxFileSize = 1024 * 1000 * 1000 * 20 // 20GB

  case class BodyParams( fileParams:collection.Map[String,Seq[FileItem]], formParams:collection.Map[String,Seq[String]] ) {
    def getFileItems( key:String, fallback:Seq[FileItem] = null ):Seq[FileItem] = ( fileParams.get( key ) orElse fileParams.get( key + "[]" ) ).getOrElse( ( fallback == null ) ? Nil | fallback ) 
  }

  val BodyParamsKey = "org.tyranid.web.fileupload.bodyParams"

  def checkContext( web:WebContext ):WebContext = {
    val req = web.req
    
    if ( ServletFileUpload.isMultipartContent( req ) ) {
      val mergedParams = extractMultipartParams( req ).formParams.as[ mutable.Map[String,Seq[String]] ]

      // Add the query string parameters
      req.getParameterMap.as[JMap[String, Array[String]]] foreach {
      case (name, values) => mergedParams( name ) = values.toSeq ++ mergedParams.getOrElse( name, Nil )
      }
      
      val newWeb = web.copy( req =
        new HttpServletRequestWrapper( req ) {
          override def getParameter( name:String ) = ( mergedParams.get(name) map { _.head } getOrElse null ).stripXss
          override def getParameterNames           = mergedParams.keysIterator
          override def getParameterMap = {
            val map = new JHashMap[String,Array[String]]()
            for ( ( k, v ) <- mergedParams )
              map( k ) = v.toArray
            map
          }
          
          override def getHeader( name:String ) = req.getHeader( name ).stripXss
          
          override def getParameterValues( name:String ) = {
            val values = mergedParams.get( name ) map { _.toArray } getOrElse null

            if ( values == null ) {
              null
            } else {
              val count = values.length
              val encodedValues = new Array[String](count)
              
              for ( i <- 0 until count )
                encodedValues(i) = values(i).stripXss
  
              encodedValues
            }
          }
        }
      )
      
      newWeb.upload = true
      newWeb
    } else
      web
  }
  
  private def extractMultipartParams( req:HttpServletRequest ) = {
    var bodyParams = req.getAttribute( BodyParamsKey ).as[BodyParams]
  
    if ( bodyParams == null ) {
      val sfu = new ServletFileUpload( new DiskFileItemFactory )
      sfu.setSizeMax( maxFileSize )
      var fileParams = mutable.Map[String, Seq[FileItem]]()
      var formParams = mutable.Map[String, Seq[String]]()
     
      try {
        val items = sfu.parseRequest(req).asInstanceOf[JList[FileItem]]
  
        for ( item <- items )
          if ( item.isFormField )
            formParams( item.getFieldName ) = fileItemToString(item) +: formParams.getOrElse(item.getFieldName, Nil )
          else
            fileParams( item.getFieldName ) = item +: fileParams.getOrElse(item.getFieldName, Nil )
      } catch {
        case e => 
          e.printStackTrace
          // nop
      }

      bodyParams = BodyParams( fileParams, formParams )
      req.setAttribute( BodyParamsKey, bodyParams )
    }

    bodyParams
  }

  /**
   * Converts a file item to a string.
   *
   * Browsers tend to be sloppy about providing content type headers with
   * charset information to form-data parts.  Worse, browsers are
   * inconsistent about how they encode these parameters.
   *
   * The default implementation attempts to use the charset specified on
   * the request.  If that is unspecified, and it usually isn't, then it
   * falls back to the kernel's charset.
   */
  private def fileItemToString( item:FileItem ):String = {
    val charset = item match {
      case diskItem:DiskFileItem =>
        // Why doesn't FileItem have this method???
        Option( diskItem.getCharSet )
      case _ =>
        None
    }
    item.getString( charset getOrElse "UTF-8" )
  }
}

