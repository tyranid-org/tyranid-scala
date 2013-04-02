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

package org.tyranid.pdf

import java.io.{ File, FileOutputStream, IOException }

import com.pdfcrowd.{ Client, PdfcrowdError }

import org.tyranid.math.Base36

import org.tyranid.Imp._
      
object Pdf {
  val lock = ""
    
  def urlToFile( url:String, outFile:File, enableHyperlinks:Boolean = false, username:String = null, password:String = null ) = {
    // PDF Crowd API only allows one at a time
    lock.synchronized {
      var useJavascript = true
      var retry = true
      var fileStream:FileOutputStream = null
        
      while ( retry ) {
    	  try {
    	    fileStream = new FileOutputStream( outFile )     
    	 
    	    // create an API client instance
    	    val client:Client = new Client( B.pdfCrowdName, B.pdfCrowdKey )
    	    client.useSSL( true )
    	        
    	    // convert a web page and save the PDF to a file
          if ( B.onePagePdf )
            client.setPageHeight( -1 )
    	    
          client.enableHyperlinks( enableHyperlinks )
          
          if ( !useJavascript )
            client.enableJavaScript( false )
          
          val finalUrl = ( username.notBlank && password.notBlank ) ? {
            val idx = url.toLowerCase().indexOf( "://" )
            
            if ( idx > -1 ) {
              url.substring( 0, idx + 3 ) + username + ":" + password +  "@" + url.substring( idx + 3  ) 
            } else {
              ( username + ":" + password +  "@" + url )
            }
          } | url
          
          //println( finalUrl )
    	    client.convertURI( finalUrl, fileStream )
    	    
    	    if ( !useJavascript )
            B.sendMessage( "When importing the URL [" + url + "], there was an error.  It was tried again with Javascript turned off on the page and it was successful.  The result may not exactly reflect what the page looked like.", T.user.tid )
            
    	    retry = false
    	  } catch {
    	    case why:PdfcrowdError =>
    	      why.statusCode match {
    	        case js if useJavascript => // 510 = 413 Timed out. Can't load the specified URL.
    	          useJavascript = false
    	        case sc =>
    	          retry = false
    	        //503 - Simultaneous API calls from a single IP are not allowed.
                val msg = why.getMessage
                println( "status code: " + sc )
                throw new RuntimeException( "Status code: " + sc + ", " + msg.firstSuffix( '-' ) or msg )
    	        }
    	    case e:IOException =>
    	      retry = false
            println( e.getMessage )
            throw new RuntimeException( e )
    	  } finally {
    	    if ( fileStream != null )
    	      fileStream.close
    	  }
      }
  	  
  	  outFile
    }
  }
}