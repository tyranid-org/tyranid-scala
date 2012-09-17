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
    
  def urlToFile( url:String, outFile:File, enableHyperlinks:Boolean = false ) = {
    // PDF Crowd API only allows one at a time
    lock.synchronized {
      var fileStream:FileOutputStream = null
      
  	  try {
  	    fileStream = new FileOutputStream( outFile )     
  	 
  	    // create an API client instance
  	    val client:Client = new Client( B.pdfCrowdName, B.pdfCrowdKey )
  	    client.useSSL( true )
  	        
  	    // convert a web page and save the PDF to a file
        client.setPageHeight( -1 );
        client.enableHyperlinks( enableHyperlinks )
  	    client.convertURI( url, fileStream )
  	  } catch {
  	    case why:PdfcrowdError =>
          //503 - Simultaneous API calls from a single IP are not allowed.
  	      val msg = why.getMessage
          println( msg )
          throw new RuntimeException( msg.firstSuffix( '-' ) or msg )
  	    case e:IOException =>
          println( e.getMessage )
          throw new RuntimeException( e )
  	  } finally {
  	    if ( fileStream != null )
  	      fileStream.close
  	  }
  	  
  	  outFile
    }
  }
}