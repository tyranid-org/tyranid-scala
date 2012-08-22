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

package org.tyranid.io

import scala.annotation.tailrec

import java.io.{ IOException, FileOutputStream, InputStream, InputStreamReader, OutputStream }

import org.tyranid.db.meta.{ TidItem }
import org.tyranid.Imp._
import org.tyranid.web.{ WebContext, Weblet }


class InputStreamImp( is:InputStream ) {

  def asString = {
    val buffer = new Array[Char]( 0x10000 )
    val sb = new StringBuilder
    val in = new InputStreamReader( is, "UTF-8" )
    var read = 0

    try {
      do {
        read = in.read( buffer, 0, buffer.length )
        if ( read > 0 )
          sb.appendAll( buffer, 0, read )
        
      } while ( read >= 0 )
    } finally {
      is.close
    }

    sb.toString
  }

  def transferTo( to:OutputStream, closeInput:Boolean = false ) {
  	try {
      val buffer = new Array[Byte]( 8192 )
  
      @tailrec def transfer {
        val read = is.read( buffer )
  
        if ( read >= 0 ) {
          to.write( buffer, 0, read )
          transfer
        }
      }
        
      transfer
  	} finally {
  	  to.flush
  	  
  	  if ( closeInput )
  	    is.close
  	}
  }
}

object Iolet extends Weblet {
  def handle( web:WebContext ) {

    rpath match {
      
    // example: /thumb/a09vUCwNUOSweddROKEl/l
    case s if s.startsWith( "/thumb" ) =>
      val parts = s.substring( 6 ).split( "/" )
      
      val tid = parts(0)
      
      val tidItem = TidItem.by( tid )

      //tidItem.tid match {
      //  case 
     // }
      
      val size = parts(1)
      
      


      //val originalUrl = web.req.getAttribute( "javax.servlet.forward.request_uri" )

      //println( originalUrl )
      
      //if ( originalUrl != null )
      //  log( Event.Error404, "p" -> originalUrl )

      //web.template( <tyr:404/> )

    case _ =>
      _404
    }
  }
}


