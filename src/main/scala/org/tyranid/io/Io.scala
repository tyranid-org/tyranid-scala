/**
 * Copyright (c) 2008-2011 Tyranid <http://tyranid.org>
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

import org.tyranid.Imp._


class InputStreamImp( is:InputStream ) {

  def asString = {
    val buffer = new Array[Char]( 0x10000 )
    val sb = new StringBuilder
    val in = new InputStreamReader( is, "UTF-8" )
    var read = 0

    do {
      read = in.read( buffer, 0, buffer.length )
      if ( read > 0 )
        sb.appendAll( buffer, 0, read )
      
    } while ( read >= 0 )

    is.close
    sb.toString
  }

  def transferTo( to:OutputStream ) {
  	val buffer = new Array[Byte]( 8192 )

    @tailrec def transfer {
  		val read = is.read( buffer )

  		if ( read >= 0 ) {
  			to.write( buffer, 0, read )
  			transfer
  		}
  	}
  		
  	transfer
  }
}



