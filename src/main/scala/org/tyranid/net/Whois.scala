/**
 * Copyright (c) 2008-2013 Tyranid <http://tyranid.org>
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

package org.tyranid.net

import java.io._
import java.net.Socket

import org.tyranid.Imp._


object Whois {

  val Port = 43

  val endpoints = Seq(
    "whois.networksolutions.com",
    "whois.ascio.com",
    "whois.internic.net"
  )

  def apply( domain:String ):String = {

    val s = new Socket( endpoints( 0 ), Port )
    s.setSoTimeout( 30000 )

		val host = domain + "\n"
		val buf = host.getBytes

		s.getOutputStream.write( buf )

		val str = s.getInputStream.asString
    val lines = str.split( "\n" )
		s.close

    for ( i <- 0 until lines.size ) {
      val s = lines( i )

      if ( s.equals( "Registrant:" ) ) {
        return lines( i+1 )
      }
    }

    null
	}
}

