/**
 * Copyright (c) 2008-2014 Tyranid <http://tyranid.org>
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

import scala.collection.JavaConversions._

import org.tyranid.Imp._


object Ip {

  def sanitize( s:String ) =
    if ( s.startsWith( "/" ) )
      s.substring( 1 )
    else
      s

  def determineHost:String = {
    val addresses =
      for ( interface <- java.net.NetworkInterface.getNetworkInterfaces;
            //if !interface.isLoopbackAddress;
            address <- interface.getInetAddresses;
            if !address.isLoopbackAddress )
        yield address.toString

    // prefer an ipv4 address if we have one
    for ( a <- addresses;
          if a.indexOf( ':' ) < 0 )
      return sanitize( a )

    for ( a <- addresses )
      return sanitize( a )

    "127.0.0.1"
  }

  lazy val Host:String = determineHost
}

