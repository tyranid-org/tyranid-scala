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

import scala.collection.JavaConversions._

import org.tyranid.Imp._


object Ip {

  lazy val Host =
    java.net.NetworkInterface.getNetworkInterfaces.
      find( ni => !ni.isLoopback && ni.getInetAddresses.hasMoreElements ).
      pluck( _.getInetAddresses.nextElement.getHostAddress, "127.0.0.1" )
}

