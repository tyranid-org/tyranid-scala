
package org.tyranid.net

import scala.collection.JavaConversions._

import org.tyranid.Imp._


object Ip {

  lazy val Host =
    java.net.NetworkInterface.getNetworkInterfaces.
      find( ni => !ni.isLoopback && ni.getInetAddresses.hasMoreElements ).
      flatten( _.getInetAddresses.nextElement.getHostAddress, "127.0.0.1" )
}

