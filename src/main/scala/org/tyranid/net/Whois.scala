
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

    spam( str )
    null
	}
}

