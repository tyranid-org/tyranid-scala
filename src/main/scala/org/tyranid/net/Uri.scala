
package org.tyranid.net

import org.tyranid.Imp._


object Uri {

  /*
   * Given "google.com" returns "google"
   */
  def domainPart( domain:String ) =
    // TODO:  make this more efficient & intelligent
    org.tyranid.email.Email.domainPart( "junk@" + domain )

  def completeUri( base:String ):String = {

    if ( base.indexOf( "://" ) != -1 )
      return base

    "http://" + base
  }

  def nameForDomain( domain:String ):String =
    try {
      val s = ( "http://" + domain ).GET().toLowerCase

      var idx = s.indexOf( "<title>" )

      if ( idx != -1 ) {
        idx += 7 // skip past <title>
        val sb = new StringBuilder

        while ( idx < s.length ) {
          s.charAt( idx ) match {
          case '<' | '-' | '|' =>
            idx = s.length
        
          case ch =>
            sb += ch
          }

          idx += 1
        }

        sb.toString.trim
      } else {
        null
      }
    } catch {
    case e:java.net.UnknownHostException =>
      return null
    }

  /*
   * Example:  AT&T, returns att
   */
  def lowerDomainChars( domain:String ) = {
    val sb = new StringBuilder

    for ( ch <- domain )
      ch match {
      case ch if ch.isLetterOrDigit || ch == '-' => sb += ch.toLower
      case _ =>
      }

    sb.toString
  }
}

