
package org.tyranid.net

import org.tyranid.Imp._


object Uri {

  def completeUri( base:String ):String = {

    if ( base.indexOf( "://" ) != -1 )
      return base

    "http://" + base
  }
}

