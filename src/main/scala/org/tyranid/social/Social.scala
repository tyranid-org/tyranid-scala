
package org.tyranid.social

import org.tyranid.Imp._
import org.tyranid.profile.User
import org.tyranid.social.linkedIn.LinkedIn


object Social {


  def createCompany( user:User, domain:String ) = {
    LinkedIn.createCompany( user, domain )
  }
}

