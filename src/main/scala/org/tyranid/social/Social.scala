
package org.tyranid.social

import org.bson.types.ObjectId
import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.profile.User
import org.tyranid.social.linkedIn.LinkedIn


object Social {

  // TODO:  this should return Org objects once we have Orgs in Tyranid
  def createCompanies( fromOrgId:ObjectId, domains:Seq[String] ):Seq[DBObject] = {
    //LinkedIn.createCompany( user, domain )

    Nil
  }
}

