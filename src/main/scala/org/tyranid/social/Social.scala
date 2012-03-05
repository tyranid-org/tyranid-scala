
package org.tyranid.social

import org.bson.types.ObjectId
import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.profile.{ Org, User }
import org.tyranid.social.linkedIn.LinkedIn


object Social {

  def createCompanies( fromOrgId:ObjectId, domains:Seq[String] ):Seq[Org] =
    LinkedIn.createCompanies( fromOrgId, domains )
}

