
package org.tyranid.social

import org.bson.types.ObjectId
import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.profile.{ Org, User }


object Social {

  def createCompanies( fromOrgId:ObjectId, domains:Seq[String] ):Seq[Org] =
    B.linkedIn.createCompanies( fromOrgId, domains )
}

