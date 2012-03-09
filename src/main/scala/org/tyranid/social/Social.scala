
package org.tyranid.social

import scala.xml.NodeSeq

import org.bson.types.ObjectId
import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.profile.{ Org, User }
import org.tyranid.web.Weblet


object Social {

  def createCompanies( fromOrgId:ObjectId, domains:Seq[String] ):Seq[Org] =
    B.linkedIn.createCompanies( fromOrgId, domains )

  def appFor( networkCode:String ) =
    networks.find( _.networkCode == networkCode ).get

  lazy val networks =
    Seq(
      B.linkedIn != null |* Some( B.linkedIn ),
      B.facebook != null |* Some( B.facebook )
    ).flatten
}

trait SoApp {
  val networkCode:String
  val networkName:String

  lazy val idName = networkCode + "id"

  def copyAttributes( from:User, to:User ):Unit
  def saveAttributes( user:User ):Unit
  def removeAttributes( user:DBObject ):Unit
  def loginButton( weblet:Weblet ):NodeSeq
  def exchangeToken:Boolean

  def importUser( user:User, uid:String )
}



