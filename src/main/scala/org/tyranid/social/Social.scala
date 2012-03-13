
package org.tyranid.social

import scala.xml.NodeSeq

import org.bson.types.ObjectId
import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.db.mongo.Imp._
import org.tyranid.profile.{ Org, User }
import org.tyranid.web.Weblet


object Social {
  lazy val networks =
    Seq(
      B.linkedIn != null |* Some( B.linkedIn ),
      B.facebook != null |* Some( B.facebook )
    ).flatten

  def createCompanies( fromOrgId:ObjectId, domains:Seq[String] ):Seq[Org] =
    B.linkedIn.createCompanies( fromOrgId, domains )

  def appFor( networkCode:String ) =
    networks.find( _.networkCode == networkCode ).get
}

trait SoApp {
  val networkCode:String
  val networkName:String

  lazy val idName = networkCode + "id"

  val logo:String

  def copyAttributes( from:User, to:User ):Unit
  def saveAttributes( user:User ):Unit
  def removeAttributes( user:DBObject ):Unit
  def loginButton( weblet:Weblet ):NodeSeq
  def linkButton:NodeSeq
  def linkPreview( user:User ):NodeSeq
  def isActive:Boolean
  def exchangeToken:Boolean

  def importUser( user:User, uid:String )

  def exchangeAttributes( user:User ) = {

    val uid = user.s( idName )

    if ( user.id != null ) {
      val existing = B.User.db.findOne( Mobj( idName -> uid ) )
      if ( existing != null && user.id != null && existing.id != user.id )
        removeAttributes( existing )
    }

    if ( !user.isNew )
      saveAttributes( user )
  }
}



