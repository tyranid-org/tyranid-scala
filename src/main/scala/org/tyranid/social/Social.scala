/**
 * Copyright (c) 2008-2012 Tyranid <http://tyranid.org>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

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

  def pendingScripts = {
    val logoutNetwork = T.web.req.s( 'lo )
    logoutNetwork.notBlank |* appFor( logoutNetwork ).logoutScript
  }

  def removeCookies = networks foreach { _.removeCookies }
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
  def logoutScript:NodeSeq
  def removeCookies:Unit
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



