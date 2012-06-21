package org.tyranid.document

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

import scala.xml.NodeSeq

import org.bson.types.ObjectId
import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.cloud.aws.S3
import org.tyranid.db.mongo.Imp._
import org.tyranid.io.File
import org.tyranid.profile.{ Org, User }
import org.tyranid.web.Weblet


object Service {
  lazy val services =
    Seq(
      B.crocodoc != null |* Some( B.crocodoc )//,
      //B.scribd != null |* Some( B.scribd )
    ).flatten

  def appFor( serviceCode:String ) =
    services.find( _.serviceCode == serviceCode ).get
}

trait DocApp {
  val serviceCode:String
  val serviceName:String

  val supportedFormats:List[String] 
    
  lazy val idName = serviceCode + "id"

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



