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

import java.io.File

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
      B.crocodoc != null |* Some( B.crocodoc ),
      B.scribddoc != null |* Some( B.scribddoc )
    ).flatten

  def appFor( serviceCode:String ) = services.find( _.serviceCode == serviceCode ).getOrElse( null )
  
  def statusFor( extDocId:String ) = {
    if ( extDocId.notBlank ) {
      val parts = extDocId.split( "," )
      appFor( parts(0) ).statusFor( parts.drop(1).mkString( "," ) )
    } else
      null
  }
  
  def appCodeForId( extDocId:String ):String = {
    if ( extDocId.notBlank ) {
      val parts = extDocId.split( "," )
      appFor( parts(0) ).serviceCode
    } else
      null
  }
  
  def getThumbnailFile( extDocId:String, width:Int = 300, height:Int = 300 ) = {
    if ( extDocId.notBlank ) {
      val parts = extDocId.split( "," )
      appFor( parts(0 ) ).getThumbnailFile( parts.drop(1).mkString( "," ) )
    } else 
      null
  }
  
  def previewParams( extDocId:String, width:String, height:String ):Map[String,AnyRef] = {
    if ( extDocId.notBlank ) {
      val parts = extDocId.split( "," )
      appFor( parts(0 ) ).previewParams( parts.drop(1).mkString( "," ), width, height )
    } else {
      Map()
    }
  }
  
  def delete( extDocId:String ) = {
    if ( extDocId.notBlank ) {
      val parts = extDocId.split( "," )
      appFor( parts(0 ) ).delete( parts.drop(1).mkString( "," ) )
    } else 
      false
  }
}

trait DocApp {
  val serviceCode:String
  val serviceName:String
  val websiteUrl:String
  val supportedFormats:List[String] 
    
  def upload( file:File, fileSize:Long, filename:String ):String
  def statusFor( extDocId:String ):String
  def getThumbnailFile( extDocId:String, width:Int = 300, height:Int = 300 ):File
  def previewParams( extDocId:String, width:String, height:String ):Map[String,AnyRef]
  def delete( extDocId:String ): Boolean
  
  protected def externalDocId( extDocId:String ) =
    ( extDocId.notBlank ) ? ( serviceCode + "," + extDocId ) | null

  def filetypeFor( filename:String ) = filename.substring( filename.lastIndexOf( '.' ) + 1 )
  def supports( ext:String ) = supportedFormats.contains( ext.toUpperCase )
}
