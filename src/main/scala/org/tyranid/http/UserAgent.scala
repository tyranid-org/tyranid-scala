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

package org.tyranid.http

import scala.collection.mutable

import org.tyranid.Imp._
import org.tyranid.db.{ DbChar, DbIntSerial }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.MongoEntity
import org.tyranid.db.meta.AutoIncrement


object UserAgent extends MongoEntity( tid = "a0Dt" ) {
  "id"          is DbIntSerial   is 'id;
  "ua"          is DbChar(256)   is 'label as "User Agent";

  private val idByUa = mutable.HashMap[String,Int]()
  private val uaById = mutable.HashMap[Int,String]()

  def uaFor( id:Int ) = synchronized {
    uaById.getOrElseUpdate( id, {
      db.findOne( Mobj( "_id" -> id ) ) match {
      case null => "unknown"
      case to   => to.s( "ua" )
      }
    } )
  }

  def idFor( ua:String ) = synchronized {
    idByUa.getOrElseUpdate( ua, {
      db.findOne( Mobj( "ua" -> ua ) ) match {
      case null =>
        val id = AutoIncrement( "userAgent" )
        db.save( Mobj( "_id" -> id, "ua" -> ua ) )
        id

      case to =>
        to.i( "_id" )
      }
    } )
  }
}


