/**
 * Copyright (c) 2008-2012 Tyranid (   http://tyranid.org>
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

package org.tyranid.profile

import scala.collection.mutable
import scala.xml.NodeSeq

import org.tyranid.Imp._
import org.tyranid.db.{ DbChar, DbIntSerial, Record }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.MongoEntity
import org.tyranid.db.meta.AutoIncrement


object Tag extends MongoEntity( tid = "a0Ct" ) {
  "_id"         is DbIntSerial   is 'id;
  "name"        is DbChar(64)    is 'label;

  private val tags = mutable.HashMap[String,Int]()


  private val newPrefix = "$+$"

  def isNew( value:String ) = value.startsWith( newPrefix )

  def extractNew( value:String ) = value.substring( newPrefix.length )

  def idFor( tag:String ) = synchronized {
    tags.getOrElseUpdate( tag, {
      db.findOne( Mobj( "name" -> tag ) ) match {
      case null =>
        val id = AutoIncrement( "tag" )
        db.save( Mobj( "_id" -> id, "name" -> tag ) )
        id

      case to =>
        to.i( "_id" )
      }
    } )
  }

  def html( setId:String, r:Record ):NodeSeq = html( setId, r.tid, r.label )

  def html( setId:String, id:String, label:String ):NodeSeq =
    <li class="tag">
     <span>
      { label }
      <a class="closeTag"><i class="icon-remove"/></a>
      <input type="hidden" style="display:none;" value={ id } id={ setId } name={ setId + "[]" }/>
     </span>
    </li>
}

