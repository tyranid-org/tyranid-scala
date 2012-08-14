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

package org.tyranid.content


import org.tyranid.Imp._
import org.tyranid.db.{ DbChar, DbDateTime, DbInt, DbLink, EnumEntity }
import org.tyranid.db.mongo.MongoEntity
import org.tyranid.db.ram.RamEntity
import org.tyranid.db.tuple.Tuple



object ViewType extends RamEntity( tid = "a13v" ) with EnumEntity[ViewType] {
  "_id"    is DbInt      is 'id;
  "name"   is DbChar(64) is 'label;

  def apply( id:Int, name:String ) = {
    val t = new ViewType
    t( '_id )  = id
    t( 'name ) = name
    t
  }

  val Post  = apply( 1, "Post"  )
  val Card  = apply( 2, "Card"  )
  val Table = apply( 3, "Table" )
  val Grid  = apply( 4, "Grid"  )

  static( Post, Card, Table, Grid )
}

class ViewType extends Tuple( ViewType.makeView )

object ContentType extends RamEntity( tid = "a10v" ) with EnumEntity[ContentType] {
  "_id"    is DbInt      is 'id;
  "name"   is DbChar(64) is 'label;

  def apply( id:Int, name:String ) = {
    val t = new ContentType
    t( '_id )  = id
    t( 'name ) = name
    t
  }

  val ChangeLog          = apply( 1, "ChangeLog" )
  val Message            = apply( 2, "Message" )
  val Content            = apply( 3, "Content" )
  val Folder             = apply( 4, "Folder" )
  val Document           = apply( 5, "Document" )

  static( ChangeLog, Message, Content, Folder, Document )
}

class ContentType extends Tuple( ContentType.makeView )




object Reply extends MongoEntity( tid = "b00w", embedded = true ) {
  "on"             is DbDateTime           ;
  "m"              is DbChar(1024)         is 'label;

  override def init = {
    super.init
    "u"            is DbLink(B.User)       ;           // user who created the reply
  }

}

