/**
 * Copyright (c) 2008-2011 Tyranid <http://tyranid.org>
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

package org.tyranid.db.mongo

import scala.collection.mutable

import org.bson.BSONObject
import com.mongodb.{ BasicDBObject, DB, DBCollection, DBObject }

import org.tyranid.Bind
import org.tyranid.Imp.string
import org.tyranid.db.{ Entity, Record, View, ViewAttribute }
import org.tyranid.db.mongo.Imp._


class MongoEntity extends Entity {


	override lazy val dbName = name.plural

  lazy val coll = Mongo.connect.db( Bind.ProfileDbName )( dbName )


  def create {}
  def drop   { coll.drop }


}

class MongoView( override val entity:MongoEntity ) extends View {

  private val map = mutable.HashMap[String,ViewAttribute]()

  def apply( name:String ) =
    map.getOrElseUpdate( name, new ViewAttribute( this, entity.attrib( name ), -1 ) )
}

class MongoRecord( override val view:MongoView ) extends Record {

  val db:DBObject = Mongo.obj

  def apply( key:String )            = db.get( key )
  def update( key:String, v:AnyRef ) = db.put( key, v )

  override def /( key:String ) = apply( key ).asInstanceOf[MongoRecord]
}

