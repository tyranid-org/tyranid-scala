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

import org.bson.BSONObject
import com.mongodb.{ BasicDBObject, DB, DBCollection, DBObject }

import org.tyranid.db.Entity


/**
 * IMPlicit IMPorts.
 */
object Imp {
  object Mongo {
    def connect = new com.mongodb.Mongo

    def obj = new DBObjectImp( new BasicDBObject )
  }

	implicit def mongoImp( mongo:com.mongodb.Mongo ) = new MongoImp( mongo )
	implicit def dbImp( db:DB )                      = new DBImp( db )
	implicit def collectionImp( coll:DBCollection )  = new DBCollectionImp( coll )
	implicit def objImp( obj:DBObject )              = new DBObjectImp( obj )
}

import Imp._


case class MongoImp( mongo:com.mongodb.Mongo ) {

  def db( name:String ) = mongo.getDB( name )
}


case class DBImp( db:com.mongodb.DB ) {
  def apply( name:String ) = db.getCollection( name )
}

case class DBCollectionImp( coll:DBCollection ) {

  def +=( obj:DBObject ) = coll.insert( obj )
}

trait DBValue {

  def /( name:String ):DBValue

  def string:String
  def int:Int
}

case class DBObjectImp( obj:DBObject ) extends DBObject with DBValue {

  def apply( key:String )            = /( key )
  def update( key:String, v:AnyRef ) = obj.put( key, v )


  /*
   * * *   DBValue
   */

  def /( name:String ) =
    obj.get( name ) match {
    case obj:DBObject => DBObjectImp( obj )
    case null         => MissingDBValue
    case v            => BasicDBValue( v )
    }

  def int = throw new IllegalArgumentException( obj + " is not convertible to int." )

  def string = obj.toString



  /*
   * * *   DBObject delegation
   */

  def containsField( s:String )      = obj.containsField( s )
  @deprecated( "use containsField" )
  def containsKey( s:String )        = obj.containsKey( s )
  def get( key:String )              = obj.get( key )
  def keySet                         = obj.keySet
  def put( key:String, v:AnyRef )    = obj.put( key, v )
  def putAll( o:BSONObject )         = obj.putAll( o )
  def putAll( m:java.util.Map[_,_] ) = obj.putAll( m )
  def removeField( key:String )      = obj.removeField( key )
  def toMap                          = obj.toMap
  def isPartialObject                = obj.isPartialObject
  def markAsPartialObject            = obj.markAsPartialObject
}

case class BasicDBValue( ref:AnyRef ) extends DBValue {

  def /( name:String ) = throw new IllegalArgumentException( ref + " is not an object." )

  def string = ref.toString

  def int =
    ref match {
    case n:Number  => n.intValue
    case s:String  => s.toInt
    case _         => throw new IllegalArgumentException( ref + " cannot be converted to an int." )
    }

  //implicit def native[A <: Any: Manifest]: A = ref.asInstanceOf[A]
}

case object MissingDBValue extends DBValue {

  def /( name:String ) = throw new IllegalArgumentException( "MissingDBValue is not an object." )

  def string = "[missing]"

  def int = throw new IllegalArgumentException( "MissingDBValue cannot be converted to an int." )
}

