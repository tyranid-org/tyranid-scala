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

import scala.collection.JavaConversions._

import org.bson.BSONObject
import org.bson.types.ObjectId

import com.mongodb.{ BasicDBList, BasicDBObject, DB, DBCollection, DBCursor, DBObject }

import org.tyranid.Imp._
import org.tyranid.any.Deep
import org.tyranid.bson.{ BsonObject, BsonList }


/**
 * IMPlicit IMPorts.
 */
object Imp {
  val $all         = "$all"
  val $and         = "$and"
  val $elemMatch   = "$elemMatch"
  val $exists      = "$exists"
  val $gt          = "$gt"
  val $gte         = "$gte"
  val $in          = "$in"
  val $inc         = "$inc"
  val $lt          = "$lt"
  val $lte         = "$lte"
  val $max         = "$max"
  val $maxDistance = "$maxDistance"
  val $min         = "$min"
  val $mod         = "$mod"
  val $ne          = "$ne"
  val $near        = "$near"
  val $nearSphere  = "$nearSphere"
  val $nin         = "$nin"
  val $not         = "$not"
  val $nor         = "$nor"
  val $options     = "$options"
  val $or          = "$or"
  val $regex       = "$regex"
  val $set         = "$set"
  val $size        = "$size"
  val $unset       = "$unset"
  val $where       = "$where"

  /*
  def $newset( vals: ( String, Any )* ):(String,Any) = {
    val o = new BasicDBObject

    for ( v <- vals )
      o.put( v._1, v._2 )

    "$set" -> new DBObjectImp( o )
  }
  */

  def $push( field:String, value:Any ) = Mobj( "$push" -> Mobj( field -> value ) )

	implicit def mongoImp( mongo:com.mongodb.Mongo ) = new MongoImp( mongo )
	implicit def dbImp( db:DB )                      = new DBImp( db )
	implicit def collectionImp( coll:DBCollection )  = new DBCollectionImp( coll )
	implicit def objImp( obj:DBObject )              = new DBObjectImp( obj )
	implicit def listImp( obj:BasicDBList )          = new DBListImp( obj )
	implicit def cursorImp( cursor:DBCursor )        = new DBCursorImp( cursor )

  object Mobj {
    def apply = new DBObjectImp( new BasicDBObject )
    def apply( vals: ( String, Any )* ):DBObjectImp = {
      val o = new BasicDBObject
      for ( v <- vals )
        o.put( v._1, v._2 )
      new DBObjectImp( o )
    }
  }

  object Mlist {
    def apply = new BasicDBList
    def apply( vals:Any* ):BasicDBList = {
      val l = new BasicDBList
      import Imp.listImp
      for ( i <- 0 until vals.size )
        l( i ) = vals( i ).asInstanceOf[AnyRef]
      l
    }
  }

  object Mongo {
    lazy val connect = new com.mongodb.Mongo( B.mongoHost )

    object EmptyArray extends BasicDBList {
      override def put( idx:Int, obj:AnyRef ) = throw new RuntimeException( "modifying immutable empty array" )
    }
  }
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
  
  // TODO:  is there a way to implement this without actually bringing the object back?
  def exists( query:DBObject ) = coll.findOne( query ) != null

  def updateId( id:ObjectId, update:DBObject ) =
    coll.update( Mobj( "_id" -> id ), update )

  /**
   * This attempts to find the object in the collection.  If it does not exist, a clone of the
   * query object is returned.
   */
  def findOrMake( query:DBObject ) =
    coll.findOne( query ) match {
    case null =>
      val o =
        query match {
        case o:DBObjectWrap  => o.obj.asInstanceOf[BasicDBObject]
        case b:BasicDBObject => b
        }

      o.deep.asInstanceOf[DBObject]
    case o    => o
    }
}

case class DBCursorImp( cursor:DBCursor ) extends Iterator[DBObject] {

  def hasNext = cursor.hasNext
  def next = cursor.next
}

trait DBValue {

  def /( name:String ):DBValue
  def string:String
  def int:Int
}

trait DBObjectWrap extends DBObject with BsonObject with DBValue {
  val obj:DBObject

  override def has( key:String ) = obj.containsField( key )
  def remove( key:String )       = obj.removeField( key )

  def rename( from:String, to:String ) = obj.put( to, obj.removeField( from ) )

  override def isNew = !has( "_id" )


  /*
   * * *   BsonObject
   */

  def keys = obj.keySet.toSeq

  override def o( key:String ):DBObjectWrap =
    apply( key ) match {
    case o:DBObjectWrap => o
    case o:DBObject     => DBObjectImp( o )
    case null           => null
    }

  
  /*
   * * *   DBObject delegation
   */

  def containsField( s:String )      = obj.containsField( s )
  @deprecated( message = "use containsField", since = "..." )
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

  override def deep:DBObjectWrap = {

    val newobj = new BasicDBObject

    import scala.collection.JavaConversions._
    for ( field <- keySet ) {
      get( field ) match {
      case v:BasicDBList => newobj.put( field, DBListWrap.deep( v ) )
      case v:Deep        => newobj.put( field, v.deep )
      case v:DBObject    => newobj.put( field, v.deep )
      case v             => newobj.put( field, v )
      }
    }

    newobj
  }
    
}

case class DBObjectImp( obj:DBObject ) extends DBObjectWrap with DBValue {

  def apply( key:String )         = obj.get( key )
  def update( key:String, v:Any ) = obj.put( key, v )
}

object DBListWrap {

  def deep( list:BasicDBList ):BasicDBList = {
    val newlist = new BasicDBList

    import scala.collection.JavaConversions._
    for ( field <- list.keySet ) {
      list.get( field ) match {
      case v:Deep     => newlist.put( field, v.deep )
      case v:DBObject => newlist.put( field, v.deep )
      case v          => newlist.put( field, v )
      }
    }

    newlist
  }

}

trait DBListWrap extends DBObjectWrap with BsonList {
  val obj:BasicDBList


  override def deep:DBListWrap = DBListWrap.deep( obj )

  /*
   * * *   Seq[Any] delegation
   */

  def apply( idx:Int )         = obj.get( idx )
  def update( idx:Int, v:Any ) = obj.put( idx, v )
  def length = obj.size

  import scala.collection.JavaConversions._
  def iterator = obj.iterator
}

case class DBListImp( obj:BasicDBList ) extends DBListWrap with Seq[Any] {

  def apply( key:String )         = obj.get( key )
  def update( key:String, v:Any ) = obj.put( key, v )


  /*
   * * *  Helper methods for when the list is a list of DBObjects
   */
   
  def nextId = if ( obj.size == 0 ) 1 else obj.map( _.asInstanceOf[DBObject] i 'aid ).max + 1
  
  def addAndId( aobj:DBObject ) = {
    aobj( "aid" ) = nextId
    obj.put( obj.size, aobj )
  }
  
  def find( id:Int ):Option[DBObject] = obj.map( _.asInstanceOf[DBObject] ).find( _.i( 'aid ) == id )
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

