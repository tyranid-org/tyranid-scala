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
import scala.xml.NodeSeq

import org.bson.BSONObject
import com.mongodb.{ BasicDBObject, DB, DBCollection, DBObject }

import net.liftweb.http.SHtml

import org.tyranid.Bind
import org.tyranid.Imp.string
import org.tyranid.db.{ Domain, Entity, Record, View, ViewAttribute }
import org.tyranid.db.mongo.Imp._
import org.tyranid.math.Base64
import org.tyranid.ui.Field


case object DbMongoId extends Domain {
  val sqlName = "invalid"

  override def tid( r:Record, va:ViewAttribute ) = Base64.toString( r.oid( va ).toByteArray )

  override def ui( r:Record, f:Field, opts:(String,String)* ):NodeSeq =
    SHtml.text( r s f.va, v => r( f.va ) = v, "class" -> "textInput" ) 

  //override def inputcClasses = " select"
}

case class MongoEntity( tid:String ) extends Entity {

	override lazy val dbName = name.plural

  lazy val coll = Mongo.connect.db( Bind.ProfileDbName )( dbName )

  def create {}
  def drop   { coll.drop }
}

class MongoView( override val entity:MongoEntity ) extends View {

  private val byName = mutable.HashMap[String,ViewAttribute]()
  private val byIndex = mutable.HashMap[Int,ViewAttribute]()
  @volatile private var nextIndex = 0

  def vas = byName.values

  private def add( name:String ) = {
    val va = new ViewAttribute( this, entity.attrib( name ), nextIndex )
    nextIndex += 1
    byName( name ) = va
    byIndex( va.index ) = va
    va
  }

  def apply( name:String ) = byName.getOrElse( name, add( name ) )
  def apply( idx:Int )     = byIndex( idx )
}

class MongoRecord( override val view:MongoView ) extends Record with DBObject {

  val obj:DBObject = Mongo.obj
  val db:DBCollection = view.entity.coll

  private var temporaries:mutable.Map[String,AnyRef] = null

  def apply( va:ViewAttribute ) =
    if ( va.temporary ) {
      if ( temporaries == null )
        null // TODO:  return a canonical empty value based on attribute type
      else
        temporaries.get( va.name ).getOrElse( null ) // TODO:  return a canonical empty value based on attribute type
    } else {
      va.name match {
      case "id" => obj.get( "_id" )
      case s    => obj.get( s )
      }
    }

  def update( va:ViewAttribute, v:AnyRef ) =
    if ( va.temporary ) {
      if ( temporaries == null )
        temporaries = mutable.HashMap()
      temporaries( va.name ) = v
    } else {
      va.name match {
      case "id" => obj.put( "_id", v )
      case s    => obj.put( s, v )
      }
    }

  override def /( key:String ) = apply( key ).asInstanceOf[MongoRecord]

  override def save {
    throw new RuntimeException( "not supported yet" )
    //view.entity.coll.
  }


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

