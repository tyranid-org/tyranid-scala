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
import org.bson.types.ObjectId
import com.mongodb.{ BasicDBObject, DB, DBCollection, DBObject }

import net.liftweb.http.SHtml

import org.tyranid.Bind
import org.tyranid.Imp._
import org.tyranid.db.{ DbLink, Domain, Entity, Record, Scope, View, ViewAttribute }
import org.tyranid.db.mongo.Imp._
import org.tyranid.math.Base64
import org.tyranid.ui.Field


case object DbMongoId extends Domain {
  val sqlName = "invalid"

  override def tid( r:Record, va:ViewAttribute ) = Base64.toString( r.oid( va ).toByteArray )

  override def ui( s:Scope, f:Field, opts:(String,String)* ):NodeSeq =
    SHtml.text( s.rec s f.va, v => s.rec( f.va ) = v, "class" -> "textInput" ) 

  //override def inputcClasses = " select"
}

case class MongoEntity( tid:String ) extends Entity {

	override lazy val dbName = name.plural

  lazy val db = Mongo.connect.db( Bind.ProfileDbName )( dbName )

  lazy val makeView = MongoView( this )

  override def idLabels:Iterable[(AnyRef,String)] = {
    val labelName = labelAtt.get.name // TODO:  this should be labelAtt.dbName, but dbName by default is underscore-upper, and there is no MongoAttribute

    db.find( Mobj(), Mobj( labelName -> 1 ) )
      .map( obj => ( obj( '_id ), obj s labelName ) ).toIterable
  }

  def create {}
  def drop   { db.drop }

  def apply( obj:DBObject ) =
    if ( obj != null ) MongoRecord( makeView, obj ) else null

  override def byRecordTid( recordTid:String ):Option[MongoRecord] = {
    val obj = db.findOne( new ObjectId( Base64.toBytes( recordTid ) ) )

    obj != null |* Some( make( obj ) )
  }

  def make = MongoRecord( makeView, Mobj() )

  def make( obj:DBObject, parent:MongoRecord = null ) = MongoRecord( makeView, obj, parent )

  def remove( obj:DBObject ) = db.remove( obj )
}

case class MongoView( override val entity:MongoEntity ) extends View {

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

case class MongoRecord( override val view:MongoView,
                        obj:DBObject = Mobj(),
                        override val parent:MongoRecord = null ) extends Record with DBObjectWrap {

  val db:DBCollection = view.entity.db

  override def entity = super.entity.asInstanceOf[MongoEntity]

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

  def update( va:ViewAttribute, v:Any ) =
    if ( va.temporary ) {
      if ( temporaries == null )
        temporaries = mutable.HashMap()
      temporaries( va.name ) = v.asInstanceOf[AnyRef]
    } else {
      va.name match {
      case "id" => obj.put( "_id", v )
      case s    => obj.put( s, v )
      }
    }

  override def o( va:ViewAttribute ):DBObjectWrap =
    apply( va ) match {
    case o:DBObjectWrap => o
    case o:DBObject     => DBObjectImp( o )
    case null           => null
    }

  def rec( va:ViewAttribute ):MongoRecord = {
    var obj = o( va )

    if ( obj.isInstanceOf[Record] )
      return obj.asInstanceOf[MongoRecord]

    // embedded link ...
    //val en = va.att.domain.asInstanceOf[DbLink].toEntity
    // embedded record ...
    val en = va.att.domain.asInstanceOf[MongoEntity]

    if ( obj == null )
      obj = Mobj()

    val rec = en.make( obj, this )
    update( va, rec )
    rec
  }

  override def save {
    db.save( this )
  }

  def remove {
    db.remove( this )
  }
}

