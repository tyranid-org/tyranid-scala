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
 */

package org.tyranid.db.mongo

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.xml.NodeSeq

import org.bson.BSONObject
import org.bson.types.ObjectId
import com.mongodb.{ BasicDBObject, DB, DBCollection, DBObject }

import org.tyranid.Imp._
import org.tyranid.db.{ DbLink, Attribute, Domain, Entity, Record, Scope, View, ViewAttribute }
import org.tyranid.db.meta.Tid
import org.tyranid.db.mongo.Imp._
import org.tyranid.math.Base64
import org.tyranid.report.{ Run, Sort }
import org.tyranid.ui.{ Input, PathField, Search }


case object DbMongoId extends Domain {
  val sqlName = "invalid"

  override def idToRecordTid( v:Any ) =
    v match {
    case oid:ObjectId => Base64.toString( oid.toByteArray )
    case _ => null
    }

  override def recordTidToId( recordTid:String ) = new ObjectId( Base64.toBytes( recordTid ) )

  override def ui( s:Scope, f:PathField ) =
    Input( f.id, s.rec.s( f.va.name ), ( f.effOpts ++ Seq( "class" -> "textInput" ) ):_*  )

  //override def inputcClasses = " select"

  override def fromString( s:String ) = new ObjectId( s.trim )
}


case class MongoEntity( tid:String, embedded:Boolean = false ) extends Entity {
  val storageName = "MongoDB"

	override lazy val dbName = name.plural

  lazy val db = {
    if ( embedded )
      problem( "embedded mongodb entities do not have db objects" )

    Mongo.connect.db( B.profileDbName )( dbName )
  }

  lazy val makeView = MongoView( this )

  def make = apply( Mobj() )

  def make( obj:DBObject, parent:MongoRecord = null ) = MongoRecord( makeView, obj, parent )

  
  override def save( rec:Record ) = {
    try {
      db.save( rec.as[MongoRecord] )
    } catch {
    case e:IllegalArgumentException =>
      rec.as[MongoRecord].validateStructure // this should throw a more description exception
      throw e               // if validate passes, throw the original exception
    }

    super.save( rec ) // call after, so that tid is available
  }

  override def delete( rec:Record ) = {
    super.delete( rec )
    db.remove( Mobj( "_id" -> rec.id ) )
  }


  def create {}
  def drop   { db.drop }

  def apply( obj:DBObject ) = if ( obj != null ) MongoRecord( makeView, obj ) else null

  override def byRecordTid( recordTid:String ):Option[MongoRecord] = byId( recordTidToId( recordTid ) )

  def byId( id:Any ) = {
    val obj = db.findOne( id )
    obj != null |* Some( apply( obj ) )
  }

  def byTid( tid:String ) =
    if ( !tid.startsWith( this.tid ) ) {
      None
    } else {
      try {
        byRecordTid( tid.substring( 4 ) )
      } catch {
      case e =>
        e.logWith( "m" -> ( "tid[" + tid + "]" )  )
        None
      }
    }

  def remove( obj:DBObject ) = db.remove( obj )

  override def idLabels:Iterable[(AnyRef,String)] = {
    // TODO:  this should be labelAtt.dbName, but dbName by default is underscore-upper, and there is no MongoAttribute
    val labelName = labelAtt.map( _.name ).getOrElse { problem( "missing a label attribute" ) }

    db.find( Mobj(), Mobj( labelName -> 1 ) ).toSeq.
       map( obj => ( obj( '_id ), obj s labelName ) )
  }

  def labelFor( id:Any ):String =
    if ( isStatic ) {
      staticLabelFor( id.asInstanceOf[Long] )
    } else {
      // TODO:  this should be labelAtt.dbName, but dbName by default is underscore-upper, and there is no MongoAttribute
      val labelName = labelAtt.map( _.name ).getOrElse { return idToTid( id ) /* problem( "missing a label attribute" ) */ }
      val obj = db.findOne( Mobj( "_id" -> id ), Mobj( labelName -> 1 ) )

      if ( obj != null ) obj.s( labelName )
      else               ""
    }


  def recify( obj:Any, parent:MongoRecord, update: ( MongoRecord ) => Unit ):MongoRecord = {

    if ( obj.isInstanceOf[Record] )
      return obj.as[MongoRecord]

    val rec = make(
      if ( obj == null ) Mobj()
      else               obj.asInstanceOf[DBObject],
      parent ) // but we don't have that...
    update( rec )
    rec
  }

  override def records:Iterable[Record] = db.find.map( apply ).toIterable

  def query( run:Run, offset:Int = 0, count:Int = 20, sort:Sort = null ) = {
    val search = Mobj()

    for ( sf <- run.report.query.searchFields;
          value = run.report.searchRec( sf.name )
          if value != null )
      sf.mongoSearch( run, search, value )

    var cursor = db.find( search )//, Mobj() )
    if ( offset != 0 )
      cursor = cursor.skip( offset )

    if ( count != 0 )
      cursor = cursor.limit( count )

    if ( sort != null )
      cursor = cursor.sort( sort.sortObj )
    
    cursor.toIterable.map( apply )
  }
}

case class MongoView( override val entity:MongoEntity ) extends View {

  private val byName = mutable.HashMap[String,ViewAttribute]()
  private val byIndex = mutable.HashMap[Int,ViewAttribute]()
  @volatile private var nextIndex = 0

  private var cacheVas:Seq[ViewAttribute] = Nil
  private var rebuildVas = false

  def vas = synchronized {
    if ( rebuildVas || cacheVas.isEmpty ) {
      val values = byName.values

      val buf = new mutable.ArrayBuffer[ViewAttribute]( values.size )
      for ( v <- values )
        buf += v
      cacheVas = buf
      rebuildVas = false
    }

    cacheVas
  }

  // This preloads all of the view attributes for the entity associated with this view.
  entity.attribs.foreach { a => add( a.name ) }

  private def add( fullName:String ) = {
    val ( name, search ) = Search.extract( fullName )

    val att =
      if ( search == Search.Custom )
        new Attribute( entity, fullName )
      else
        entity.attrib( name )

    val va = new ViewAttribute( this, att, nextIndex, search = search, fullName = fullName )
    nextIndex += 1
    byName( fullName ) = va
    byIndex( va.index ) = va
    rebuildVas = true
    va
  }

  def apply( name:String ) = synchronized {
    byName.get( name ).getOrElse {
      val a = add( name )
      byName( name ) = a
      a
    }
  }

  def apply( idx:Int )     = synchronized { byIndex( idx ) }
}

case class MongoRecord( override val view:MongoView,
                        obj:DBObject = Mobj(),
                        override val parent:MongoRecord = null ) extends Record with DBObjectWrap {

  def db:DBCollection = view.entity.db

  override def entity = super.entity.asInstanceOf[MongoEntity]

  override def id = apply( "_id" )

  private var temporaries:mutable.Map[String,AnyRef] = null

  override def toString = super.toString + " temporaries:" + temporaries

  override def clear {
    for ( key <- obj.keySet.toSeq )
      obj.removeField( key )
    temporaries = null
  }

  override def deep:MongoRecord = {
    val copy = new MongoRecord( view, obj.deep, parent )

    if ( temporaries != null )
      copy.temporaries = temporaries.clone

    copy
  }

  def temporary( name:String ) =
    if ( temporaries != null ) temporaries.get( name )
    else                       None

  def temporary( name:String, value:Any ) {
    if ( temporaries == null ) temporaries = mutable.HashMap()
    temporaries( name ) = value.asInstanceOf[AnyRef]
  }

  def has( va:ViewAttribute ) =
    if ( va.temporary )
      temporaries != null && temporaries.get( va.name ).getOrElse( null ) != null
    else
      obj.has( va.name )

  def apply( va:ViewAttribute ) =
    if ( va.temporary ) {
      if ( temporaries == null )
        null // TODO:  return a canonical empty value based on attribute type
      else
        temporaries.get( va.name ).getOrElse( null ) // TODO:  return a canonical empty value based on attribute type
    } else {
      obj.get( va.name )
    }

  def update( va:ViewAttribute, v:Any ) =
    if ( va.temporary ) {
      if ( temporaries == null )
        temporaries = mutable.HashMap()
      temporaries( va.name ) = v.asInstanceOf[AnyRef]
    } else {
      obj.put( va.name, v )
    }

  override def remove( va:ViewAttribute ) = obj.removeField( va.name )

  override def o( va:ViewAttribute ):DBObjectWrap =
    apply( va ) match {
    case o:DBObjectWrap => o
    case o:DBObject     => DBObjectImp( o )
    case null           => null
    }

  def rec( va:ViewAttribute ):MongoRecord =
    va.att.domain.asInstanceOf[MongoEntity].recify( o( va ), this, rec => update( va, rec ) )

  def remove {
    db.remove( this )
  }
}

