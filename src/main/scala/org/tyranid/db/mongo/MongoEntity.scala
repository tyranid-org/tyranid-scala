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
 */

package org.tyranid.db.mongo

import scala.collection.JavaConversions._
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

  override def tid( r:Record, va:ViewAttribute ) = {
    val oid = r.oid( va )

    if ( oid == null )
      null
    else
      Base64.toString( oid.toByteArray )
  }

  override def ui( s:Scope, f:Field, opts:(String,String)* ):NodeSeq =
    SHtml.text( s.rec s f.va, v => s.rec( f.va ) = v, "class" -> "textInput" ) 

  //override def inputcClasses = " select"
}

case class MongoEntity( tid:String ) extends Entity {

	override lazy val dbName = name.plural

  lazy val db = Mongo.connect.db( Bind.ProfileDbName )( dbName )

  lazy val makeView = MongoView( this )

  
  override def save( rec:Record ) = {
    db.save( rec.asInstanceOf[MongoRecord] )
    super.save( rec ) // call after, so that tid is available
  }


  def create {}
  def drop   { db.drop }

  def apply( obj:DBObject ) =
    if ( obj != null ) MongoRecord( makeView, obj ) else null


  def toTid( oid:ObjectId ) = tid + Base64.toString( oid.toByteArray )
  
  def tidToOid( tid:String ) = {

    val ( entityTid, recordTid ) = tid.splitAt( 4 )

    assert( Entity.byTid( entityTid ).get == this )

    idFromRecordTid( recordTid )
  }

  def idFromRecordTid( recordTid:String ) = new ObjectId( Base64.toBytes( recordTid ) )
  
  override def byRecordTid( recordTid:String ):Option[MongoRecord] =
    byId( idFromRecordTid( recordTid ) )

  def byId( id:AnyRef ) = {
    val obj = db.findOne( id )
    obj != null |* Some( apply( obj ) )
  }

  def make = apply( Mobj() )

  def make( obj:DBObject, parent:MongoRecord = null ) = MongoRecord( makeView, obj, parent )

  def remove( obj:DBObject ) = db.remove( obj )


  override def idLabels:Iterable[(AnyRef,String)] = {
    val labelName = labelAtt.get.name // TODO:  this should be labelAtt.dbName, but dbName by default is underscore-upper, and there is no MongoAttribute

    db.find( Mobj(), Mobj( labelName -> 1 ) ).toSeq.
       map( obj => ( obj( '_id ), obj s labelName ) )
  }

  def labelFor( id:Any ) =
    if ( isStatic ) {
      staticLabelFor( id.asInstanceOf[Long] )
    } else {
      val labelName = labelAtt.get.name // TODO:  this should be labelAtt.dbName, but dbName by default is underscore-upper, and there is no MongoAttribute

      val obj = db.findOne( Mobj( "_id" -> id ), Mobj( labelName -> 1 ) )

      if ( obj != null ) obj.s( labelName )
      else               ""
    }


  def recify( obj:Any, parent:MongoRecord, update: ( MongoRecord ) => Unit ):MongoRecord = {

    if ( obj.isInstanceOf[Record] )
      return obj.asInstanceOf[MongoRecord]

    val rec = make(
      if ( obj == null ) Mobj()
      else               obj.asInstanceOf[DBObject],
      parent ) // but we don't have that...
    update( rec )
    rec
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

  private def add( name:String ) = {
    val va = new ViewAttribute( this, entity.attrib( name ), nextIndex )
    nextIndex += 1
    byName( name ) = va
    byIndex( va.index ) = va
    rebuildVas = true
    va
  }

  def apply( name:String ) = synchronized {
    var rslt = byName.get( name )

    if ( rslt == None ) {
      val a = add( name )


      byName( name ) = a
      a
      
    } else {
      rslt.get
    }

    //byName.getOrElse( name, add( name ) )
  }
  def apply( idx:Int )     = synchronized { byIndex( idx ) }
}

case class MongoRecord( override val view:MongoView,
                        obj:DBObject = Mobj(),
                        override val parent:MongoRecord = null ) extends Record with DBObjectWrap {

  def db:DBCollection = view.entity.db

  override def entity = super.entity.asInstanceOf[MongoEntity]

  override def id = apply( "id" )

  private var temporaries:mutable.Map[String,AnyRef] = null

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
    if ( va.temporary ) {
      temporaries != null && temporaries.get( va.name ).getOrElse( null ) != null
    } else {
      va.name match {
      case "id" => obj.has( "_id" )
      case s    => obj.has( s )
      }
    }

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

  def rec( va:ViewAttribute ):MongoRecord =
    va.att.domain.asInstanceOf[MongoEntity].recify( o( va ), this, rec => update( va, rec ) )

  def remove {
    db.remove( this )
  }
}

