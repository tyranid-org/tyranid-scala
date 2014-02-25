/**
 * Copyright (c) 2008-2014 Tyranid <http://tyranid.org>
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
    case s:String     => Base64.toString( s._oid.toByteArray ) 
    case _ => null
    }

  override def recordTidToId( recordTid:String ) =
    if ( recordTid.isBlank || recordTid == "null" )
      null
    else
      new ObjectId( Base64.toBytes( recordTid ) )

  override def ui( s:Scope, f:PathField ) =
    Input( f.id, get( s, f )._s, ( f.effOpts ++ Seq( "class" -> "textInput" ) ):_*  )

  override def fromString( s:String ) = new ObjectId( s.trim )
}

case class MongoEntity( tid:String, embedded:Boolean = false ) extends Entity {
  type RecType >: Null <: MongoRecord

  val storageName = "MongoDB"

	override lazy val dbName = name.plural

	@transient var _db:DBCollection = null
	
  def db = {
    if ( _db == null ) {
      if ( embedded )
        problem( "embedded mongodb entities do not have db objects" )
  
      _db = Mongo.connect.db( B.profileDbName )( dbName )
    }
    
    _db
  }

  @transient lazy val makeView = MongoView( this )
  
  def make = apply( Mobj() )

  def make( obj:DBObject, parent:MongoRecord = null ) = apply( obj, parent )

  
  override def save( rec:Record ) = {
    rec.compute( temporary = false )

    val mrec = rec.as[MongoRecord]
    
    try {
      mrec.db.save( mrec )
    } catch {
    case e:IllegalArgumentException =>
      mrec.validateStructure // this should throw a more description exception
      throw e               // if validate passes, throw the original exception
    }

    super.save( rec ) // call after, so that tid is available
  }

  override def delete( id:Any ) = {
    super.delete( id )
    db.remove( Mobj( "_id" -> id ) )
  }
  
  override def delete( rec:Record ) = {
    super.delete( rec )
    rec.as[MongoRecord].db.remove( Mobj( "_id" -> rec.id ) )
  }


  def create {}
  def drop   { db.drop }

  def convert( obj:DBObject, parent:MongoRecord = null ):RecType = MongoRecord( makeView, obj, parent ).as[RecType]

  final def apply( obj:DBObject, parent:MongoRecord ):RecType = 
    obj match {
    case null               => null
    case record:MongoRecord => record.as[RecType]
    case obj:DBObject       => convert( obj, parent )
    }

  final def apply( obj:DBObject ):RecType = apply( obj, null )

  override def byRecordTid( recordTid:String ):Option[RecType] = byId( recordTidToId( recordTid ) )

  override def getByRecordTid( recordTid:String ):RecType = getById( recordTidToId( recordTid ) )

  def    byId( id:Any ) = Option( getById( id ) )

  def getById( id:Any ) =
    try {
      //if ( T.tidCache.has( idToTid( id ) ) ) sp am( "CACHE: HIT  (size=" + T.tidCache.size + ")" )
      T.tidCache.byTid.getOrElseUpdate( idToTid( id ), queryById( id ) ).as[RecType]
      // TODO:  if we already had it here should we clone it ?
    } catch {
    case e:Throwable =>
      e.logWith( "m" -> ( "tid[" + tid + "]" )  )
      null
    }

  def    byTid( tid:String ) = Option( getByTid( tid ) )

  def getByTid( tid:String ) =
    if ( tid.isBlank || !tid.startsWith( this.tid ) ) {
      null
    } else {
      try {
        //if ( T.tidCache.has( tid ) ) sp am( "CACHE: HIT  (size=" + T.tidCache.size + ")" )
        T.tidCache.byTid.getOrElseUpdate( tid, queryById( recordTidToId( tid.substring( 4 ) ) ) ).as[RecType]
        // TODO:  if we already had it here should we clone it ?
      } catch {
      case e:Throwable =>
        e.logWith( "m" -> ( "tid[" + tid + "]" )  )
        null
      }
    }

  def queryById( id:Any ) = {
    val c = db.find( Mobj( "_id" -> id ) ).limit(1)
    apply( c.hasNext ? c.next | null )
  }

  def getByIds( ids:Seq[Any] )      = _getByTIds( ids = ids,                 tids = ids.map( idToTid ) )
  def getByTids( tids:Seq[String] ) = _getByTIds( ids = tids.map( tidToId ), tids = tids               )

  /**
   * This is an internal method not meant to be used except by getByIds and getByTids.  The ids and tids array need to match exactly.
   */
  private def _getByTIds( ids:Seq[Any], tids:Seq[String] ):Seq[RecType] = {
    val tc = T.tidCache.byTid

    val size = ids.size

    val recs = new mutable.ArrayBuffer[RecType]( size )
    val idsToQuery = Mlist()

    for ( i <- 0 until size ) {
      val tid = tids( i )

      tc.get( tid ) match {
      case Some( rec ) =>
        recs += rec.asInstanceOf[RecType]

      case None =>
        idsToQuery += ids( i ).asInstanceOf[AnyRef]
        recs += null
      }
    }

    for ( rec <- db.find( Mobj( "_id" -> Mobj( $in -> idsToQuery ) ) ).map( apply ).toSeq;
          idx = ids.indexOf( rec.id ) ) {
      recs( idx ) = rec
      T.tidCache.byTid( tids( idx ) ) = rec
    }

    if ( idsToQuery.nonEmpty && recs.exists( _ == null ) )
      recs.filter( _ != null )
    else
      recs
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
      val objc = db.find( Mobj( "_id" -> id ), Mobj( labelName -> 1 ) ).limit(1)
      val obj = objc.hasNext ? objc.next | null

      if ( obj != null ) obj.s( labelName )
      else               ""
    }


  def recify( obj:Any, parent:MongoRecord, update: ( MongoRecord ) => Unit ):RecType = {

    if ( obj.isInstanceOf[Record] )
      return obj.as[RecType]

    val rec = make(
      if ( obj == null ) Mobj()
      else               obj.asInstanceOf[DBObject],
      parent ) // but we don't have that...
    update( rec )
    rec
  }

  override def records:Iterable[RecType] = db.find.map( apply ).toIterable

  def query( run:Run, offset:Int = 0, count:Int = 20, sort:Sort = null ) = {
    val search = Mobj()

    for ( sf <- run.report.query.searchFields;
          value = run.report.searchRec( sf.name ) ) 
      sf.mongoSearch( run, search, value )

    var cursor = db.find( search )//, Mobj() )
    if ( offset != 0 )
      cursor = cursor.skip( offset )

    if ( count != 0 )
      cursor = cursor.limit( count )

    if ( sort != null )
      cursor = cursor.sort( sort.sortObj )
    
    cursor.toIterable.map( obj => apply( obj, null ) )
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
  assert( obj != null && !obj.is[Record] )

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
    val copy = entity.convert( obj.deep, parent )

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

  override def has( key:String ) = has( view( key ) ) // override has() in DBObjectWrap
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

  /**
   * The default is to do a "truthy" set.  i.e. the value is only set if the value is "truthy" otherwise the field is removed.
   *
   * If you need to set a non-truthy value (like you want a blank string or a false value stored) use rec.set(...) instead of rec(...).
   *
   * TODO:  we might also want to add an option on attribute, something like "is 'falsey" or something to sidestep this before for a particular
   *        attribute.
   */
  def update( va:ViewAttribute, v:Any ) =
    if ( va.temporary ) {
      if ( temporaries == null )
        temporaries = mutable.HashMap()
      temporaries( va.name ) = v.asInstanceOf[AnyRef]
    } else {
      v match {
      case b:Boolean =>
        if ( b )          obj.put( va.name, true )
        else              obj.remove( va.name )

      case s:String =>
        if ( s.notBlank ) obj.put( va.name, s )
        else              obj.remove( va.name )

      case _ =>
        if ( v != null )  obj.put( va.name, v )
        else              obj.remove( va.name )
      }
    }

  override def remove( key:String ) = remove( view( key ) ) // override remove() in DBObjectWrap
  override def remove( va:ViewAttribute ) =
    if ( va.temporary ) {
      if ( temporaries != null )
        temporaries.remove( va.name )
    } else {
      obj.removeField( va.name )
    }


  override def o( va:ViewAttribute ):DBObjectWrap =
    apply( va ) match {
    case o:DBObjectWrap => o
    case o:DBObject     => DBObjectImp( o )
    case null           => null
    }

  def rec( va:ViewAttribute ):MongoRecord =
    va.att.domain match {
    case me:MongoEntity =>
      me.asInstanceOf[MongoEntity].recify( o( va ), this, rec => update( va, rec ) )

    case link:DbLink =>
      // TODO:  we have an id and we have the entity, we shouldn't have to convert to a tid here, but the API isn't there yet
      Record.getByTid( tid( va ) ).as[MongoRecord]
    }

  def remove {
    db.remove( this )
  }

  def update( update:DBObject ) = db.update( Mobj( "_id" -> this.id ), update )

  def inc( key:String, value:Int = 1 ) = {
    this( key ) = i( key ) + value
    update( Mobj( $inc -> Mobj( key -> value ) ) )
  }
}

