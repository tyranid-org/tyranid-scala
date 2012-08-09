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

package org.tyranid.db.ram

import scala.collection.mutable.{ ArrayBuffer, HashMap }

import org.tyranid.Imp._
import org.tyranid.db.{ DbIntSerial, DbChar, Entity, View, ViewAttribute, Record }
import org.tyranid.db.tuple.{ Tuple, TupleView }
import org.tyranid.report.{ Run, Sort }


case class RamEntity( tid:String ) extends Entity {
  type RecType >: Null <: Tuple
  def convert( view:TupleView ) = ( new Tuple( view ) ).as[RecType]

  val storageName = "RAM"
  val embedded = false

	override lazy val dbName = name.plural

  def viewFor( names:String* ) = {
		val v = new TupleView
    val len = names.size
		val vas = new Array[ViewAttribute]( len )
    for ( i <- 0 until len )
      vas( i ) = new ViewAttribute( v, attrib( names( i ) ), i )

    v.leaves = vas
    v
  }

  lazy val makeView:TupleView = {
    val v = new TupleView

    val len = attribs.size
    val vas = new Array[ViewAttribute]( len )

    for ( i <- 0 until len )
      vas( i ) = new ViewAttribute( v, attribs( i ), i )

    v.leaves = vas
    v
  }

  def make = convert( makeView )

  override def makeTuple( view:TupleView ) = convert( view )

	def create {}
	def drop   { /* TODO */ }

  def labelFor( id:Any ) =
    id match {
    case l:Long => staticLabelFor( l )
    case i:Int  => staticLabelFor( i.toLong )
    case null   => ""
    case s:String =>
      val l = s.toLaxLong
      if ( l != 0 ) staticLabelFor( l )
      else          s
    case id     => id.toString
    }

  override def records = staticRecords

  def query( run:Run, offset:Int = 0, count:Int = 20, sort:Sort = null ) = {

    var allRecs = records.filter( _.matchesSearch( run ) )

    if ( sort != null )
      allRecs = allRecs.sortWith( sort.comparator )

    if ( offset != 0 || count != 0 )
      allRecs.slice( offset, offset + count )
    else
      allRecs
  }

  override def idLabels:Seq[(AnyRef,String)] = staticRecords.map( _.idLabel )

  def byId( id:Long ) = staticIdIndex.get( id ).map( _.as[RecType] )

  def getById( id:Long ) = byId( id ).orNull

  override def byRecordTid( recordTid:String ):Option[RecType] =
    byId( recordTidToId( recordTid )._l )

  override def getByRecordTid( recordTid:String ):RecType =
    byId( recordTidToId( recordTid )._l ) getOrElse null

  override def save( rec:Record )   = throw new UnsupportedOperationException
  override def delete( rec:Record ) = throw new UnsupportedOperationException
}


abstract class RamEnumEntity( nameLen:Int, tid:String ) extends RamEntity( tid ) {

	"_id"  is DbIntSerial       is 'id   ;
	"name" is DbChar( nameLen ) is 'label ;
}

