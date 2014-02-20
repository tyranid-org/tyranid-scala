/**
 * Copyright (c) 2008-2013 Tyranid <http://tyranid.org>
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

package org.tyranid.db.tuple

import org.tyranid.db.{ Attribute, Record, View, ViewAttribute }

class TupleView extends View {
	var leaves:Array[ViewAttribute] = null
	def leafCount = leaves.size

  def apply( name:String ) = leaves.find( _.name == name ).get
  def apply( idx:Int )     = leaves( idx )

  def vas = leaves

	lazy val entity  = leaves( 0 ).att.entity
	lazy val eleaves = leaves.filter( _.att.entity == entity )
	lazy val ekeys   = eleaves.filter( _.att.isId )
	lazy val elabels = eleaves.filter( _.att.isLabel )
}


class Tuple( val view:TupleView ) extends Record {

  override def id = apply( "_id" )

  override def equals( t:Any ) =
    t match {
    case null => false
    case t:Tuple => id == t.id
    case _ => false
    }
  
  def keys = view.leaves.map( _.name )

  val parent = null

	val values = new Array[AnyRef]( view.leafCount )

  def has( index:Int )                = values( index ) != null
  def has( va:ViewAttribute ):Boolean = has( va.index )

	def apply( index:Int )         = values( index )
	def update( index:Int, v:Any ) = values( index ) = v.asInstanceOf[AnyRef]
  def remove( index:Int )        = values( index ) = null

  def removeKey( key:String ) = update( key, null )

  def apply( va:ViewAttribute )         = apply( va.index )
  def update( va:ViewAttribute, v:Any ) = values( va.index ) = v.asInstanceOf[AnyRef]
  def remove( va:ViewAttribute )        = values( va.index ) = null

	def see( index:Int ) = view.leaves( index ).att.domain.see( apply( index ) )

  override def clear {
    for ( i <- 0 until values.length )
      values( i ) = null
  }

  def deep:Tuple = throw new UnsupportedOperationException

	override def toString = {
		val sb = new StringBuilder
		sb += '('
		for ( i <- 0 until values.length ) {
			if ( i > 0 ) sb += ','
			sb ++= view.leaves( i ).att.name + '=' ++= values( i ).toString
		}

		sb += ')'
		sb.toString
	}

	override def isNew = view.ekeys.indexWhere( va => values( va.index ) == null ) != -1

  def rec( va:ViewAttribute ):Tuple = throw new UnsupportedOperationException
}


