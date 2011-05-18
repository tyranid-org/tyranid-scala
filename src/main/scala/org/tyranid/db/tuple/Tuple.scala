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
	lazy val ekeys   = eleaves.filter( _.att.isKey )
	lazy val elabels = eleaves.filter( _.att.isLabelAtt )
}


class Tuple( val view:TupleView ) extends Record {

	val values = new Array[AnyRef]( view.leafCount )

	def apply( index: Int ) = values( index )
	def update( index: Int, v:AnyRef )       = values( index ) = v

  def apply( va:ViewAttribute ) = apply( va.index )
  def update( va:ViewAttribute, v:AnyRef ) = values( va.index ) = v

	def see( index: Int ) = view.leaves( index ).att.domain.see( apply( index ) )

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

	def isNew = view.ekeys.findIndexOf( va => values( va.index ) == null ) != -1
}


