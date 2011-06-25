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

package org.tyranid.db.ram

import scala.collection.mutable.{ ArrayBuffer, HashMap }

import org.tyranid.Imp.{ string, symbol }
import org.tyranid.db.{ DbIntSerial, DbChar, Entity }


case class RamEntity( tid:String ) extends Entity {

	override lazy val dbName = name.plural

  def makeView = throw new UnsupportedOperationException

	def create {}
	def drop   { /* TODO */ }

  def labelFor( id:Any ) =
    id match {
    case l:Long => staticLabelFor( l )
    case i:Int  => staticLabelFor( i.toLong )
    case id     => id.toString
    }

  override def idLabels:Seq[(AnyRef,String)] = staticRecords.map( _.idLabel )
}


abstract class RamEnumEntity( nameLen:Int, tid:String ) extends RamEntity( tid ) {

	"id"   is DbIntSerial       is 'key   ;
	"name" is DbChar( nameLen ) is 'label ;
}

