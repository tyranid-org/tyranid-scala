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

package org.tyranid.db

import scala.collection.mutable.{ ArrayBuffer, HashMap }

import org.tyranid.Imp.stringImp



class ModelException( msg:String )            extends RuntimeException( msg )
class MissingAttributeException( msg:String ) extends ModelException( msg )


trait DbItem {
	val name: String

	lazy val dbName = name.camelCaseToUnderLower
}


/*
 * * *   S c h e m a
 */

object Schema {

	private val entities = new ArrayBuffer[Entity]

	val byDbName = new HashMap[String,Entity]

	def recreate = entities foreach { _.recreate }

	def add( ens: Entity* ) { ens foreach { add( _ ) } }

	def add( en: Entity ) {
		entities += en
		byDbName( en.dbName ) = en
	}

	def main( args: Array[String] ) = {
		args foreach {
			_ match {
			case "recreate" => recreate
			case t          => throw new RuntimeException( "Unknown option: " + t )
			}
		}
	}

}



