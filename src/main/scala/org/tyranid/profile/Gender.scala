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

package org.tyranid.profile

import org.tyranid.Imp._
import org.tyranid.db.{ DbChar, DbInt, EnumEntity }
import org.tyranid.db.ram.RamEntity
import org.tyranid.db.tuple.Tuple


object Gender extends RamEntity( tid = "a00t" ) with EnumEntity[Gender] {
  "id"     is DbInt      is 'id;
  "name"   is DbChar(64) is 'label;

  def apply( id:Int, name:String ) = {
    val t = new Gender
    t( 'id ) = id
    t( 'name ) = name
    t
  }

  val Male   = apply( 1, "Male" )
  val Female = apply( 2, "Female" )
  val Other  = apply( 3, "Other" )

  static( Male, Female, Other )

  def by( name:String ) =
    name.toLowerCase match {
    case "male"   | "m" => Male
    case "female" | "f" => Female
    case _              => null
    }
}

class Gender extends Tuple( Gender.makeView ) {

  def name = s( 'name )
}

