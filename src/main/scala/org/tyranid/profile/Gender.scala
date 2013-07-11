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

package org.tyranid.profile

import org.tyranid.Imp._
import org.tyranid.db.{ DbChar, DbInt }
import org.tyranid.db.ram.RamEntity
import org.tyranid.db.tuple.{ Tuple, TupleView }


object Gender extends RamEntity( tid = "a00t" ) {
  type RecType = Gender
  override def convert( view:TupleView ) = new Gender( view )

  "_id"    is DbInt      is 'id;
  "name"   is DbChar(64) is 'label;


  override val addNames = Seq( "_id", "name" )

  val Male   = add( 1, "Male" )
  val Female = add( 2, "Female" )
  val Other  = add( 3, "Other" )


  def by( name:String ) =
    name.toLowerCase match {
    case "male"   | "m" => Male
    case "female" | "f" => Female
    case _              => null
    }
}

case class Gender( override val view:TupleView ) extends Tuple( view ) {

  def name = s( 'name )
}

