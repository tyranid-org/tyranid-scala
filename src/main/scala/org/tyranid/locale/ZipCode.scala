/**
 * Copyright (c) 2008-2011 Tyranid (   http://tyranid.org>
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

package org.tyranid.locale

import org.tyranid.Bind
import org.tyranid.Imp._
import org.tyranid.db.{ DbInt, DbChar, DbLink, Record }
import org.tyranid.db.mongo.Imp._

case class LatLong( lat:Double, long:Double )

object ZipCode {

  lazy val zipdatadb = {
    val db = Mongo.connect.db( Bind.ProfileDbName )( "zipdata" )
    db.ensureIndex( Mobj( "ZipCode" -> 1 ) )
    db
  }

  def latLongFor( zipCode:Int ) = {

    val obj = zipdatadb.findOne( Mobj( "ZipCode" -> zipCode ) )
    obj != null |* Some( LatLong( lat = obj.d( 'Latitude ), long = obj.d( 'Longitude ) ) )
  }

  
}

