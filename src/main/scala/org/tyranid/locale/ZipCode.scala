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

import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.db.{ DbInt, DbChar, DbLink, Record }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.MongoEntity


object ZipCode extends MongoEntity( tid = "a0Ft" ) {

  override lazy val dbName = "zipdata"

  def latLongFor( zipCode:Int ) = {

    val obj = db.findOne( Mobj( "ZipCode" -> zipCode ) )
    obj != null |* Some( LatLong( lat = obj.d( 'Latitude ), long = obj.d( 'Longitude ) ) )
  }

  def forFips6( fips6:String ):Seq[DBObject] = {
    // FIPS 6-4:  http://www.itl.nist.gov/fipspubs/co-codes/states.txt
    //            state code + county code

    val split =
      fips6.length match {
      case 5 => 2
      case 6 => 3
      case _ => return Nil
      }

    val stateFips  = fips6.substring( 0, split ).toInt
    val countyFips = fips6.substring( split ).toInt

    db.find( Mobj( "StateFIPS" -> stateFips, "CountyFIPS" -> countyFips ), Mobj( "ZipCode" -> 1, "Latitude" -> 1, "Longitude" -> 1 ) ).toSeq
  }

  db.ensureIndex( Mobj( "StateFIPS" -> 1, "CountyFIPS" -> 1 ) )
  db.ensureIndex( Mobj( "ZipCode" -> 1 ) )
}

