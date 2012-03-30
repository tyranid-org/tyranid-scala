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

import org.tyranid.Imp._
import org.tyranid.db.{ DbArray, DbChar, DbDouble }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.MongoEntity


case class LatLong( lat:Double, long:Double )

object GeoCode extends MongoEntity( tid = "a0Lt" ) {
  "id"    is DbChar(256)        is 'key as "Canonical Address";
  "c"     is DbArray(DbDouble)  as "Coordinates";


  def canonical( address:String ) = address.toLowerCase.replace( "  ", " " )

  def byZip( zipCode:Int ) = ZipCode.latLongFor( zipCode )

  def byRawAddress( rawAddress:String ) = byCanonicalAddress( canonical( rawAddress ) )

  def byCanonicalAddress( canonicalAddress:String ) = {

    val obj = db.findOne( Mobj( "_id" -> canonicalAddress ) )

    if ( obj != null ) {
      val coordinates = obj.a( 'c )
      LatLong( lat = coordinates( 1 ).coerceDouble, long = coordinates( 0 ).coerceDouble )

    } else {
      val latLong = B.google.geocode( canonicalAddress )

      if ( latLong != null )
        db.save( Mobj( "_id" -> canonicalAddress, "c" -> Mlist( latLong.long, latLong.lat ) ) )

      latLong
    }
  }
}


