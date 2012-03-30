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
import org.tyranid.db.{ DbArray, DbChar, DbDouble, DbLink }
import org.tyranid.db.es.Search
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ MongoEntity, MongoRecord }


object Address extends MongoEntity( tid = "a0Kt" ) {
  "street1"    is DbChar(100)        is 'required is Search();
  "street2"    is DbChar(100)        is Search();
  "city"       is DbChar(30)         is 'required is Search();
  // state also means "province"
  "state"      is DbLink(Region)     as "State/Region";
  "postalCode" is DbChar(10)         as "Zip/Postal Code" is 'required is Search();
  "country"    is DbLink(Country)    is 'required;

  "longLat"    is DbArray(DbDouble);

  override def apply( obj:DBObject ) = new Address( obj )
}

class Address( override val obj:DBObject = Mobj() ) extends MongoRecord( Address.makeView, obj ) {

  def full = {
    val sb = new StringBuilder
    sb ++= ( s( 'street1 ) != null |* s( 'street1 ) )
    sb ++= ( s( 'city ) != null |* ( ( sb.size > 0 |* " " ) + s( 'city ) ) )
    sb ++= ( s( 'state ).notBlank |* ( ( s( 'city ) |* ", " ) + Region.codeForId( i( 'state ) ) ) )
    sb ++= ( s( 'postalCode ) != null |* ( ( s( 'state ) |* " " ) + s( 'postalCode ) ) )
    sb ++= ( i( 'country ) > 0 |* " " + Country.nameForId( i( 'country ) ) )
    sb.toString
  }

  def cityState = {
    val sb = new StringBuilder
    sb ++= ( s( 'city ).notBlank |* ( ( sb.size > 0 |* " " ) + s( 'city ) ) )
    sb ++= ( s( 'state ).notBlank |* ( ( s( 'city ) |* ", " ) + Region.codeForId( i( 'state ) ) ) )
    sb ++= ( i( 'country ) > 0 |* " " + Country.nameForId( i( 'country ) ) )
    sb.toString
  }

  def latLong:LatLong = {
    val longLat = a( 'longLat )

    if ( longLat == null ) {
      val latLong = GeoCode.byRawAddress( full )

      if ( latLong != null )
        this( 'longLat ) = Mlist( latLong.long, latLong.lat )
      
      latLong
    } else {
      LatLong( longLat( 1 ).coerceDouble, longLat( 0 ).coerceDouble )
    }
  }

  def regeocode {
    this( 'longLat ) = null
    latLong
  }
}
