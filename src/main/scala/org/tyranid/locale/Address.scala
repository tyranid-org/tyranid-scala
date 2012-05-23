/**
 * Copyright (c) 2008-2012 Tyranid (   http://tyranid.org>
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
  type RecType = Address
  override def convert( obj:DBObject, parent:MongoRecord ) = new Address( obj, parent )

  "street1"    is DbChar(100)        is 'required is Search();
  "street2"    is DbChar(100)        is Search();
  "city"       is DbChar(30)         is 'required is Search();
  // state also means "province"
  "state"      is DbLink(Region)     as "State/Region";
  "postalCode" is DbChar(10)         as "Zip/Postal Code" is 'required is Search();
  "country"    is DbLink(Country)    is 'required;

  "longLat"    is DbArray(DbDouble);
}

class Address( obj:DBObject, parent:MongoRecord ) extends MongoRecord( Address.makeView, obj, parent ) {

  def full = {
    val sb = new StringBuilder
    sb ++= ( s( 'street1 ) != null |* s( 'street1 ) )
    sb ++= ( s( 'city ) != null |* ( ( sb.size > 0 |* " " ) + s( 'city ) ) )
    sb ++= ( ( s( 'street1 ) != null || s( 'city ) != null ) |* ", " ) //If either the street1 or city exist, throw in the comma and space
    sb ++= ( s( 'state ).notBlank |* Region.codeForId( i( 'state ) ) ) 
    sb ++= ( s( 'postalCode ) != null |* ( ( s( 'state ) |* " " ) + s( 'postalCode ) ) )
    sb ++= ( i( 'country ) > 0 |* " " + Country.nameForId( i( 'country ) ) )
    sb.toString
  }

  def cityState = {
    val sb = new StringBuilder
    sb ++= ( s( 'city ).notBlank |* ( ( sb.size > 0 |* " " ) + s( 'city ) ) )
    sb ++= ( ( s( 'city ).notBlank && s( 'state ).notBlank ) |* ", ")  //Only put in the comma if both city and state exist
    sb ++= ( s( 'state ).notBlank |* Region.codeForId( i( 'state ) ) ) 
    sb ++= ( i( 'country ) > 0 |* " " + Country.nameForId( i( 'country ) ) )
    sb.toString
  }

  def cityStatePostal = {
    val sb = new StringBuilder
    sb ++= ( s( 'city ).notBlank |* ( ( sb.size > 0 |* " " ) + s( 'city ) ) + ", " )
    sb ++= ( s( 'state ).notBlank |* Region.codeForId( i( 'state ) ) ) 
    sb += ' ' ++= s( 'postalCode )
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
      LatLong( longLat( 1 )._d, longLat( 0 )._d )
    }
  }

  def regeocode {
    this( 'longLat ) = null
    latLong
  }
}

