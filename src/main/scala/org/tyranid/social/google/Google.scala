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

package org.tyranid.social.google

import scala.xml.Unparsed

import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.db.mongo.Imp._
import org.tyranid.http.Http
import org.tyranid.locale.{ Country, LatLong }
import org.tyranid.math.Base64
import org.tyranid.session.Session


case class GoApp( simpleKey:String ) { // extends SoApp {

  val networkCode = "go"
  val networkName = "Google+"

  //val logo = "/images/facebook_logo.png"

/*
  def copyAttributes( from:User, to:User ) = {
    to( 'fbid ) = from.s( 'fbid )
    to( 'fbt )  = from.s( 'fbt )
    to( 'fbte ) = from.l( 'fbte )
  }

  def saveAttributes( user:User ) {
    B.User.db.update(
      Mobj( "_id" -> user.id ),
      Mobj( $set -> Mobj(
        "fbid" -> user.s( 'fbid ),
        "fbt"  -> user.s( 'fbt ),
        "fbte" -> user.l( 'fbte ) )
      )
    )
  }

  def removeAttributes( user:DBObject ) {
    user.remove( 'fbid )
    user.remove( 'fbt )
    user.remove( 'fbte )
    B.User.db.update( Mobj( "_id" -> user.id ), Mobj( $unset -> Mobj( "fbid" -> 1, "fbt" -> 1, "fbte" -> 1 ) ) )
  }
*/


  def geocode( address:String ):LatLong = {

    val str = "https://maps.googleapis.com/maps/api/geocode/json".GET( Map( "address" -> address, "sensor" -> "false" ) ).s
    val obj = str.parseJsonObject

    if ( obj.s( 'status ) == "OK" ) {
      val rslts = obj.a_?( 'results )
      if ( rslts.size > 0 ) {
        val loc = rslts( 0 ).as[ObjectMap].o_?( 'geometry ).o_?( 'location )
        return LatLong( lat = loc.d( 'lat ), long = loc.d( 'lng ) )
      }
    }

    log( Event.Google, "m" -> ( "geocode problem:\n\nAddress:" + address + "\n\nResponse:\n\n" + str ) )
    null
  }
}

