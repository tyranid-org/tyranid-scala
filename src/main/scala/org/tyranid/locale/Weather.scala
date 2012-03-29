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

import scala.xml.Node

import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.db.{ DbArray, DbChar, DbDateTime, DbDouble, DbInt, DbLink, Record }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ MongoEntity, MongoRecord }
import org.tyranid.time.Time


object Weather {


}

/*
 * CAP = Common Alerting Protocol
 * 
 * https://wiki.citizen.apps.gov/nws_developers/index.php/Category:Common_Alerting_Protocol
 *
 * VTEC = Valid Time Event Code     ... http://www.nws.noaa.gov/om/vtec/
 * UGF  = Universal Geographic Code ... http://www.nws.noaa.gov/emwin/winugc.htm
 */
object Cap extends MongoEntity( tid = "a0Et" ) {
  "id"          is DbChar(128)   is 'key;

  "set"         is DbInt         ; // a random number generator so we can keep track of which fields were updated

  "updated"     is DbDateTime    ;
  "pub"         is DbDateTime    as "Published";
  "title"       is DbChar(256)   ;
  "summary"     is DbChar(512)   ;
  "event"       is DbChar(64)    ; // one of http://alerts.weather.gov/cap/product_list.txt
  "eff"         is DbDateTime    as "Effective";
  "exp"         is DbDateTime    as "Expiration";

  "status"      is DbChar(64)    as "Status";
  "msgType"     is DbChar(64)    as "Message Type";
  "cat"         is DbChar(64)    as "Category";

  "urg"         is DbChar(64)    as "Urgency";
  "sev"         is DbChar(64)    as "Severity";
  "cer"         is DbChar(64)    as "Certainty";

  "cer"         is DbChar(64)    as "Certainty";

  "areaDesc"    is DbChar(512)   ;

  "zips"        is DbArray(DbChar(16));

  "longLat"     is DbArray(DbDouble);
  "radius"      is DbDouble;

  override def apply( obj:DBObject ):Cap =
    if ( obj != null ) new Cap( obj ) else null    

  def idFromEntry( entryXml:Node ) = {
    val id = ( entryXml \ "id" ).text
    id.substring( id.indexOf( "x=" ) + 2 )
  }

  def parse( o:DBObject, entry:Node ) = {

    o( '_id )      = idFromEntry( entry )
    o( 'updated )  = ( entry \ "updated" ).text.parseDate()
    o( 'pub )      = ( entry \ "published" ).text.parseDate()
    o( 'title )    = ( entry \ "title" ).text
    o( 'summary )  = ( entry \ "summary" ).text
    o( 'event )    = ( entry \ "event" ).text.toLowerCase
    o( 'eff )      = ( entry \ "effective" ).text.parseDate()
    o( 'exp )      = ( entry \ "expires" ).text.parseDate()

    o( 'status )   = ( entry \ "status" ).text.toLowerCase
    o( 'msgType )  = ( entry \ "msgType" ).text.toLowerCase
    o( 'cat )      = ( entry \ "category" ).text.toLowerCase

    o( 'urg )      = ( entry \ "urgency" ).text.toLowerCase
    o( 'sev )      = ( entry \ "severity" ).text.toLowerCase
    o( 'cer )      = ( entry \ "certainty" ).text.toLowerCase

    o( 'areaDesc ) = ( entry \ "areaDesc" ).text

    {
      var fips6 = ""
      var ugc = ""
      var name = ""

      for ( ns <- entry \ "geocode"; n <- ns; el <- n.child ) {
        el.label match {
        case "valueName" =>
          name = el.text

        case "value" =>
          name match {
          case "FIPS6" => fips6 = el.text
          case "UGC"   => ugc = el.text
          }

        case "#PCDATA" =>
        }
      }

      val zipdata = fips6.split( " " ).flatMap( ZipCode.forFips6 )

      o( 'zips ) = Mlist( zipdata.map( _.i( 'ZipCode ) ).distinct:_* )

      val longs = zipdata.map( _.d( 'Longitude ) )
      val avgLong = longs.sum / longs.size

      val lats = zipdata.map( _.d( 'Latitude ) )
      val avgLat = lats.sum / lats.size
      o( 'longLat ) = Mlist( avgLong, avgLat )

      o( 'radius ) = ( longs.size > 0 |* ( longs.max - longs.min ) ) + ( lats.size > 0 |* ( lats.max - lats.min ) )
    }
  }

  def load {
    var str:String = null
    try {
      def setId = scala.util.Random.nextInt

      str = "http://alerts.weather.gov/cap/us.php?x=0".GET()
      for ( entryXml <- str.toXml \ "entry" ) {
  
        var entry = db.findOne( Mobj( "_id" -> idFromEntry( entryXml ) ) )
  
        if ( entry == null )
          entry = Mobj()
  
        parse( entry, entryXml )
        entry( 'set ) = setId
        db.save( entry )
      }

      db.remove( Mobj( "set" -> Mobj( $ne -> setId ) ) )
    } catch {
    case e:org.xml.sax.SAXParseException =>
      log( Event.Noaa, "m" -> str, "ex" -> e )
    case e:org.apache.http.conn.HttpHostConnectException =>
      println( "Cannot load weather: " + e.getMessage )
    case e:java.net.ConnectException =>
      println( "Cannot load weather: " + e.getMessage )
    case e:java.net.UnknownHostException =>
      println( "Cannot load weather: " + e.getMessage )
    case unknown =>
      throw unknown
    }
  }

  def weightFor( zips:Seq[String] ) = {

    // TODO:  add in date querying  (maybe if it is close to expiring or recently-expired it goes down in weight ?)

    val rslt = db.find( Mobj( "zips" -> Mobj( $in -> Mlist( zips:_* ) ) ) ).toSeq
    
    rslt.map( cap => Cap( cap ).weight ).foldLeft( 0 )( _ max _ )
  }

  def toJson = {
    val sb = new StringBuilder

    sb += '['

    var first = true

    for ( capo <- Cap.db.find( Mobj() );
          longLat = capo.a_?( 'longLat ) if longLat.size > 0 ) {
      val cap = Cap( capo )

      if ( first )
        first = false
      else
        sb += ','

      sb ++= "{t:" ++= longLat( 1 ).toString ++= ",n:" ++= longLat( 0 ).toString ++= ",r:" ++= cap.s( 'radius ) ++= ",w:" ++= cap.weight.toString += '}'
    }

    sb += ']'
    sb.toString
  }
}

class Cap( override val obj:DBObject = Mobj() ) extends MongoRecord( Cap.makeView, obj ) {

  def sevWeight =
    s( 'sev ) match {
    case "extreme"   => 100
    case "severe"    => 75
    case "moderate"  => 50
    case "minor"     => 25
    case "unknown"   => 50
    case _           => 50
    }

  def urgWeight =
    s( 'urg ) match {
    case "immediate" => 100
    case "expected"  => 75
    case "future"    => 50
    case "past"      => 25
    case "unknown"   => 50
    case _           => 50
    }

  def cerWeight =
    s( 'cer ) match {
    case "observed"  => 100
    case "likely"    => 75
    case "possible"  => 50
    case "unlikely"  => 25
    case "unknown"   => 50
    case _           => 50
    }

  def weight = ( sevWeight + urgWeight + cerWeight ) / 3

  /*

     translate zipcodes -> lat longs, average lat longs to get a coordinate, generate a circle


   */
}


/*
 <entry>
  <cap:geocode>
   <valueName>UGC</valueName>
   <value>AKZ125</value>
  </cap:geocode>
  <cap:parameter>
   <valueName>VTEC</valueName>
   <value>/X.NEW.PAFC.WW.Y.0027.120314T1219Z-120314T2100Z/</value>
  </cap:parameter>
 </entry>
 */
