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
import org.tyranid.db.{ DbArray, DbInt, DbChar, DbDateTime, DbLink, Record }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.MongoEntity
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
    o( 'event )    = ( entry \ "event" ).text
    o( 'eff )      = ( entry \ "effective" ).text.parseDate()
    o( 'exp )      = ( entry \ "expires" ).text.parseDate()

    o( 'status )   = ( entry \ "status" ).text
    o( 'msgType )  = ( entry \ "msgType" ).text
    o( 'cat )      = ( entry \ "category" ).text

    o( 'urg )      = ( entry \ "urgency" ).text
    o( 'sev )      = ( entry \ "severity" ).text
    o( 'cer )      = ( entry \ "certainty" ).text

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

      o( 'zips ) = Mlist( fips6.split( " " ).flatMap( fips6 => ZipCode.forFips6( fips6 ) ).distinct:_* )
    }
  }

  def load = {

    for ( entryXml <- "http://alerts.weather.gov/cap/us.php?x=0".GET().toXml \ "entry" ) {

      var entry = db.findOne( Mobj( "_id" -> idFromEntry( entryXml ) ) )

      if ( entry == null )
        entry = Mobj()

      parse( entry, entryXml )
      db.save( entry )
    }
  }

  def start = background {
    while ( true ) {
      trylog {
        load
      }

      Thread.sleep( 2 * Time.OneMinuteMs )
    }
  }
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
