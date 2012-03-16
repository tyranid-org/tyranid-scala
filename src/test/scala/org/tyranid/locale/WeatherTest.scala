/**
 * Copyright (c) 2008-2011 Tyranid <http://tyranid.org>
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

import org.scalatest.FunSuite

import org.tyranid.Imp._


class WeatherSuite extends FunSuite {
  org.tyranid.boot.Boot.boot

  test( "parsing" ) {

    val entryXml =
      <entry>
       <id>http://alerts.weather.gov/cap/wwacapget.php?x=AK124CA02D0EAC.WinterWeatherAdvisory.124CA02E66D0AK.AFCWSWAER.b760c0302c81f7c80ead2873b0cba35a</id>
       <updated>2012-03-14T04:19:00-08:00</updated>
       <published>2012-03-14T04:19:00-08:00</published>
       <author>
        <name>w-nws.webmaster@noaa.gov</name>
       </author>
       <title>Winter Weather Advisory issued March 14 at 4:19AM AKDT until March 14 at 1:00PM AKDT by NWS</title>
       <link href='http://alerts.weather.gov/cap/wwacapget.php?x=AK124CA02D0EAC.WinterWeatherAdvisory.124CA02E66D0AK.AFCWSWAER.b760c0302c81f7c80ead2873b0cba35a'/>
       <summary>...WINTER WEATHER ADVISORY FOR BLOWING SNOW IN EFFECT UNTIL 1 PM AKDT THIS AFTERNOON FROM WHITTER TO EASTERN TURNAGAIN ARM... THE NATIONAL WEATHER SERVICE IN ANCHORAGE HAS ISSUED A WINTER WEATHER ADVISORY FOR BLOWING SNOW...WHICH IS IN EFFECT UNTIL 1 PM AKDT THIS AFTERNOON. * LOCATION...WHITTIER TO EASTERN TURNAGAIN ARM.</summary>
       <cap:event>Winter Weather Advisory</cap:event>
       <cap:effective>2012-03-14T04:19:00-08:00</cap:effective>
       <cap:expires>2012-03-14T13:00:00-08:00</cap:expires>
       <cap:status>Actual</cap:status>
       <cap:msgType>Alert</cap:msgType>
       <cap:category>Met</cap:category>
       <cap:urgency>Expected</cap:urgency>
       <cap:severity>Minor</cap:severity>
       <cap:certainty>Likely</cap:certainty>
       <cap:areaDesc>Western Prince William Sound</cap:areaDesc>
       <cap:geocode>
        <valueName>FIPS6</valueName>
        <value>002020 002122 002261</value>
        <valueName>UGC</valueName>
        <value>AKZ125</value>
       </cap:geocode>
       <cap:parameter>
        <valueName>VTEC</valueName>
        <value>/X.NEW.PAFC.WW.Y.0027.120314T1219Z-120314T2100Z/</value>
       </cap:parameter>
      </entry>

    val entry = Cap.parse( entryXml )

    assert( entry.s( 'id )       === "AK124CA02D0EAC.WinterWeatherAdvisory.124CA02E66D0AK.AFCWSWAER.b760c0302c81f7c80ead2873b0cba35a" )
    assert( entry.t( 'updated )  === "2012.3.13 8:19pm UTC".parseDate() )
    assert( entry.s( 'areaDesc ) === "Western Prince William Sound" )
    assert( entry.s( 'sev )      === "Minor" )

    //spam( "http://alerts.weather.gov/cap/us.php?x=0".GET().toXml )

  }
}

