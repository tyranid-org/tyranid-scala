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

package org.tyranid.social

import org.tyranid.Imp._
import org.tyranid.db.{ DbArray, DbChar, DbInt }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity }


case class TrackurApp( apiKey:String, monitoredQueries: () => Seq[String] ) {

}

object Trackur extends MongoEntity( tid = "a0Jt" ) {

  /*

      source:  http://www.trackur.com/api-documentation

      /<query>/<page size>/<skip>/<comma-separated included words+phrases>/<comma-separated excluded words+phrases>/<comma-separated domain excludes>/<media type>/<start date>/<end date>

      <media type> is one of News, Blogs, Reddit, GooglePlus, Delicious, Twitter, Facebook, Media, and Forums

   */

  "id"          is DbMongoId      ;
  "query"       is DbChar(128)    ;

  "activity"    is DbArray(DbInt) as "Rolling Activity Counts";




  def monitor = {

    for ( query <- B.trackur.monitoredQueries().take( 3 ) ) {

      val obj = db.findOrMake( Mobj( "query" -> query ) )

      val rslts = ( "http://api.trackur.com/index.php/api/json/" + B.trackur.apiKey + query ).GET().parseJsonArray

      val numRslts = rslts( 0 ).asJsonObject.o_?( 'response ).i( 'totalresults )

      /*
[ { "response": {
      "link":"http://api.trackur.com/index.php/api/json/9a2c7879bf63799e3d2f71cc3b38a95e48f7/Dimensiology/1/1/////0/0",
      "error":"No results found."
    }
  }
]
[ { "response": {
      "link":"http://api.trackur.com/index.php/api/json/9a2c7879bf63799e3d2f71cc3b38a95e48f7/YRC/1/1/////0/0",
      "user":{"id":"40106","requesttime":"1332906355","requestsused":"110","requestlimit":"100000","hourlyuse":"1"},
      "request":{"keyword":"YRC","limit":"1","offset":"1","include":"","exclude":"","domains":"","sources":"","datestart":"0","dateend":"0"},
      "totalresults":"541",
      "results":[
         {"id":"aa567fca1fb8106d","title":"Trucking Sector Gains Momentum -- YRC Worldwide and Arkansas Best Looking Strong - MarketWatch (press release)","source":"Twitter","published":"2:57am Mon Mar 26 2012","url":"http://twitter.com/TruckingMotYbum/statuses/184172081968185344","content":"Trucking Sector Gains Momentum -- YRC Worldwide and Arkansas Best Looking Strong - MarketWatch (press release)"}
      ]
    },
    "navigation":{
      "previous":"http://api.trackur.com/index.php/api/json/9a2c7879bf63799e3d2f71cc3b38a95e48f7/YRC/1/0/////0/0",
      "next":"http://api.trackur.com/index.php/api/json/9a2c7879bf63799e3d2f71cc3b38a95e48f7/YRC/1/1/////0/0"
    }
  }
]
      */

      obj.a_!( 'activity ).rollRight( numRslts, 10 )

      db.save( obj )
    }
  }
}

