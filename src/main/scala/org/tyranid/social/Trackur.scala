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


case class TrackurApp( apiKey:String ) {

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


  }
}

