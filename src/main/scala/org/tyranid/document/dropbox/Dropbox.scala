/**
 * Copyright (c) 2008-2013 Tyranid <http://tyranid.org>
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

package org.tyranid.document.dropbox

import scala.collection.mutable
import scala.collection.mutable.Buffer

import java.io.{ File, FileInputStream }

import com.mongodb.{ DBObject, DBCollection }

import org.tyranid.Imp._
import org.tyranid.cloud.aws.{ S3, S3Bucket }
import org.tyranid.db.mongo.Imp._
import org.tyranid.json.Json
import org.tyranid.http.Http
import org.tyranid.log.Event
import org.tyranid.time.Time

object Dropbox {
}

case class DropboxApp( apiKey:String ) {
  // Need to copy these over because they are not solely owned by us:
  // See: https://forums.aws.amazon.com/message.jspa?messageID=371475
}

