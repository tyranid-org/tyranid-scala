/**
 * Copyright (c) 2008-2014 Tyranid <http://tyranid.org>
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

package org.tyranid.db

import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ MongoEntity, MongoRecord }


object AdHoc extends MongoEntity( tid = "a0Qt" ) {
  type RecType = AdHoc
  override def convert( obj:DBObject, parent:MongoRecord ) = new AdHoc( obj, parent )

  override lazy val db = problem( "AdHoc entities do not have a database connection." )
}

class AdHoc( obj:DBObject, parent:MongoRecord ) extends MongoRecord( AdHoc.makeView, obj, parent )

