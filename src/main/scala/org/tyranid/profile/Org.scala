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

package org.tyranid.profile

import org.bson.types.ObjectId
import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.db.{ DbChar, DbLowerChar, DbUrl }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity, MongoRecord }
import org.tyranid.image.DbThumbnail


class OrgMeta extends MongoEntity( "a02v" ) {
  "_id"                is DbMongoId               is 'id;
  "name"               is DbChar(120)             is 'label;
  "domain"             is DbLowerChar(128)        ;
  "thumbnail"          is DbThumbnail( "public" ) as "Logo";
  "website"            is DbUrl                   ;

  override def apply( obj:DBObject ):Org = throw new UnsupportedOperationException()
}

trait Org extends MongoRecord {
  def name:String
}


