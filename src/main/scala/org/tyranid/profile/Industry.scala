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

package org.tyranid.profile

import scala.collection.mutable

import org.bson.types.ObjectId
import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.db.{ DbInt, DbLong, DbChar }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ MongoEntity }
import org.tyranid.db.meta.AutoIncrement


object Industry extends MongoEntity( tid = "a0Ot" ) {
	override lazy val dbName = "businessCategories"

  "sectorId"        is DbInt               ;
  "sectorName"      is DbChar(30)          ;
  "industryGroupId" is DbInt               ;
  "industryGroup"   is DbChar(30)          ;
  "industryId"      is DbInt               ;
  "industry"        is DbChar(40)          ;
  "subIndustryId"   is DbLong              ;//is Req( UserRole.Admin ); // subIndustryId
//  "id"              is DbLong is 'key      is Req( UserRole.Admin ); // subIndustryId
  "subIndustry"     is DbChar(50) is 'label;
  "description"     is DbChar(400)         ;

  def cache = mutable.HashMap[ObjectId,DBObject]()

  def get( id:ObjectId ) = cache.getOrElseUpdate( id, Industry.db.findOne( id ) )
}

