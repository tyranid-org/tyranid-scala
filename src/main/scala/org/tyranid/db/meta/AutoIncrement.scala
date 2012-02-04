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

package org.tyranid.db.meta

import java.util.Date

import com.mongodb.{ BasicDBList, DBObject }

import org.tyranid.Imp._
import org.tyranid.db.{ Entity, Record, Path, PathValue, PathDiff, MultiPath, ViewAttribute }
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity, MongoView, MongoRecord }
import org.tyranid.db.mongo.Imp._
import org.tyranid.profile.User
import org.tyranid.session.Session


object AutoIncrement {

  private lazy val db = {
    val db = Mongo.connect.db( Tyr.profileDbName )( "auto_increment" )
    if ( db.findOne() == null )
      db.save( Mobj( "_id" -> 1 ) )
    db
  }

  def apply( name:String ):Int =
    db.findAndModify( Mobj( "_id" -> 1 ).obj,
                      null.asInstanceOf[DBObject],
                      null.asInstanceOf[DBObject],
                      false,
                      Mobj( $inc -> Mobj( name -> 1 ) ).obj,
                      true,
                      true ).i( name )
}

