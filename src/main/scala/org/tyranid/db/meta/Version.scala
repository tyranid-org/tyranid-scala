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

import com.mongodb.BasicDBList

import org.tyranid.Bind
import org.tyranid.Imp._
import org.tyranid.db.{ Entity, Record, Path, PathValue, PathDiff, MultiPath, ViewAttribute }
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity, MongoView, MongoRecord }
import org.tyranid.db.mongo.Imp._
import org.tyranid.profile.User
import org.tyranid.session.Session


trait Versioning extends Entity {

  abstract override def save( r:Record ) {
    super.save( r )

    val original = r.original

    if ( original != null ) {
      val diffs = Path.diff( original, r )
spam("2")

      if ( diffs.nonEmpty ) {
        val log = Mobj()

        log( 'user ) = Session().user.id
spam("2.1")
        log( 'on ) = new Date
spam("r: " + r )
spam("o.id apply: " + original.apply( "_id" ) )
spam("r.id: " + r.id )
        log( 'recId ) = r.id
spam("2.3")

        if ( diffs.as.nonEmpty )
          log( 'removals ) = PathValue.toDbObject( diffs.as )
        
spam("3")
        if ( diffs.bs.nonEmpty )
          log( 'adds ) = PathValue.toDbObject( diffs.bs )
        
        if ( diffs.diffs.nonEmpty )
          log( 'updates ) = PathDiff.toDbObject( diffs.diffs )

spam("4")
        val db = Mongo.connect.db( Bind.ProfileDbName )( r.entity.dbName + "_log" )

spam("5")
        db.save( log )
spam("6")
      }
spam("10")
      
    }
  }
}

object Version {

}

