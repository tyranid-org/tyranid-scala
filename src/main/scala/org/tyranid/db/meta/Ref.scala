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

package org.tyranid.db.meta

import java.util.Date

import com.mongodb.{ BasicDBList, DBObject }

import org.tyranid.Imp._
import org.tyranid.db.{ DbArray, DbLink, DbTextLike, DbTid, Entity, Record, MultiPath, Path, PathNode, Tid, ViewAttribute }
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity, MongoView, MongoRecord }
import org.tyranid.db.mongo.Imp._
import org.tyranid.profile.User
import org.tyranid.session.Session


object Ref {

  def to( tid:String, in:Entity = null ) {
    val ( entityTid, recordTid ) = Tid.split( tid )

    val refEn = Entity.byTid( entityTid ).get
    val refId = refEn.recordTidToId( recordTid )

    var query:DBObject = null

    def enter( path:List[PathNode], v:Any ) {

      v match {
      case link:DbLink =>
        if ( link.toEntity == refEn ) {
          val p = MultiPath( path:_* )

          query( p.name ) = refId
        }

      case link:DbTid =>
        if ( link.of.contains( refEn ) ) {
          val p = MultiPath( path:_* )

          query( p.name ) = tid
        }

      case array:DbArray =>
        enter( path, array.of )

      case en:Entity =>
        val v = en.makeView
        for ( va <- v.vas )
          enter( va :: path, va.domain )

      case _ =>
      }
    }

    def entity( en:Entity ) {
      query = Mobj()
      enter( Nil, en )

      if ( en.is[Versioning] ) {
        // handle this:   log( 'user ) = Session().user.id
        //if ( diffs.as.nonEmpty )
          //log( 'removals ) = PathValue.toDbObject( diffs.as )
        
        //if ( diffs.bs.nonEmpty )
          //log( 'adds ) = PathValue.toDbObject( diffs.bs )
        
        //if ( diffs.diffs.nonEmpty )
          //log( 'updates ) = PathDiff.toDbObject( diffs.diffs )
      }
    }

    if ( in != null ) {
      entity( in )
    } else {
      for ( en <- Entity.all )
        entity( en )
    }

    val fullQuery =
      if ( query.keySet.size > 1 )
        Mobj( $or -> query )
      else
        query

    spam( "query=" + fullQuery )

  }



}

