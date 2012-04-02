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
import org.tyranid.db.{ DbArray, DbLink, DbTextLike, Entity, Record, MultiPath, Path, PathNode, Tid, ViewAttribute }
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity, MongoView, MongoRecord }
import org.tyranid.db.mongo.Imp._
import org.tyranid.profile.User
import org.tyranid.session.Session


object Ref {

  def to( tid:String, in:Entity = null ) {
    val ( entityTid, recordTid ) = Tid.split( tid )

    val refEn = Entity.byTid( entityTid ).get
spam( "recordTid=" + recordTid )
spam( "size=" +  org.tyranid.math.Base64.toBytes( recordTid ).size )
    val refId = refEn.recordTidToId( recordTid )

    var query:DBObject = null

    def enter( path:List[PathNode], v:Any ) {

 spam( "enter.v=" + v.toString )
      v match {
      case link:DbLink =>
        if ( link.toEntity == refEn ) {
          val p = MultiPath( path:_* )

          if ( path.head.as[ViewAttribute].domain == DbArray )
            query( p.name ) = refId
          else
            query( p.name ) = Mobj( $in -> refId )
        }

      case array:DbArray =>
        enter( path, array.of )

      case en:Entity =>
        val v = en.makeView
        for ( va <- v.vas )
          enter( va :: path, va.domain )

      case s:DbTextLike => // if it is a tid ?
      case _ =>
      }
    }

    if ( in != null ) {
      query = Mobj()
      enter( Nil, in )
    } else {
      for ( en <- Entity.all ) {
        query = Mobj()
        enter( Nil, en )
      }
    }

    spam( "query=" + query )

    /*

        need to find raw ids ... DbLinks

        also need to find tid references, like in Volees ...



     */


  }



}

