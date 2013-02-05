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

import scala.collection.mutable

import com.mongodb.BasicDBList

import org.tyranid.Imp._
import org.tyranid.db.{ Entity, Record, Path, PathValue, PathDiff, MultiPath, ViewAttribute }
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity, MongoView, MongoRecord }
import org.tyranid.db.mongo.Imp._
import org.tyranid.profile.User
import org.tyranid.session.Session
import org.tyranid.web.Weblet


trait Versioning extends Entity {

  abstract override def save( r:Record ) {
    super.save( r )

    val original = r.original

    if ( original != null ) {
      val diff = Path.diff( original, r )

      if ( diff.nonEmpty ) {
        val as    = diff.as   .filter( !_.path.leaf.att.noVersion )
        val bs    = diff.bs   .filter( !_.path.leaf.att.noVersion )
        val diffs = diff.diffs.filter( !_.path.leaf.att.noVersion )

        if ( as.nonEmpty || bs.nonEmpty || diffs.nonEmpty ) {
          val log = Mobj()

          log( 'user ) = Session().user.id
          log( 'on ) = new Date
          log( 'recId ) = r.id

          if ( as.nonEmpty )
            log( 'removals ) = PathValue.toDbObject( as )
        
          if ( bs.nonEmpty )
            log( 'adds ) = PathValue.toDbObject( bs )
        
          if ( diffs.nonEmpty )
            log( 'updates ) = PathDiff.toDbObject( diffs )

          logdb.save( log )
        }
      }
    }
  }

  abstract override def delete( r:Record ) {
    super.delete( r )
    logdb.remove( Mobj( "recId" -> r.id ) )
  }

  abstract override def init {
    super.init
    logdb.ensureIndex( Mobj( "on" -> -1 ) )
  }

  lazy val logdbName = dbName + "_log"

  lazy val logdb = {
    if ( embedded )
      problem( "embedded mongodb entities do not have db objects" )

    Mongo.connect.db( B.profileDbName )( logdbName  )
  }
}

object Versioning {

  case class Change( path:String, a:Any, b:Any )

  def ui( weblet:Weblet, tid:String ) = {
    val ( en, id ) = Tid.parse( tid )

    val objs = en.as[Versioning].logdb.find( Mobj( "recId" -> id ) ).sort( Mobj( "on" -> -1 ) )

    <table class="dtable nested">
     <thead><tr><th style="width:150px;">On</th><th>User</th><th>Attribute</th><th>Old</th><th style="width:200px;">New</th></tr></thead>{
       objs.map { ver =>
         val user = ver( 'user )

         val map = mutable.HashMap[String,Change]()

         val removals = ver.o( 'removals )
         if ( removals != null )
           for ( name <- removals.keys )
             map( name ) = Change( name, removals( name ), null )

         val updates = ver.o( 'updates )
         if ( updates != null )
           for ( name <- updates.keys ) {
             val u = updates.o( name )
             map( name ) = Change( name, u( 'a ), u( 'b ) )
           }

         val adds = ver.o( 'adds )
         if ( adds != null )
           for ( name <- adds.keys )
             map( name ) = Change( name, null, adds( name ) )

         val values = map.values.toSeq.sortBy( _.path )

         var first = true
         values.map { change =>
           <tr>
            { first |* <td rowspan={ values.size.toString }>{ ver.t( 'on ).toUserDateTimeStr }</td> }
            { first |* <td rowspan={ values.size.toString }><a href={ weblet.wpath + "?tid=" + B.User.idToTid( user ) }>{ B.User.labelFor( user ) }</a></td> }
            <td>{ first = false; change.path }</td>
            <td>{ change.a }</td>
            <td>{ change.b }</td>
           </tr>
         }
       }
    }</table>
  }
}

