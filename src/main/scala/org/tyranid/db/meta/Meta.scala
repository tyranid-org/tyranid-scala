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

import com.mongodb.BasicDBList

import org.tyranid.Imp._
import org.tyranid.db.{ Entity, Record, ViewAttribute }
import org.tyranid.db.{ AttributeAnnotation, Domain, DbArray, DbLink, Entity, Record, ViewAttribute }
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity, MongoView, MongoRecord }
import org.tyranid.db.mongo.Imp._
import org.tyranid.profile.User



/*
 * * *   UiMap
 */

case class UiMapping( url:String, paths:List[String]* ) {

  private def matchOne( path:List[ViewAttribute], tpath:List[String] ) = {
    //( 1 until tpath.size ).foreach( i => spam( "i:" + i + " p:" + path( i-1 ).name + " tp:" + tpath( i ) ) )

    // start at 1 to skip past "org" ... probably needs to be done better?
    !( 1 until tpath.size ).exists( i => tpath( i ) != path( i-1 ).name )
  }

  def matches( path:List[ViewAttribute] ) = paths.exists( tpath => matchOne( path, tpath ) )
}

object UiMap {

  private var mappings:List[UiMapping] = Nil

  def +=( mapping:UiMapping ) = {
    mappings ::= mapping
    this
  }

  def findMatch( path:List[ViewAttribute] ) = mappings.find( _.matches( path ) )
}


/*
 * * *   Completion
 */

trait From extends AttributeAnnotation {
  val weight:Double

  def matches( user:User ):Boolean
}

case class Completion( total:Double, completed:Double, paths:List[ List[ViewAttribute] ] )

object Completion {

  def apply( rec:Record, user:User ):Completion = {
    var total = 0d;
    var completed = 0d;
    var paths:List[ List[ViewAttribute] ] = Nil

    def record( path:List[ViewAttribute], rec:Record ) {
      for ( va <- rec.view.vas ) {
        va.att.domain match {
        case en:Entity   => record( va :: path, rec.rec( va ) )
        case arr:DbArray => array( va :: path, rec.a( va ) )
        case dom         => simple( va :: path, rec( va ) )
        }
      }
    }

    def array( path:List[ViewAttribute], arr:BasicDBList ) {
      if ( arr == null || arr.size == 0 ) {
        Nil
      } else {
        val va = path.head
        val dom = va.att.domain.asInstanceOf[DbArray].of
        val value = arr( 0 )

        dom match {
        case en:MongoEntity => record( va :: path, en.recify( arr( 0 ), rec => arr( 0 ) = rec ) )
        case arr:DbArray    => array( va :: path, value.asInstanceOf[BasicDBList] )
        case dom            => simple( va :: path, value )
        }
      }
    }

    def simple( path:List[ViewAttribute], value:Any ) {
      val va = path.head
      val a  = va.att

      for ( from <- a.annotated[From];
            d = a.domain;
            ri <- path;
            rfrom <- ri.att.annotated[From];
            if rfrom.matches( user ) ) {
        total += from.weight
        d match {
        case link:DbLink =>
          val key = value
          // TODO:  what?

          if ( d.isSet( value ) )
            completed += from.weight
          else
            paths ::= path

        case dom =>
          if ( d.isSet( value ) )
            completed += from.weight
          else
            paths ::= path
        }
      }
    }

    record( Nil, rec )
    Completion( total, completed, paths )
  }
}

