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

package org.tyranid.db.meta

import com.mongodb.BasicDBList

import org.tyranid.db.{ Entity, Record, Path, MultiPath, ViewAttribute }
import org.tyranid.db.{ AttributeAnnotation, Domain, DbArray, DbLink, Entity, Record, ViewAttribute }
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity, MongoView, MongoRecord }
import org.tyranid.db.mongo.Imp._
import org.tyranid.profile.User
import org.tyranid.Imp._



/*
 * * *   UiMap
 */

case class UiMapping( url:String, paths:Seq[String]* ) {

  def matches( path:Path ) = {
    // start at 1 to skip past "org" ... probably needs to be done better?
    paths.exists( tpath => path.matches( tpath, 1 ) )
  }
}


/**
 * A UiMap maps fields in the database to URL-paths in the UI where that field can be edited.
 */
object UiMap {

  private var mappings:List[UiMapping] = Nil

  def +=( mapping:UiMapping ) = {
    mappings ::= mapping
    this
  }

  def findMatch( path:Path ) = mappings.find( _.matches( path ) )
}


/*
 * * *   Completion
 */

/**
 * This provides additional requirements for an Attribute.  For example, how important it is (weighting),
 * who is responsible for completing it, whether it is absolutely required, and so on.
 */
trait Req extends AttributeAnnotation {
  val weight:Double

  val tags:Seq[Int]

  def matches( user:User ):Boolean
}

case class Completion( total:Double, completed:Double, paths:List[Path] )

object Completion {

  def apply( rec:Record, user:User ):Completion = {
    var total = 0d;
    var completed = 0d;
    var paths:List[Path] = Nil

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
        case en:MongoEntity => record( va :: path, en.recify( arr( 0 ), parent = null /* TODO:  this is wrong, pass in parent record to array() ? */, rec => arr( 0 ) = rec ) )
        case arr:DbArray    => array( va :: path, value.asInstanceOf[BasicDBList] )
        case dom            => simple( va :: path, value )
        }
      }
    }

    def simple( path:List[ViewAttribute], value:Any ) {
      val va = path.head
      val a  = va.att

      for ( req <- a.annotated[Req];
            d = a.domain;
            ri <- path;
            rreq <- ri.att.annotated[Req];
            if rreq.matches( user ) ) {
        total += req.weight
        d match {
        case link:DbLink =>
          val key = value
          // TODO:  what?

          if ( d.isSet( value ) )
            completed += req.weight
          else
            paths ::= MultiPath( path.reverse:_* )

        case dom =>
          if ( d.isSet( value ) )
            completed += req.weight
          else
            paths ::= MultiPath( path.reverse:_* )
        }
      }
    }

    record( Nil, rec )
    Completion( total, completed, paths )
  }
}

