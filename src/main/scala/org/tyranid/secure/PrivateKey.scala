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

package org.tyranid.secure

import scala.xml.Text

import org.tyranid.Imp._
import org.tyranid.db.DbChar
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ MongoEntity, MongoRecord }
import org.tyranid.math.Base62



trait PrivateKeyEntity extends MongoEntity {
  type RecType >: Null <: PrivateKeyRecord

  "pk"       is DbChar(10)        help Text( "A private-key, generated on-demand.  Used where a group URL needs to be hard-to-guess-yet-publicly-accessible.  For example, RSS Feeds." );



  def byPrivateId( privateId:String ) = {
    val split = privateId.length - 10
    val tid = privateId.substring( 0, split )
    val pk  = privateId.substring( split )

    byTid( tid ).filter( _.pk == pk )
  }
}

trait PrivateKeyRecord extends MongoRecord {



  // "private" key
  def pk = {
    var v = s( 'pk )
    if ( v.isBlank ) {
      v = Base62.make( 10 )
      update( 'pk, v )

      if ( !isNew )
        db.update( Mobj( "_id" -> id ), Mobj( $set -> Mobj( "pk" -> v ) ) )
    }

    v
  }

  def privateId = tid + pk

}

