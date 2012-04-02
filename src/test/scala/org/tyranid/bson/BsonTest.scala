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

package org.tyranid.bson

import org.scalatest.FunSuite

import org.tyranid.Imp._
import org.tyranid.db.mongo.Imp._


class BsonSuite extends FunSuite {

  test( "basic" ) {
    val obj = Mlist()

    obj.rollRight( 3, 3 )
    obj.rollRight( 2, 3 )
    obj.rollRight( 1, 3 )

    assert( obj === Mlist( 1, 2, 3 ) )

    obj.truncate( 2 )
    assert( obj === Mlist( 1, 2 ) )

    obj.rollRight( 3, 3 )
    assert( obj === Mlist( 3, 1, 2 ) )

    obj.rollRight( 3, 3 )
    assert( obj === Mlist( 3, 3, 1 ) )

    obj.rollRight( 9, 5 )
    assert( obj === Mlist( 9, 3, 3, 1 ) )

    obj.rollRight( 8, 5 )
    assert( obj === Mlist( 8, 9, 3, 3, 1 ) )

    obj.rollRight( 7, 5 )
    assert( obj === Mlist( 7, 8, 9, 3, 3 ) )
  }
}

