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

package org.tyranid.db

import org.scalatest.FunSuite

import org.tyranid.Imp._
import org.tyranid.db.mongo.Imp._
import org.tyranid.test.db._


class RecordSuite extends FunSuite {

 test( "toClientJson" ) {

    val rec = Widget.make
    rec( 'name ) = "Widget A"
    rec( 'level ) = 5
    rec( 'prices ) = Mlist( Mobj( "aid" -> 3, "price" -> 1.0 ), Mobj( "price" -> 2.0 ) )

    rec.compute( client = true )

    assert( rec.toJsonStr( client = true ) === """{"level":5,"name":"Widget A","prices":[{"price":1.0},{"price":2.0}],"level2":10}""" )

    //assert( rec.view.path( "prices.0.price" ).aidName_( rec ) === "prices_3a_price" )

    // falls back to array index if aid isn't present
    //assert( rec.view.path( "prices.1.price" ).aidName_( rec ) === "prices_1_price" )
  }
}

