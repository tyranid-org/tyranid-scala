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

package org.tyranid.db

import org.scalatest.FunSuite

import org.tyranid.Imp._
import org.tyranid.db.mongo.Imp._
import org.tyranid.test.db._


class PathSuite extends FunSuite {

  test( "flatten" ) {

    val obj = Widget.make

    obj( 'name ) = "test"
    obj( 'dims ) = Mobj( "height" -> 20, "weight" -> 31 )
    obj( 'tags ) = Mlist( "acme", "fun" )

    val pvs = Path.flatten( obj ).sorted

    assert( pvs.size === 5 )
    
    assert( pvs( 4 ).path.name === "tags.1" )
    assert( pvs( 4 ).toString === "tags.1=fun" )

    assert( pvs( 0 ).value === 20 )
    assert( pvs( 1 ).value === 31 )
    assert( pvs( 2 ).value === "test" )
    assert( pvs( 3 ).value === "acme" )
    assert( pvs( 4 ).value === "fun" )
  }

  test( "diff" ) {

    val a = Widget.make
    a( 'name ) = "test"
    a( 'dims ) = Mobj( "height" -> 20, "weight" -> 31 )
    a( 'tags ) = Mlist( "acme", "fun" )

    val b = Widget.make
    b( 'name ) = "test"
    b( 'dims ) = Mobj( "height" -> 21, "weight" -> 31 )
    b( 'tags ) = Mlist( "acme", "fun" )

    var diff = Path.diff( a, b )
    assert( diff.as.size === 0 )
    assert( diff.bs.size === 0 )
    assert( diff.diffs.size === 1 )
    assert( diff.diffs( 0 ).path.name === "dims.height" )
    assert( diff.diffs( 0 ).b === 21 )

    a( 'level ) = 2
    a.o( 'dims )( 'height ) = 21

    diff = Path.diff( a, b )
    assert( diff.as.size === 1 )
    assert( diff.bs.size === 0 )
    assert( diff.diffs.size === 0 )
    assert( diff.as( 0 ).path.name === "level" )
    assert( diff.as( 0 ).value === 2 )

    a.remove( 'level )
    b( 'level ) = 2

    diff = Path.diff( a, b )
    assert( diff.as.size === 0 )
    assert( diff.bs.size === 1 )
    assert( diff.diffs.size === 0 )
    assert( diff.bs( 0 ).path.name === "level" )
    assert( diff.bs( 0 ).value === 2 )
  }

  test( "pathNames" ) {
    val view = Widget.makeView
    val paths = Array(
      "categories",
      "categories_1",
      "categories_1_name",
      "dims_height",
      "prices_0_price",
      "prices_0_type_name",
      "prices_0_type_quantity"
    )

    for ( path <- paths )
      assert( view.path( path ).pathName === path )
  }

  test( "dbobject" ) {
    val view = Widget.makeView
    val data = Array(
      ( "name",        "test" ),
      ( "dims_height", 20 ),
      ( "tags_0",      "acme" ),
      ( "tags_1",      "fun" )
    )

    val pvs = (
      for ( d <- data ) yield
        PathValue( Path.parse( view, d._1 ), d._2 ) ).sorted

    val pvs2 = PathValue.fromDbObject( view, PathValue.toDbObject( pvs ) ).toSeq.sorted.toArray

    assert( pvs === pvs2 )
  }

  test( "pathGet" ) {

    val obj = Widget.make
    val v = obj.view

    obj( 'name ) = "test"
    obj( 'dims ) = Mobj( "height" -> 20, "weight" -> 31 )
    obj( 'tags ) = Mlist( "acme", "fun" )

    assert( Path.parse( v, "name"        ).get( obj ) == "test" )
    assert( Path.parse( v, "dims.height" ).get( obj ) == 20 )
    assert( Path.parse( v, "tags.0"      ).get( obj ) == "acme" )
  }
}

