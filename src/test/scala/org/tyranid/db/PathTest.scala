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

  test( "arrayDiff" ) {

    val a = Widget.make
    a( 'name ) = "test"
    a( 'prices ) = Mlist( Mobj( "price" -> 1.0 ), Mobj( "price" -> 2.0 ) )

    val b = Widget.make
    b( 'name ) = "test"
    b( 'prices ) = Mlist( Mobj( "price" -> 3.0 ), Mobj( "price" -> 2.0 ) )

    var diff = Path.diff( a, b )

    assert( diff.as.size === 0 )
    assert( diff.bs.size === 0 )
    assert( diff.diffs.size === 1 )
    assert( diff.diffs( 0 ).a === 1 )
    assert( diff.diffs( 0 ).b === 3 )
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
      "prices_0_type_quantity",
      "prices_3a_type_quantity"
    )

    for ( path <- paths )
      assert( view.path( path ).pathName === path )
  }

  test( "pathNameSlash" ) {
    val view = Widget.makeView
    val paths = Array(
      "categories",
      "categories/1",
      "categories/1/name",
      "dims/height",
      "prices/0/price",
      "prices/0/type/name",
      "prices/0/type/quantity",
      "prices/3a/type/quantity"
    )

    for ( path <- paths )
      assert( view.path( path ).name_/ === path )
  }

  test( "dbobject" ) {
    val view = Widget.makeView
    val data = Array(
      ( "name",           "test" ),
      ( "dims_height",    20 ),
      ( "tags_0",         "acme" ),
      ( "tags_1",         "fun" ),
      ( "prices_0_price", 5 )
    )

    val pvs = (
      for ( d <- data ) yield
        PathValue( view.path( d._1 ), d._2 ) ).sorted

    val pvs2 = PathValue.fromDbObject( view, PathValue.toDbObject( pvs ) ).toSeq.sorted.toArray

    assert( pvs === pvs2 )
  }

  test( "pathAccessors" ) {

    val obj = Widget.make
    val v = obj.view

    obj( 'name )   = "test"
    obj( 'dims )   = Mobj( "height" -> 20, "weight" -> 31 )
    obj( 'tags )   = Mlist( "acme", "fun" )
    obj( 'prices ) = Mlist( Mobj( "aid" -> 3, "price" -> 1.0 ), Mobj( "price" -> 2.0 ) )

    assert( v.path( "name"            ).get( obj ) == "test" )
    assert( v.path( "dims.height"     ).get( obj ) == 20 )
    assert( v.path( "tags.0"          ).get( obj ) == "acme" )
    assert( v.path( "prices.1.price"  ).get( obj ) == 2.0 )
    assert( v.path( "prices.0.price"  ).get( obj ) == 1.0 )
    assert( v.path( "prices.3a.price" ).get( obj ) == 1.0 )

    v.path( "name" ).set( obj, "test2" )
    assert( v.path( "name" ).s( obj ) == "test2" )

    v.path( "name" ).remove( obj )
    assert( v.path( "name" ).get( obj ) == null )

    v.path( "tags.0" ).set( obj, "widget" )
    assert( v.path( "tags.0" ).get( obj ) == "widget" )

    v.path( "tags.0" ).remove( obj )
    assert( v.path( "tags.0" ).get( obj ) == null )

    v.path( "prices.3a.price" ).set( obj, 1.1 )
    assert( v.path( "prices.3a.price" ).get( obj ) == 1.1 )

    v.path( "prices.3a.price" ).remove( obj )
    assert( v.path( "prices.3a.price" ).get( obj ) == null )
  }

  test( "display" ) {

    val obj = Widget.make
    val v = obj.view

    obj( 'cert ) = Certification.CertifiedId

    val pvs = Path.flatten( obj )

    assert( pvs.size === 1 )
    assert( pvs( 0 ).displayValue === "Certified" )
  }

  test( "aidName_" ) {

    val rec = Widget.make
    rec( 'prices ) = Mlist( Mobj( "aid" -> 3, "price" -> 1.0 ), Mobj( "price" -> 2.0 ) )

    assert( rec.view.path( "prices.0.price" ).aidName_( rec ) === "prices_3a_price" )

    // falls back to array index if aid isn't present
    assert( rec.view.path( "prices.1.price" ).aidName_( rec ) === "prices_1_price" )
  }

  test( "scopePaths" ) {

    val rec = Widget.make
    rec( 'prices ) = Mlist( Mobj( "aid" -> 3, "price" -> 1.0 ), Mobj( "price" -> 2.0 ) )

    val scope1 = Scope( rec )
    assert( scope1.pathName_      === "" )
    assert( scope1.fullPath.name_ === "" )

    val scope2 = scope1.at( "prices.1" )
    assert( scope2.pathName_             === "" )
    assert( scope2.pathFromParent.name_  === "prices_1" )
    assert( scope2.fullPath.name_        === "prices_1" )

    val scope3 = scope2.at( "price" )
    assert( scope3.pathName_      === "price" )
    assert( scope3.fullPath.name_ === "prices_1_price" )

    val scope4 = scope1.at( "name" )
    assert( scope4.fullPath.name_ === "name" )

    val scope5 = scope1.at( "dims" )
    assert( scope5.pathName_      === "" )
    assert( scope5.fullPath.name_ === "dims" )

    val scope6 = scope5.at( "height" )
    assert( scope6.pathName_      === "height" )
    assert( scope6.fullPath.name_ === "dims_height" )

    val scope7 = scope1.at( "prices.3a" )
    assert( scope7.pathName_      === "" )
    assert( scope7.fullPath.name_ === "prices_3a" )
  }
}

