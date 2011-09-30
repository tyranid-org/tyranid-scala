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

package org.tyranid.collection

import org.scalatest.FunSuite

import org.tyranid.Imp._


class SeqSuite extends FunSuite {

  test( "group" ) {
    val data = Seq( ( 1, 'a ), ( 1, 'b ), ( 2, 'a ), ( 3, 'd ) )

    val map = data.group

    assert( map( 1 ) === Seq( 'a, 'b ) )
    assert( map( 2 ) === Seq( 'a ) )
    assert( map( 3 ) === Seq( 'd ) )
  }

  test( "groupBy2" ) {
    val data = Seq( ( 1, 'a, 'foo ), ( 1, 'b, 'bar ), ( 2, 'a, 'cat ), ( 3, 'd, 'dog ) )

    val m2 = data.groupBy2( _._1, _._2 )
    assert( m2( 1 ) === Seq( 'a, 'b ) )
    assert( m2( 2 ) === Seq( 'a ) )
    assert( m2( 3 ) === Seq( 'd ) )

    val m3 = data.groupBy2( _._1, _._3 )
    assert( m3( 1 ) === Seq( 'foo, 'bar ) )
    assert( m3( 2 ) === Seq( 'cat ) )
    assert( m3( 3 ) === Seq( 'dog ) )
  }
}

