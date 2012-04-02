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

package org.tyranid.collection

import org.scalatest.FunSuite

import org.tyranid.Imp._


class ArraySuite extends FunSuite {

  test( "resize" ) {

    val arr = Array( "one", "two", "three", "four", "five" )

    val arr5 = arr.resize( 5 )
    assert( arr5 eq arr )

    val arr4 = arr.resize( 4 )
    assert( arr4.mkString( " " ) === "one two three four" )

    val arr6 = arr.resize( 6 )
    assert( arr6.mkString( " " ) === "one two three four five null" )
  }
}

