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

package org.tyranid.text

import org.scalatest.FunSuite

import org.tyranid.Imp._


class StringSuite extends FunSuite {

  test( "decUrl" ) {

    //"%u2122".decUrl

    //assert( re.base36  === Base36.toString( re.decimal ) )
  }

  test( "allBlank" ) {

    assert( null.asInstanceOf[String].notAllBlank === false )
    assert( "".notAllBlank                        === false )
    assert( "   \n\r".notAllBlank                 === false )
    assert( "   a\n\r".notAllBlank                === true )
    assert( "a".notAllBlank                       === true )
  }

  test( "tokenize" ) {

    assert( "blue,green,purple,violet"     .tokenize === Array( "blue", "green", "purple", "violet" ) )
    assert( "  blue green,purple    violet".tokenize === Array( "blue", "green", "purple", "violet" ) )
  }

  test( "splitAmp" ) {

    assert( "1&2& 3".splitAmp === Array( "1", "2", " 3" ) )
  }

}

