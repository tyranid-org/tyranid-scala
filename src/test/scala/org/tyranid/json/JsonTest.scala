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

package org.tyranid.json

import org.scalatest.FunSuite

import org.tyranid.Imp._


class JsonSuite extends FunSuite {

  test( "basic" ) {

    val json = """
{  "foo": "bar",
  "five": 5
}""".toJson

    //assert( jackson( json ).foo.s === "bar" )
  }

  test( "stringify" ) {
    val data = Seq(
      "cat",                                       "\"cat\"",
      5,                                           "5",
      List( 1, 2, 3 ),                             "[1,2,3]",
      List( 1, "cat", 3 ),                         """[1,"cat",3]""",
	  Array( 1, 2, 3 ),							   """[1,2,3]""",
      Map( "bar" -> 1, "foo" -> List( 1, 2, 3 ) ), """{"bar":1,"foo":[1,2,3]}""",
      false,                                       "false"
    )

    for ( d <- 0 to data.size - 1 by 2 )
      assert( data( d ).toJsonStr === data( d+1 ) )
  }
}

