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

import scala.collection.mutable.LinkedHashMap

import org.scalatest.FunSuite

import org.tyranid.Imp._


class JsonSuite extends FunSuite {

  test( "basic" ) {

    val json = """
{  "foo": "bar",
  "five": 5
}""".toJson
  }

  test( "stringify" ) {
    val data = Seq(
      "cat",                                       "\"cat\"",
      "a\"b",                                      "\"a\\\"b\"",
      5,                                           "5",
      List( 1, 2, 3 ),                             "[1,2,3]",
      List( 1, "cat", 3 ),                         """[1,"cat",3]""",
	    Array( 1, 2, 3 ),							               """[1,2,3]""",
      Map( "bar" -> 1, "foo" -> List( 1, 2, 3 ) ), """{"bar":1,"foo":[1,2,3]}""",
      false,                                       "false"
    )

    for ( d <- 0 to data.size - 1 by 2 )
      assert( data( d ).toJsonStr === data( d+1 ) )
  }

  test( "parsing" ) {
    val data = Seq(
      ( "null",                         null ),
      ( "true",                         true ),
      ( "false",                        false ),
      ( "3",                            3 ),
      ( "3000000000000000",             3000000000000000L ),
      ( "\"hi\"",                       "hi" ),
      ( "'h\\u0003i'",                  "h\u0003i" ),
      ( "'hi \"there\"'",               "hi \"there\"" ),
      ( "''",                           "" ),
      ( "[]",                           Array() ),
      ( "[0,1]",                        Array( 0, 1 ) ),
      ( "{\"test\":3}",                 LinkedHashMap( "test" -> 3 ) ),
      ( "{test:3}",                     LinkedHashMap( "test" -> 3 ) ),
      ( "{\"test\":3,\"foo\":\"bar\"}", LinkedHashMap( "test" -> 3, "foo" -> "bar" ) ),
      ( "{test:3,foo:'bar'}",           LinkedHashMap( "test" -> 3, "foo" -> "bar" ) ),
      ( "{test:null,foo:false}",        LinkedHashMap( "test" -> null, "foo" -> false ) ),
      ( "{}",                           Map() )
    )

    for ( d <- data )
      assert( d._1.parseJson === d._2 )
  }

  test( "parsingNumbers" ) {
    val data = Seq(
      ( "3",                classOf[java.lang.Integer] ),
      ( "3000000000000000", classOf[java.lang.Long] ),
      ( "3.2",              classOf[java.lang.Double] )
    )

    for ( d <- data )
      assert( d._1.parseJson.getClass === d._2 )
  }

  test( "parse1" ) {
    val jsonStr = """
{"positions": {
  "_total": 4,
  "values": [
    {"company": {
      "industry": "Internet"
    }},
    {"company": {
      "industry": "Internet",
      "size": "11-50 employees",
      "type": "Privately Held"
    }},
    {"company": {
      "industry": "Internet"
    }},
    {"company": {
      "industry": "Computer Software",
      "size": "51-200 employees",
      "type": "Privately Held"
    }}
  ]
}}
"""

    val json = jsonStr.parseJsonObject

    assert( json.o( 'positions ).a_?( 'values ).size === 4 )

  }
}

