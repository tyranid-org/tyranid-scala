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

package org.tyranid.math

import scala.util.Random

import org.scalatest.FunSuite

import org.tyranid.Imp._


case class RadixLong( decimal:Long, base36:String, base62:String )
case class RadixBigInt( decimal:BigInt, base36:String, base62:String )

class RadixSuite extends FunSuite {
  val longs = Seq(
    RadixLong(           0L,         "0",        "0" ),
    RadixLong(           1L,         "1",        "1" ),
    RadixLong(          -1L,        "-1",       "-1" ),
    RadixLong(          -3L,        "-3",       "-3" ),
    RadixLong(          10L,         "A",        "A" ),
    RadixLong(          36L,        "10",        "a" ),
    RadixLong(          62L,        "1Q",       "10" ),
    RadixLong(      238328L,      "53W8",     "1000" ),
    RadixLong( 56800235584L,   "Q3DE69S",  "1000000" ),
    RadixLong( -56800235584L, "-Q3DE69S", "-1000000" )
  )

  val bigints = Seq(
    RadixBigInt( BigInt( 62 ),    "1Q",     "10" )
  )

  test( "longRadices" ) {

    for ( re <- longs ) {
      assert( re.decimal === Base36.toLong( re.base36 ) )
      assert( re.base36  === Base36.toString( re.decimal ) )

      assert( re.decimal === Base62.toLong( re.base62 ) )
      assert( re.base62  === Base62.toString( re.decimal ) )
    }
  }

  test( "bigIntRadices" ) {

    for ( re <- longs ) {
      assert( BigInt( re.decimal ) === Base36.toBigInt( re.base36 ) )
      assert( re.base36            === Base36.toString( BigInt( re.decimal ) ) )

      assert( BigInt( re.decimal ) === Base62.toBigInt( re.base62 ) )
      assert( re.base62            === Base62.toString( BigInt( re.decimal ) ) )
    }

    for ( re <- bigints ) {
      assert( re.decimal === Base36.toBigInt( re.base36 ) )
      assert( re.base36  === Base36.toString( re.decimal ) )

      assert( re.decimal === Base62.toBigInt( re.base62 ) )
      assert( re.base62  === Base62.toString( re.decimal ) )
    }
  }

  test( "randomBytes" ) {
    val bytes = new Array[Byte]( 12 )
    val random = new Random

    for ( i <- 1 to 10 ) {
      random.nextBytes( bytes )

      val bi = BigInt( bytes )

      assert( bi === Base36.toBigInt( Base36.toString( bi ) ) )
      assert( bi === Base62.toBigInt( Base62.toString( bi ) ) )
      assert( bytes === Base64.toBytes( Base64.toString( bytes ) ) )
    }
  }
}

