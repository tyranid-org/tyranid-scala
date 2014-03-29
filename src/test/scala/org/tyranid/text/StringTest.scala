/**
 * Copyright (c) 2008-2014 Tyranid <http://tyranid.org>
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

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

import org.tyranid.ImpT._

@RunWith(classOf[JUnitRunner])
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
  
  test ( "phoneMask" ) {
    assert( "19522003541".toOnlyNumbers == "19522003541" )
    assert( "19522003541".toPhoneMask == "(952) 200-3541" )
    assert( "19522003541".toPhoneMask.toOnlyNumbers == "9522003541" )
    assert( "9522003541".toPhoneMask == "(952) 200-3541" )
    assert( "9522003541".toPhoneMask.toOnlyNumbers  == "9522003541" )
  }
  
  test( "hashCrypt" ) {
    val password = "somepassword"
    
    assert( password.checkShash( password.shash() ) )
  }
  
  test( "isUrl" ) {
    val data = Seq(
      ( "http://",         false ),
      ( "https://",        false ),
      ( "http://foo.bar",  true ),
      ( "https://foo.bar", true ),
      ( "cat",             false ),
      ( "",                false )
    )

    for ( d <- data;
          url = d._1;
          expected = d._2 ) {
      assert( url.isUrl === expected )
    }
  }
}

