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

package org.tyranid.time

import org.scalatest.FunSuite

import org.tyranid.Imp._


class TimeSuite extends FunSuite {

  test( "parsing" ) {

    assert( "Sep-10-2015".isDate === true )
    assert( "June-4-2015".isDate === true )

    assert( "Sep-10-2015".toLaxDate === "09/10/2015".toLaxDate )
    assert( "Aug/10/2015".toLaxDate === "08-10-2015".toLaxDate )
    assert( "June-05-2015".toLaxDate === "Jun-05-2015".toLaxDate )
    assert( "June-5-2015".toLaxDate === "Jun-05-2015".toLaxDate )

    //"%u2122".decUrl

    //assert( re.base36  === Base36.toString( re.decimal ) )
  }
}

