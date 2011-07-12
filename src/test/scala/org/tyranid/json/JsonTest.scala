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

    assert( jackson( json ).foo.s === "bar" )
  }
}

