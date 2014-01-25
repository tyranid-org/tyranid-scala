/*
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

package org.tyranid.web

import java.util.{ Calendar, Date, TimeZone }

import scala.xml.Unparsed

import org.scalatest.FunSuite

import org.tyranid.Imp._
import org.tyranid.session.Session


class WebSuite extends FunSuite {
  org.tyranid.boot.Boot.boot

  test( "thumbUrl" ) {


    assert( !WebFilter.notAsset( "https://delta.foo.com/io/thumb/a0YvUPgCkCqIOx4r2C8nv/m" ) )
  }
}

