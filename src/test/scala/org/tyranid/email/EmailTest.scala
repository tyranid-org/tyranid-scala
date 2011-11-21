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

package org.tyranid.email

import org.scalatest.FunSuite

import org.tyranid.Imp._

case class EmailData( email:String, domain:String, domainPart:String )

class EmailSuite extends FunSuite {

  val addresses = Seq(
    EmailData( "yrc.officer@yahoo.com", "yahoo.com", "yahoo" )
  )

  test( "addresses" ) {

    for ( addr <- addresses ) {

      assert( Email.domainFor ( addr.email ) == addr.domain )
      assert( Email.domainPart( addr.email ) == addr.domainPart )
    }
  }
}
