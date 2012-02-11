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

package org.tyranid.linkedIn

import org.scalatest.FunSuite

import org.tyranid.Imp._


class LinkedInSuite extends FunSuite {

  test( "exchange" ) {

    val value = """{"signature_version":"1","signature_method":"HMAC-SHA1","signature_order":["access_token","member_id"],"access_token":"VDp9zX35CRVMQVqp-4WqSiqTH_WA5C3g-elW","signature":"NA8AVXcxXh3tE5k3dj44hXWvqFk=","member_id":"vadXjeVAsc"}"""

    LinkedIn.parseExchangeToken( value )

    //assert rslt  === expected )
  }
}

