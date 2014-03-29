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

package org.tyranid.oauth

import org.scalatest.FunSuite

import org.tyranid.ImpT._


class OAuthSuite extends FunSuite {

  test( "encOAuth" ) {
    val data = Seq(
      "+",
      "hello there",
      "l1kj2329%2390!@#!@#(*%&*ASDJksalasoieuq][;'l><.,.,.,/???~`"
    )

    for ( s <- data ) {
      assert( s === s.encOAuthUrl.decOAuthUrl )
    }



  }
}

