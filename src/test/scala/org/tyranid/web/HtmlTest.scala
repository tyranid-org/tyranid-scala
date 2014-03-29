/*
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

package org.tyranid.web

import org.scalatest.FunSuite

import org.tyranid.ImpT._


class HtmlSuite extends FunSuite {

  test( "images" ) {
    assert(
      """<html><body><div><img src="bar.jpg"></div><img bar="blah" src="cat.png"/></body></html>""".parseHtml.images ===
      Seq( "bar.jpg", "cat.png" ) )
  }

  test( "ogImage" ) {
    assert(
      """
      <html>
       <head>
        <meta property="og:image" content="http://fortunebrainstormtech.files.wordpress.com/2012/04/lochhead.jpeg"/>
       </head>
      </html>
      """.parseHtml.ogImages ===
      Seq( "http://fortunebrainstormtech.files.wordpress.com/2012/04/lochhead.jpeg" ) )
  }
}

