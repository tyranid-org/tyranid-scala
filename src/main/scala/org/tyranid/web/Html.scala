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

package org.tyranid.web

import scala.collection.JavaConversions._

import org.jsoup.Jsoup

import org.tyranid.Imp._

case class Html( html:String ) {
  val doc = Jsoup.parse( html )

  def images = doc.select( "img" ).map( _.attr( "src" ) )

  def ogImages = doc.select( "meta[property=og:image]" ).map( _.attr( "content" ) )

  def title = doc.select( "title" ).text
  def description = doc.select( "meta[property=og:description]" ).map( _.attr( "content" ) ).mkString

  def favicon = {
    val s = doc.select( "link[rel=icon]" ).map( _.attr( "href" ) ).headOption.getOrElse( null )

    if ( s.notBlank ) s
    else              doc.select( "link[rel=shortcut icon]" ).map( _.attr( "href" ) ).headOption.getOrElse( null )
  }  
}

