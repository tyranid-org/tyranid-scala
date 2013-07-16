/**
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

package org.tyranid.document.markdown

import java.util.Date

import javax.servlet.http.HttpSession

import scala.xml.{ Text, Unparsed }

import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.json.JsModel
import org.tyranid.web.{ Weblet, WebContext }

object Markdown {

  //def convert( markdown:String ) = T.actuariusTransformer( markdown )
  def convert( markdown:String ) = new org.markdown4j.Markdown4jProcessor().process( markdown ).urlifyAsString
}

object Markdownlet extends Weblet {

  def handle( web:WebContext ) = {

    if ( !T.user.isGod )
      _404

    rpath match {
    case "/" =>
      val opts = web.jsobj( 'opts )

      web.json(
        Map(
          "html" -> Markdown.convert( opts.s( 'markdown ) )
        )
      )

    case _ =>
      _404
    }
  }
}


