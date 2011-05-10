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

package org.tyranid.ui

import scala.xml.NodeSeq

import org.tyranid.Imp._
import org.tyranid.db.{ View, Record }

object Opts {
  val Empty = Opts()
}

case class Opts( opts:(String,String)* )

object Field {

  implicit def string2Field( name:String ) = Field( name )
  implicit def symbol2Field( name:Symbol ) = Field( name.name )
}

case class Field( name:String, opts:Opts = Opts.Empty, span:Int = 1 ) {

  def va( view:View ) = view( name )
}

case class Row( fields:Field* )

case class Box( rec:Record, rows:Row* ) {
  val boxSpan = rows.map( _.fields.length ).max

  implicit def draw =
    <table>
    {
      for ( row <- rows;
            line <- 1 to 3 ) yield
        <tr>
        {
          val fc = row.fields.length
          var fi = 0
          val remainingSpan = boxSpan

          for ( f <- row.fields ) yield {
            fi += 1
            val span = ( fi == fc ) ? remainingSpan | f.span
            val va = f.va( rec.view )

            <td colspan={ span.toString }>{
              line match {
              case 1 => va.label( rec, f.opts.opts:_* )
              case 2 => va.ui( rec, f.opts.opts:_* )
              case 3 => NodeSeq.Empty
              }
            }</td>
          }
        }
        </tr>
    }
    </table>
}

