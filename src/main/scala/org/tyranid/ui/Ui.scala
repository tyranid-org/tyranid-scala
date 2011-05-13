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

object Grid {

  def apply( rec:Record, rows:Row* ) = new Grid( rec.view, rows:_* ).draw( rec )
}

case class Grid( view:View, rows:Row* ) {
  val boxSpan = rows.map( _.fields.length ).max

  def draw( rec:Record ) =
    for ( row <- rows;
          line <- 1 to 3 ) yield
      <tr>{
        val fc = row.fields.length
        var fi = 0
        val remainingSpan = boxSpan

        for ( f <- row.fields ) yield {
          fi += 1
          val span = ( fi == fc ) ? remainingSpan | f.span
          val va = f.va( rec.view )

          line match {
          case 1 => <td class="labelc" colspan={ span.toString }>{ va.label( rec, f.opts.opts:_* ) }{ va.att.required |* <span class="required">*</span> }</td>
          case 2 => <td class="fieldc" colspan={ span.toString }>{ va.ui( rec, ( f.opts.opts ++ Seq( "id" -> va.name ) ):_* ) }</td>
          case 3 => <td class="notec"  colspan={ span.toString }></td>
          }
        }
      }</tr>
}

