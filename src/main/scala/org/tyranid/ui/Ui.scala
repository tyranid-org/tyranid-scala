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

import net.liftweb.http.S
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.JE.JsRaw

import org.tyranid.Imp._
import org.tyranid.db.{ Record, Scope, View }
import org.tyranid.logic.Invalid


object Opts {
  val Empty = Opts()
}

case class Opts( opts:(String,String)* )

object Field {

  implicit def string2Field( name:String )( implicit view:View ) = Field( name )
  implicit def symbol2Field( name:Symbol )( implicit view:View ) = Field( name.name )
}

case class Field( name:String, opts:Opts = Opts.Empty, span:Int = 1 )( implicit view:View ) {

  lazy val va = view( name )

  private def invalidLines( invalids:Seq[Invalid] ) =
    for ( invalid <- invalids )
      yield <p>{ invalid.message }</p>

  def ui( rec:Record ) = {
    val invalids = va.invalids( Scope( rec, Some( va ) ) )

    <div id={ va.name + "_c" } class={ "fieldc" + ( !invalids.isEmpty |* " invalid" ) }>{

      <div class="labelc">{ va.label( rec, opts.opts:_* ) }{ va.att.required |* <span class="required">*</span> }</div>
      <div class="inputc">{ va.att.domain.ui( rec, this, ( opts.opts ++ Seq( "id" -> va.name ) ):_* ) }</div>
      <div id={ va.name + "_e" } class="notec">{ !invalids.isEmpty |* invalidLines( invalids ) }</div>
    }</div>
  }

  def updateDisplayCmd( r:Record ) = {
    val invalids = va.invalids( Scope( r, Some( va ) ) )
      
    if ( invalids.isEmpty ) {
      if ( r.invalids( va.index ) ) {
        r.invalids -= va.index

        SetHtml( va.name + "_e", NodeSeq.Empty ) &
        JsRaw( "$('#" + va.name + "_c').removeClass('invalid');" )
      } else {
        Noop
      }
    } else {
      r.invalids += va.index

      SetHtml( va.name + "_e", invalidLines( invalids ) ) &
      JsRaw( "$('#" + va.name + "_c').addClass('invalid');" )
    }
  }
}

case class Row( fields:Field* )

object Grid {

  def apply( rec:Record, rows:Row* ) = new Grid( rec.view, rows:_* ).draw( rec )
}

case class Grid( view:View, rows:Row* ) {
  val boxSpan = rows.map( _.fields.length ).max

  def draw( rec:Record ) =
    for ( row <- rows ) yield
      <tr>{
        for ( f <- row.fields ) yield
          <td colspan={ f.span.toString } class="cell">{ f.ui( rec ) }</td>
      }</tr>
}

