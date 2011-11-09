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

package org.tyranid.report

import scala.collection.mutable
import scala.xml.Unparsed

import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmds.SetHtml
import net.liftweb.http.js.JE.JsRaw

import org.tyranid.Imp._
import org.tyranid.db.{ Path, Record, ViewAttribute }
import org.tyranid.session.Session


trait Query {

  val name:String

  val paths:Seq[Path]

  val defaultColumns:Seq[Path]

  def run( report:Report ):Iterable[Record]

  def newReport = {
    var r = new Report
    r.columns ++= defaultColumns
    r.hidden ++= paths.filter( p => !r.columns.contains( p ) )
    r
  }

  def by( name:String ) = paths.find( _.name_ == name ).get
}

class Report {

  @volatile var name:String = _

  val filters = mutable.Map[Path,Any]()

  val hidden  = mutable.ArrayBuffer[Path]()
  val columns = mutable.ArrayBuffer[Path]()

  def remove( remove:Path ) = {
    columns -= remove
    hidden -= remove
    hidden += remove
  }

  def insertBefore( insert:Path, before:Path ) = {
    hidden -= insert
    columns -= insert
    columns.insert( columns.indexOf( before ), insert )
  }
}

case class Grid( query:Query ) {

  val report = Session().reportFor( query )

  val id = "grid" // TODO:  modify this by session

  def drag( js:String ) = {
    val ( fn, tn ) = js.splitFirst( ':' )

    val fp = query.by( fn )

    if ( tn == "def" ) {
      report.remove( fp )

    } else {
      report.insertBefore( insert = fp, before = query.by( tn ) )
    }

    SetHtml( id, innerDraw ) &
    net.liftweb.http.js.JsCmds.Run( "initReport();" )
  }

  private def col( p:Path ) =
    <div id={ p.name_ }>{ p.label }</div>

  private def innerDraw = {
    val rows = query.run( report )

    <div id="def" class="def">
     <div>Available Columns</div>
     <div class="availc">
      <table class="colc">
       { for ( p <- report.hidden ) yield
         <tr><td id={ p.name_ } class="cola">{ col( p ) }</td></tr>
       }
      </table>
     </div>
     <div>Filtered Columns</div>
    </div>
    <table class="grid">
     <thead>
      { report.columns.map( p => <th class="colh" id={ p.name_ }>{ col( p ) }</th> ) }
     </thead>
     <tbody>
      { for ( r <- rows ) yield
      <tr>
       { report.columns.map( p => <td>{ p.get( r ).asInstanceOf[AnyRef].safeString }</td> ) }
      </tr> }
     </tbody>
    </table>
  }

  def draw = {

    <head>
     <script src="/js/report.js" type="text/javascript"/>
     <script>{ Unparsed( """

function execDrag( id ) {
  """ + SHtml.ajaxCall( JsRaw( "id" ), drag _ )._2.toJsCmd + """;
}

     """ ) }</script>
    </head>
    <div class="report" id={ id }>
    { innerDraw }
    </div>
  }
}


