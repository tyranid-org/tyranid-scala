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
import scala.xml.{ NodeSeq, Unparsed }

import com.mongodb.DBObject

import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.SetHtml
import net.liftweb.http.js.JE.JsRaw

import org.tyranid.Imp._
import org.tyranid.db.{ Path, Record, ViewAttribute }
import org.tyranid.db.mongo.Imp._
import org.tyranid.session.Session
import org.tyranid.ui.Button


trait Query {

  val name:String

  val paths:Seq[Path]

  val defaultColumns:Seq[Path]

  def run( report:Report ):Iterable[Record]


  def newReport = {
    var r = new Report
    r.columns ++= defaultColumns
    r.hidden ++= paths.filter( p => !r.columns.contains( p ) ).sortBy( _.label )
    r.layout = "Standard"
    r
  }

  def by( name:String ) = paths.find( _.name_ == name ).get

  val searchScreen:String = null

  def layouts:Seq[Layout] = Nil

  def layout( name:String ) = layouts.find( _.name == name )
}

trait Layout {
  def name:String
  def run( grid:Grid ):LayoutRun
}

trait LayoutRun {
  def header:NodeSeq
  def row( rec:Record ):NodeSeq
}

class Report {

  @volatile var name:String = _

  @volatile var layout:String = _

  // TODO:  make this database-agnostic
  val search        = Mobj()
  val parameters    = Mobj()
  @volatile var sort:DBObject = null
  @volatile var offset:Int = 0
  @volatile var pageSize = 20

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

  def label( title:String, attr:String ) = <label for={ attr }>{ title }</label>

  def bool( title:String, attr:String ) =
    SHtml.checkbox(
      search.b( attr ),
      ( v:Boolean ) => {
        if ( v ) search( attr ) = v
        else     search.remove( attr )
      } ) ++ label( title, attr )

  def boolExists( title:String, attr:String ) =
    SHtml.checkbox(
      search.s( attr ).notBlank,
      ( v:Boolean ) => {
        if ( v ) search( attr ) = Mobj( $gt -> "" )
        else     search.remove( attr )
      } ) ++ label( title, attr )

  def textUpper( attr:String, width:Int ) =
    SHtml.text( search.s( attr ),
    ( v:String ) => {
      if ( v.notBlank ) search( attr ) = v.toUpperCase
      else              search.remove( attr )
    },
    "style" -> ( "width:" + width.toString + "px;" ) )

  def textUpperSubst( attr:String, width:Int ) =
    SHtml.text(
      { val regex = search.o( attr )
        regex != null |* regex.s( $regex )
      },
      ( v:String ) => {
        if ( v.notBlank ) search( attr ) = Mobj( $regex -> v.toUpperCase )
        else              search.remove( attr )
      },
      "style" -> ( "width:" + width.toString + "px;" ) )

  def intGte( attr:String ) =
    SHtml.text(
      { val o = search.o( attr )

        if ( o == null ) ""
        else             o.i( $gte ).toString },
      ( v:String ) => {
        val i = v.toLaxInt

        if ( i == 0 ) search.remove( attr )
        else          search( attr ) = Mobj( $gte -> i ) },
      "style" -> "width:80px;" )
}

case class Grid( query:Query ) {

  val report = Session().reportFor( query )

  val id = "grid" // TODO:  modify this by session
  var odd = false

  def rowClass = {
    odd = !odd
    odd |* "odd"
  }

  def drag( js:String ) = {
    val ( fn, tn ) = js.splitFirst( ':' )

    val fp = query.by( fn )

    if ( tn == "def" )
      report.remove( fp )
    else
      report.insertBefore( insert = fp, before = query.by( tn ) )

    redraw
  }

  private def redraw:JsCmd =
    SetHtml( id, innerDraw ) &
    net.liftweb.http.js.JsCmds.Run( "initReport();" )

  private def col( p:Path ) =
    <div id={ p.name_ }>{ p.label }</div>

  private def prev = {
    report.offset -= report.pageSize
    if ( report.offset < 0 ) report.offset = 0
    redraw
  }

  private def next = {
    report.offset += report.pageSize
    redraw
  }

  private def adHoc = {
    report.layout = null
    redraw
  }

  private def standard = {
    report.layout = "Standard"
    redraw
  }

  private def innerDraw = {
    val rows = query.run( report )

    <div class="gridNav">
     { Button.bar(
         Seq(
           query.searchScreen.notBlank |* Some( Button.link( "Change Search", query.searchScreen, color = "grey" ) ),
           report.offset > 0 |* Some( Button.ajaxButton( "Prev", () => prev, color = "grey" ) ),
           Some( Button.ajaxButton( "Next", () => next, color = "grey" ) ),
           report.layout.notBlank |* Some( Button.ajaxButton( "Ad-Hoc", () => adHoc, color = "grey" ) ),
           report.layout.isBlank |* Some( Button.ajaxButton( "Standard", () => standard, color = "grey" ) )
         ).flatten:_*
       ) }
    </div> ++
    { if ( report.layout.notBlank ) {
      val run = query.layout( report.layout ).get.run( this )

      <div class="grid">
       <table>
        <thead>
         { run.header }
        </thead>
        <tbody>
         { for ( r <- rows ) yield
             run.row( r )
         }
        </tbody>
       </table>
      </div>
      } else {
      <table id="def" class="def">
       <tr>
        <th>Available Columns</th>
        <td>
         <div class="availc">
          <table class="colc">
           { for ( p <- report.hidden ) yield
             <tr><td id={ p.name_ } class="cola">{ col( p ) }</td></tr>
           }
          </table>
         </div>
        </td>
       </tr>
       <tr>
        <th>Filtered Columns</th>
       </tr>
      </table>
      <div class="grid">
       <table>
        <thead>
         { report.columns.map( p => <th class="colh" id={ p.name_ }>{ col( p ) }</th> ) }
        </thead>
        <tbody>
         { for ( r <- rows ) yield
         <tr class={ rowClass }>
          { report.columns.map( p => <td>{ p.get( r ).asInstanceOf[AnyRef].safeString }</td> ) }
         </tr> }
        </tbody>
       </table>
      </div>
      }
    }
  }

  def draw = {

    <head>
     <script src="/cjs/report.js" type="text/javascript"/>
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


