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
import scala.xml.{ NodeSeq, Text, Unparsed }

import com.mongodb.DBObject

import net.liftweb.common.{ Full, Empty }
import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.{ Noop, SetHtml }
import net.liftweb.http.js.JE.JsRaw

import org.tyranid.Imp._
import org.tyranid.db.{ Path, Record, ViewAttribute }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.MongoEntity
import org.tyranid.session.Session
import org.tyranid.ui.Button


trait Query {

  val name:String
  def label = name.camelCaseToSpaceUpper

  val allFields:Seq[Field]

  val defaultFields:Seq[Field]

  def prepareSearch( report:Report ) = report.search

  def run( report:Report ):Iterable[Record]

  def newReport = {
    var r = new Report
    r.columns ++= defaultFields
    r.hidden ++= allFields.filter( p => !r.columns.contains( p ) ).sortBy( _.label )
    r
  }

  def by( name:String ) = allFields.find( _.name == name ).get

  val searchScreen:String = null
}

trait Field {
  def section = "Standard"

  def name:String

  def label = name.camelCaseToSpaceUpper

  def header( run:Run ) = <th id={ name } class={ if ( run.report.selectedColumns( name ) ) "colh hi" else "colh" } style={ headerStyle }><div><span>{ headerCell }</span></div></th>
  def headerStyle = ""
  def headerCell:NodeSeq = Text( label )

  def cellClass:String = null
  def cell( run:Run, rec:Record ):NodeSeq

}

case class PathField( sec:String, path:Path ) extends Field {

  override def section = sec

  def name = path.name_
  override def label = path.label

  def cell( run:Run, r:Record ) = Text( r s name )
}

trait MongoQuery extends Query {
  val entity:MongoEntity

  lazy val view = entity.makeView

  def run( report:Report ) = {
    var part = entity.db.find( prepareSearch( report ), Mobj() ).limit( 20 );

    if ( report.offset != 0 )
      part = part.skip( report.offset )
    if ( report.sort != null )
      part = part.sort( report.sort )
    
    part.toIterable.map( o => entity.apply( o ) )
  }

  def field( path:String, sec:String = "Standard" ) = PathField( sec, view.path( path ) )
}

class Report {

  @volatile var name:String = _

  // TODO:  make this database-agnostic
  val search        = Mobj()
  val parameters    = Mobj()
  @volatile var sort:DBObject = null
  @volatile var offset:Int = 0
  @volatile var pageSize = 20

  @volatile var onSection:String = ""
  @volatile var onField:String = ""

  val hidden  = mutable.ArrayBuffer[Field]()
  val columns = mutable.ArrayBuffer[Field]()

  val selectedColumns = mutable.Set[String]()

  def remove( remove:Field ) = {
    columns -= remove
    hidden -= remove
    hidden += remove
  }

  def add( add:Field ) = {
    hidden -= add
    columns -= add
    columns += add
  }

  def insertBefore( insert:Field, before:Field ) = {
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

case class Run( report:Report, grid:Grid ) {

  val cache = mutable.HashMap[String,AnyRef]()

  val header = <tr>{ report.columns.map( _.header( this ) ) }</tr>

  def row( rec:Record ) = {
    // TODO:  get IDs on these ...{ report.columns.map( p => <th class="colh" id={ p.name_ }>{ col( p ) }</th> ) }
    <tr class={ grid.rowClass }>{
      for ( f <- report.columns ) yield {
        val cls = f.cellClass

        if ( cls != null )
          <td class={ cls }>{ f.cell( this, rec ) }</td>
        else
          <td>{ f.cell( this, rec ) }</td>
      }
    }</tr>
  }
}

case class Grid( query:Query ) {

  val report = Session().reportFor( query )

  val id = "grid" // TODO:  modify this by session
  var odd = false

  def rowClass = {
    odd = !odd
    !odd |* "even"
  }

  def exec( js:String ) = {
    if ( js.startsWith( "!" ) ) {
      val fp = query.by( js.substring(1) )

      val empty = report.selectedColumns.isEmpty
      report.selectedColumns( fp.name ) = !report.selectedColumns( fp.name )

      empty != report.selectedColumns.isEmpty |*
        SetHtml( "searchTitle", searchTitle )
    } else {
      val ( fn, tn ) = js.splitFirst( ':' )

      val fp = query.by( fn )

      if ( tn == "def" )
        // TODO:  if selections, remove all selections as well, also unselect everything
        report.remove( fp )
      else if ( tn == "_end" )
        report.add( fp )
      else
        // TODO:  if selections, insert everything before insert ... (if insert is selected, don't move it)
        report.insertBefore( insert = fp, before = query.by( tn ) )

      recalcFields
      redraw
    }
  }

  private def initReport:JsCmd =
    net.liftweb.http.js.JsCmds.Run( "initReport();" )

  private def redraw:JsCmd =
    SetHtml( id, innerDraw ) &
    initReport


  /*
   * * *  Navigation
   */

  private def prev = {
    report.offset -= report.pageSize
    if ( report.offset < 0 ) report.offset = 0
    redraw
  }

  private def next = {
    report.offset += report.pageSize
    redraw
  }


  /*
   * * *  Search
   */

  def searchTitle =
    if ( report.selectedColumns.isEmpty ) Text( "search all fields" )
    else                                  Unparsed( "search <span class='hitext'>highlighted</span> fields" )


  /*
   * * *  Field Dropdowns
   */

  private val sections = "Standard" +: query.allFields.map( _.section ).filter( _ != "Standard" ).sorted.distinct

  private def calcFields = query.allFields.filter( f => f.section == section && !report.columns.contains( f ) ).sortBy( _.label )
  private def recalcFields {
    fields = calcFields
    if ( !fields.find( _.name == field ).isDefined )
       report.onField = ""
  }

  private var fields     = calcFields

  private def section = {
    if ( report.onSection.isBlank )
      report.onSection = sections( 0 )

    report.onSection
  }

  private def field = {
    if ( report.onField.isBlank )
      report.onField = if ( fields.size > 0 ) fields( 0 ).name
                       else                   ""

    report.onField
  }

  private def sectionDropdown =
    SHtml.ajaxSelect(
      sections.map( sec => sec -> sec ),
      Full( section ),
      v => {
        report.onSection = v
        recalcFields
        SetHtml( "repfd", fieldDropdown ) &
        SetHtml( "repab", addBox ) &
        initReport
      },
      "style" -> "width:150px; max-width:150px;" )

  private def fieldDropdown =
    if ( fields.size == 0 )
    Unparsed( "<select style='width:150px;' disabled><option>none left</option></select>" )
    else
    SHtml.ajaxSelect(
      fields.map( f => f.name -> f.label ),
      Full( field ),
      v => {
        report.onField = v
        SetHtml( "repab", addBox ) &
        initReport
      },
      "style" -> "width:150px; max-width:150px;" )

  private def addBox:NodeSeq =
    fields.find( _.name == field ).flatten(
      f => <div id={ f.name } class="cola"><div><span>Add</span></div></div>,
      <div class="colna"><div><span>N/A</span></div></div>
    )

  private def innerDraw = {
    val rows = query.run( report )
    val run = new Run( report, this )

    <div class="header">
     { query.label }
    </div> ++
    <table class="def" id="def">
     <tr>
      <td>
       <table class="tile" style="width:300px; height:54px;">
        <tr>
         <td class="label">actions</td>
        </tr>
        <tr> 
         <td style="padding:0;">
          <table>
           <tr>
            { Seq(
                query.searchScreen.notBlank |* Some( Button.link( "Change Search", query.searchScreen, color = "grey" ) ),
                report.offset > 0 |* Some( Button.ajaxButton( "Prev", () => prev, color = "grey" ) ),
                Some( Button.ajaxButton( "Next", () => next, color = "grey" ) )
              ).flatten.map( btn => <td>{ btn }</td> ) }
           </tr>
          </table>
         </td>
        </tr>
       </table>
      </td>
      <td style="width:410px;">
      </td>
      <td>
       <table class="tile" style="width:226px; height:54px;">
        <tr>
         <td id="searchTitle" class="label">{ searchTitle }</td>
        </tr>
        <tr>
         <td>
          <form method="get" class="searchBox" action="/search/results" onsubmit="this.submit();return false;">
	         <fieldset>
            { SHtml.text( "", v => println( "do something with " + v ), "placeholder" -> "Search", "class" -> "field" ) }
		        <input type="image" class="btn" name="submit" src="http://www.volerro.com/wp-content/themes/platformpro/images/search-btn.png" alt="Go"/>
	         </fieldset>
          </form>
         </td>
        </tr>
       </table>
      </td>
      <td>
       <table class="tile" style="width:298px; height:54px;">
        <tr>
         <td class="label">section</td>
         <td id="repcd" style="width:160px;">{ sectionDropdown }</td>
         <td rowspan="2" id="repab" style="width:130px;">{ addBox }</td>
        </tr>
        <tr>
         <td class="label">field</td>
         <td id="repfd" style="width:160px;">{ fieldDropdown }</td>
        </tr>
       </table>
      </td>
     </tr>
    </table> ++
    <div class="grid">
     <table>
      <thead>
       { run.header }
      </thead>
      <tbody>
       { rows.map( r => run.row( r ) ) }
      </tbody>
     </table>
    </div>
  }

  def draw = {

    <head>
     <script src="/cjs/report.js" type="text/javascript"/>
     <script>{ Unparsed( """

function gridexec( id ) {
  """ + SHtml.ajaxCall( JsRaw( "id" ), exec _ )._2.toJsCmd + """;
}

     """ ) }</script>
    </head>
    <div class="report" id={ id }>
    { innerDraw }
    </div>
  }
}


