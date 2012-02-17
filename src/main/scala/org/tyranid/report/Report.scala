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
import net.liftweb.http.js.JsCmds
import net.liftweb.http.js.JsCmds.{ Noop, SetHtml }
import net.liftweb.http.js.JE.JsRaw

import org.tyranid.Imp._
import org.tyranid.db.{ Entity, Path, Record, ViewAttribute }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.MongoEntity
import org.tyranid.session.Session
import org.tyranid.ui.{ Button, Checkbox, Glyph, Input, Select }
import org.tyranid.web.{ Weblet, WebContext }


case class Grouping( entity:MongoEntity, keyName:String, value: () => AnyRef ) {

  def list = entity.db.find( Mobj( keyName -> value() ) ).toSeq
}

trait Query {

  val name:String
  def label = name.camelCaseToSpaceUpper

  val allFields:Seq[Field]

  val defaultFields:Seq[Field]

  def prepareSearch( run:Run ) = run.report.search

  def run( run:Run ):Iterable[Record]

  def newReport = {
    var r = Report( this )
    r.columns ++= defaultFields
    r.hidden ++= allFields.filter( p => !r.columns.contains( p ) ).sortBy( _.label )
    r
  }

  def by( name:String ) = allFields.find( _.name == name ).get

  val searchScreen:String = null

  def selectable = grouping != null

  //val actions = Seq(
    //"Group",          // multi-select
    //"Connection"      // multi-select
  //)

  val grouping:Grouping = null
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

trait PathField extends Field {
  val path:Path

  def name = path.name_
}

// TODO:  merge this functionality with Domain
case class StringField( sec:String, path:Path, l:String = null ) extends PathField {

  override def section = sec

  override def label = if ( l.notBlank ) l else path.label

  def cell( run:Run, r:Record ) = Text( path s r )
}

case class MultilineStringField( sec:String, path:Path, l:String = null ) extends PathField {

  override def section = sec

  override def label = if ( l.notBlank ) l else path.label

  def cell( run:Run, r:Record ) = Unparsed( path.s( r ).replace( "\n", "<br/>" ) )
}

case class BooleanField( sec:String, path:Path, l:String = null ) extends PathField {

  override def section = sec

  override def label = if ( l.notBlank ) l else path.label

  def cell( run:Run, r:Record ) = path.b( r ) |* Glyph.Checkmark
}

case class ExistsField( sec:String, path:Path, l:String = null ) extends PathField {

  override def section = sec

  override def label = if ( l.notBlank ) l else path.label

  def cell( run:Run, r:Record ) = path.s( r ).notBlank |* Glyph.Checkmark
}

case class DateField( sec:String, path:Path, l:String = null ) extends PathField {

  override def section = sec

  override def label = if ( l.notBlank ) l else path.label

  def cell( run:Run, r:Record ) = {
    val date = path.t( r )
    Text( if ( date != null ) date.toDateStr else "" )
  }
}

case class DateTimeField( sec:String, path:Path, l:String = null ) extends PathField {

  override def section = sec

  override def label = if ( l.notBlank ) l else path.label

  def cell( run:Run, r:Record ) = {
    val date = path.t( r )
    Unparsed( "<nobr>" + ( if ( date != null ) date.toDateTimeStr else "" ) + "</nobr>" )
  }
}

case class LinkField( sec:String, path:Path, l:String = null ) extends PathField {

  override def section = sec

  override def label = if ( l.notBlank ) l else path.label

  def cell( run:Run, r:Record ) = {
    val base = path.s( r )

    try {
      <a href={ base.toUrl.toString }>{ base }</a>
    } catch {
    case e:Exception =>
      Text( base )
    }
  }
}

case class ThumbnailField( sec:String, path:Path, l:String = null ) extends PathField {

  override def section = sec

  override def label = if ( l.notBlank ) l else path.label

  def cell( run:Run, r:Record ) = <img src={ path s r } style="width:50px; height:50px;"/>
}

trait MongoQuery extends Query {
  val entity:MongoEntity

  lazy val view = entity.makeView

  override def prepareSearch( run:Run ) = {
    super.prepareSearch( run )

    val report = run.report

    if ( grouping != null ) {
      run.groupFilter match {
      case Some( gf ) => report.search( "_id" ) = Mobj( $in -> gf.a_?( 'ids ) )
      case None       => report.search.remove( "_id" )
      }
    }

    report.search
  }

  def run( run:Run ) = {
    val report = run.report
    var part = entity.db.find( prepareSearch( run ), Mobj() ).limit( 20 );

    if ( report.offset != 0 )
      part = part.skip( report.offset )
    if ( report.sort != null )
      part = part.sort( report.sort )
    
    part.toIterable.map( o => entity.apply( o ) )
  }

  def date( path:String, sec:String = "Standard", label:String = null )        = DateField( sec, view.path( path ), l = label )
  def dateTime( path:String, sec:String = "Standard", label:String = null )    = DateTimeField( sec, view.path( path ), l = label )
  def boolean( path:String, sec:String = "Standard", label:String = null )     = BooleanField( sec, view.path( path ), l = label )
  def exists( path:String, sec:String = "Standard", label:String = null )      = ExistsField( sec, view.path( path ), l = label )
  def string( path:String, sec:String = "Standard", label:String = null )      = StringField( sec, view.path( path ), l = label )
  def multistring( path:String, sec:String = "Standard", label:String = null ) = MultilineStringField( sec, view.path( path ), l = label )
  def link( path:String, sec:String = "Standard", label:String = null )        = LinkField( sec, view.path( path ), l = label )
  def thumbnail( path:String, sec:String = "Standard", label:String = null )   = ThumbnailField( sec, view.path( path ), l = label )
}

case class Report( query:Query ) {

  @volatile var name:String = _

  // TODO:  make this database-agnostic
  val search        = Mobj()
  val searchProps   = mutable.Map[String,String]()


  val parameters    = Mobj()
  @volatile var sort:DBObject = null
  @volatile var offset:Int = 0
  @volatile var pageSize = 20

  @volatile var groupFilter = ""

  @volatile var onSection:String = ""
  @volatile var onField:String = ""

  val hidden  = mutable.ArrayBuffer[Field]()
  val columns = mutable.ArrayBuffer[Field]()

  val selectedColumns = mutable.Set[String]()
  val selectedRows = mutable.Set[AnyRef]()

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

  def extract = {
    val r = T.web.req

    for ( n <- searchProps.keys ) {
      searchProps( n ) match {
      case "bool" =>
        if ( r.b( n ) ) search( n ) = true
        else            search.remove( n )

      case "boolExists" =>
        if ( r.b( n ) ) search( n ) = Mobj( $gt -> "" )
        else            search.remove( n )

      case "textUpper" =>
        val v = r.s( n )

        if ( v.notBlank ) search( n ) = v.toUpperCase
        else              search.remove( n )
        
      case "textUpperSubst" =>
        val v = r.s( n )
        if ( v.notBlank ) search( n ) = Mobj( $regex -> v.toUpperCase )
        else              search.remove( n )

      case "intGte" =>
        val i = r.i( n )

        if ( i == 0 ) search.remove( n )
        else          search( n ) = Mobj( $gte -> i )
      }
    }
  }

  def bool( title:String, attr:String ) = {
    searchProps( attr ) = "bool"
    Checkbox( attr, search.b( attr ) ) ++ label( title, attr )
  }

  def boolExists( title:String, attr:String ) = {
    searchProps( attr ) = "boolExists"
    Checkbox( attr, search.s( attr ).notBlank ) ++ label( title, attr )
  }

  def text( attr:String, opts:(String,String)* ) = {
    searchProps( attr ) = "textUpper"
    Input( attr, search.s( attr ), opts:_* )
  }

  def textUpper( attr:String, width:Int ) = {
    searchProps( attr ) = "textUpper"
    Input( attr, search.s( attr ), "style" -> ( "width:" + width.toString + "px;" ) )
  }

  def textUpperSubst( attr:String, width:Int ) = {
    searchProps( attr ) = "textUpperSubst"
    Input( attr,
           { val regex = search.o( attr )
             regex != null |* regex.s( $regex )
           }, "style" -> ( "width:" + width.toString + "px;" ) )
  }

  def intGte( attr:String ) = {
    searchProps( attr ) = "intGte"
    Input( attr,
           { val o = search.o( attr )

             if ( o == null ) ""
             else             o.i( $gte ).toString },
           "style" -> "width:80px;" )
  }


  /*
   * * *  Searching
   */

  def searchTitle =
    if ( selectedColumns.isEmpty ) Text( "search all fields" )
    else                           Unparsed( "search <span class='hitext'>highlighted</span> fields" )

  val sections = "Standard" +: query.allFields.map( _.section ).filter( _ != "Standard" ).sorted.distinct


  /*
   * * *  Field Dropdowns
   */

  def section = {
    if ( onSection.isBlank )
      onSection = sections( 0 )

    onSection
  }

  def calcFields = query.allFields.filter( f => f.section == section && !columns.contains( f ) ).sortBy( _.label )

  def recalcFields {
    fields = calcFields
    if ( !fields.find( _.name == field ).isDefined )
       onField = ""
  }

  var fields = calcFields

  def field = {
    if ( onField.isBlank )
      onField = if ( fields.size > 0 ) fields( 0 ).name
                else                   ""

    onField
  }

}

case class Run( report:Report, grid:Grid ) {

  def query = grid.query

  val cache = mutable.HashMap[String,AnyRef]()

  val header =
    <tr>
     { query.selectable |* <td></td> }
     { report.columns.map( _.header( this ) ) }
    </tr>

  def row( rec:Record ) = {
    // TODO:  get IDs on these ...{ report.columns.map( p => <th class="colh" id={ p.name_ }>{ col( p ) }</th> ) }
    <tr class={ grid.rowClass }>
     { query.selectable |* 
        <td>{ Unparsed( "<input class='rcb' type=\"checkbox\"" + ( report.selectedRows.contains( rec.id ) |* " checked" ) + "/>" ) }</td> }
     {
      for ( f <- report.columns ) yield {
        val cls = f.cellClass

        if ( cls != null )
          <td class={ cls }>{ f.cell( this, rec ) }</td>
        else
          <td>{ f.cell( this, rec ) }</td>
      }
     }
    </tr>
  }

  lazy val groups = query.grouping.list

  def groupsFor( id:AnyRef ) = groups.filter( _.a_?( 'ids ).contains( id ) ).map( _.s( 'name ) ).mkString( ", " )

  def groupFilter = groups.find( _.s( 'name ) == report.groupFilter )
}

case class Grid( query:Query ) {

  val report = Session().reportFor( query )

  val id = "grid" // TODO:  modify this by session
  var odd = false

  def rowClass = {
    odd = !odd
    !odd |* "even"
  }

  private def initReport:JsCmd = JsCmds.Run( "initReport();" )

  private def redraw:JsCmd =
    SetHtml( id, innerDraw ) &
    initReport


  private def sectionDropdown =
    Select( "gSections", report.section, report.sections.map( sec => sec -> sec ), "style" -> "width:150px; max-width:150px;" )

  private def fieldDropdown =
    if ( report.fields.size == 0 )
      Unparsed( "<select style='width:150px;' disabled><option>none left</option></select>" )
    else
      Select( "gFields", report.field, report.fields.map( f => f.name -> f.label ), "style" -> "width:150px; max-width:150px;" )

  private def addBox:NodeSeq =
    report.fields.find( _.name == report.field ).flatten(
      f => <div id={ f.name } class="cola"><div><span>Add</span></div></div>,
      <div class="colna"><div><span>N/A</span></div></div>
    )


  /*
   * * *  Groups
   */

  private var addGroupName = ""
  private var newGroupName = ""
  private var removeGroupName = ""

  private var selectedRows:Seq[Record] = _
  private var groups:Seq[DBObject] = _

  private def group( rows:Seq[Record] ) = {
    selectedRows = rows.filter( r => report.selectedRows( r.id ) )

    if ( selectedRows.size == 0 ) {
      JsCmds.Run( "alert( 'No rows selected.' );" )
    } else {
      groups = query.grouping.list.toSeq

      addGroupName = ""
      newGroupName = ""
      removeGroupName = ""

      val intersection = groups.filter( g => selectedRows.forall( r => g.a_?( 'ids ).contains( r.id ) ) )
      val union        = groups.filter( g => selectedRows.exists( r => g.a_?( 'ids ).contains( r.id ) ) )

      val addGroup =
        SHtml.ajaxSelect(
          ( "" -> "" ) +: groups.filter( g => !intersection.contains( g ) ).map( g => ( g.s( 'name ), g.s( 'name ) ) ),
          Empty,
          v => {
            addGroupName = v
            Noop
          },
          "id" -> "addGroup_i", "style" -> "width:160px; max-width:160px;" )

      val newGroup = SHtml.ajaxText( "", v => { newGroupName = v; Noop }, "id" -> "newGroup_i", "style" -> "width:120px;" )

      val removeGroup =
        SHtml.ajaxSelect(
          ( "" -> "" ) +: union.filter( !_.b( 'builtin ) ).map( g => ( g.s( 'name ), g.s( 'name ) ) ),
          Empty,
          v => {
            removeGroupName = v
            Noop
          },
          "id" -> "removeGroup_i", "style" -> "width:160px; max-width:160px;" )


      SetHtml( "addGroup", addGroup ) &
      SetHtml( "newGroup", newGroup ) &
      SetHtml( "removeGroup", removeGroup ) &
      JsCmds.Run( "$( '#groupdial' ).dialog( 'open' )" )
    }
  }

  private def doGroup = {
    val grouping = query.grouping

    if ( addGroupName.notBlank ) {
      groups.find( _.s( 'name ) == addGroupName ) foreach { g =>
        g( 'ids ) = Mlist( ( g.a_?( 'ids ) ++ selectedRows.map( _.id ) ).distinct:_* )
        grouping.entity.db.save( g )
      }
    }

    if ( newGroupName.notBlank ) {
      grouping.entity.db.save(
        Mobj(
          "name" -> newGroupName,
           grouping.keyName -> grouping.value(),
           "ids" -> Mlist( selectedRows.map( _.id ):_* ) ) )
    }

    if ( removeGroupName.notBlank ) {
      groups.find( _.s( 'name ) == removeGroupName ) foreach { g =>
        g( 'ids ) = Mlist( g.a_?( 'ids ).filter( id => !selectedRows.exists( _.id == id ) ):_* )
        grouping.entity.db.save( g )
      }
    }

    JsCmds.Run( "$( '#groupdial' ).dialog( 'close' )" ) &
    redraw
  }

  def innerDraw = {
    val run = new Run( report, this )
    val rows = query.run( run ).toSeq

    val selecteds = report.selectedRows.clone
    report.selectedRows.clear
    rows.filter( r => selecteds( r.id ) ).foreach { report.selectedRows += _.id }


    <div class="title"><span>{ query.label }</span></div>
    <table class="def" id="def">
     <tr>
      <td>
       <table class="tile" style="width:338px; height:54px;">
        <tr>
         <td class="label">actions</td>
        </tr>
        <tr> 
         <td style="padding:0;">
          <table>
           <tr>
            { ( query.searchScreen.notBlank |* <td><a href={ query.searchScreen } class="greyBtn">Change Search</a></td> ) ++
              ( report.offset > 0 |* <td><button id="gPrev" class="greyBtn">Prev</button></td> ) ++
              <td><button id="gNext" class="greyBtn">Next</button></td> ++
              ( query.grouping != null |* <td><button id="gGroup" class="greyBtn">Group</button></td> ) }
           </tr>
          </table>
         </td>
        </tr>
       </table>
      </td>
      { query.grouping != null |*
      <td>
       <table class="tile" style="width:140px; height:54px;">
        <tr>
         <td class="label">group</td>
        </tr>
        <tr>
         <td>{ 
           Select( "gGroups", report.groupFilter, ( "" -> "All" ) +: run.groups.map( g => g.s( 'name ) -> g.s( 'name ) ), "style" -> "width:120px; max-width:120px;" )
         }</td>
        </tr>
       </table>
      </td>
      }
      <td style="width:410px; padding:0;">
      </td>
      <td>
       <table class="tile" style="width:226px; height:54px;">
        <tr>
         <td id="searchTitle" class="label">{ report.searchTitle }</td>
        </tr>
        <tr>
         <td>
          <form method="get" class="searchBox" action="/search/results" onsubmit="this.submit();return false;">
	         <fieldset>
            <input type="text" value="" placeholder="Search" class="field"/>
		        <input type="image" class="btn" name="submit" src="/images/search-btn.png" alt="Go"/>
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

function gridselect( id ) {
  $.get('/grid/select', {q:'""" + query.name + """', js:id}, function(d) {
    if ( d != '' )
      $('#searchTitle').html(d);
  });
}

function gridexec( id ) {
  $.get('/grid/exec', {q:'""" + query.name + """', js:id}, function(d) {
    $('#""" + id + """').html(d);
    initReport();
  });
}

$(document).ready(function(){

  $('#""" + id + """').on('click', '#gPrev', function(e){
    $.get('/grid/prev', { q:'""" + query.name + """' }, function(d) {
      $('#""" + id + """').html(d);
      initReport();
    })
  });

  $('#""" + id + """').on('click', '#gNext', function(e){
    $.get('/grid/next', { q:'""" + query.name + """' }, function(d) {
      $('#""" + id + """').html(d);
      initReport();
    })
  });

  $('#""" + id + """').on('click', '#gGroup', function(e){
    // invoke group( rows )
  });

  $('#""" + id + """').on('onChange', '#gGroups', function(e){
    // report.groupFilter = value of select
    // redraw
  });

  $('#""" + id + """').on('click', '#gGroupOkay', function(e){
    // invoke doGroup
  });

  $( "#groupdial" ).dialog({
    autoOpen:false,
    title:"Groups",
    modal:true,
    resizable:false,
    width:360
  });

  $('#""" + id + """').on('onChange', '#gSections', function(e) {
    // report.onSection = v
    // report.recalcFields
    // SetHtml( "repfd", fieldDropdown ) &
    // SetHtml( "repab", addBox ) &
    // initReport
  });

  $('#""" + id + """').on('onChange', '#gSections', function(e) {
    // report.onField = v
    // SetHtml( "repab", addBox ) &
    // initReport
  });

  $('#""" + id + """').on('onChange', '.rcb', function(e) {
    // <td>{ SHtml.ajaxCheckbox( report.selectedRows.contains( rec.id ), (v:Boolean) => { report.selectedRows( rec.id ) = v; Noop } ) }</td> }
  });
});


""" ) }</script>
    </head> ++
    { query.grouping != null |*
      <div id="groupdial" style="padding:8px; display:none;">
       <table>
        <tr>
         <td style="width:160px;"><label for="addGroup_i">Add to Existing Group</label></td>
         <td id="addGroup"/>
        </tr>
        <tr>
         <td><label for="newGroup_i">Add to New Group</label></td>
         <td id="newGroup"/>
        </tr>
        <tr>
         <td><label for="removeGroup_i">Remove Group</label></td>
         <td id="removeGroup"/>
        </tr>
        <tr>
         <td style="padding:8px 0 0;">
          <table>
           <tr>
            <td><button onclick="$('#groupdial').dialog('close'); return false;" class="greyBtn">Cancel</button></td>
            <td style="padding-left:8px;"><button id="gGroupOkay" class="greenBtn">Okay></button></td>
           </tr>
          </table>
         </td>
        </tr>
       </table>
      </div>
    } ++
    <div class="report" id={ id }>
    { innerDraw }
    </div>
  }
}


object Gridlet extends Weblet {

  def handle( web:WebContext ) {
    val sess = Session()

    web.path match {

    /*
     * * *  Navigation
     */

    case "/grid/prev" =>
      val report = sess.reportFor( web.req.s( 'q ) )
      report.offset -= report.pageSize
      if ( report.offset < 0 ) report.offset = 0
      web.res.html( new Grid( report.query ).innerDraw )

    case "/grid/next" =>
      val report = sess.reportFor( web.req.s( 'q ) )
      report.offset += report.pageSize
      web.res.html( new Grid( report.query ).innerDraw )

    case "/grid/select" =>
      val report = sess.reportFor( web.req.s( 'q ) )
      val js = web.req.s( 'js )
      val query = report.query
      val fp = query.by( js.substring(1) )

      val empty = report.selectedColumns.isEmpty
      report.selectedColumns( fp.name ) = !report.selectedColumns( fp.name )

      web.res.html(
        empty != report.selectedColumns.isEmpty |* report.searchTitle )

    case "/grid/exec" =>
      val report = sess.reportFor( web.req.s( 'q ) )
      val query = report.query
      val js = web.req.s( 'js )
      val ( fn, tn ) = js.splitFirst( ':' )

      val fp = query.by( fn )

      if ( tn == "def" ) {
        if ( report.selectedColumns( fn ) ) {
          report.selectedColumns.foreach { n => report.remove( query.by( n ) ) }
          report.selectedColumns.clear
        } else {
          report.remove( fp )
        }
      } else if ( tn == "_end" ) {
        report.add( fp )
      } else {
        val tp = query.by( tn )

        if ( report.selectedColumns( fn ) && !report.selectedColumns( tn ) ) {
          val columns = report.columns.filter( f => report.selectedColumns( f.name ) )

          for ( c <- columns )
            report.insertBefore( insert = c, before = tp )

          //TODO:  if selections, insert everything before insert ... (if insert is selected, don't move it)
        } else {
          report.insertBefore( insert = fp, before = tp )
        }
      }

      report.recalcFields
      web.res.html( new Grid( report.query ).innerDraw )
    }
  }
}


