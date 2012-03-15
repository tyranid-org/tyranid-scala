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

import org.bson.types.ObjectId
import com.mongodb.DBObject

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

object Query {

  val byName = mutable.Map[String,Query]()

}

trait Query {

  val name:String

  def label:AnyRef = name.camelCaseToSpaceUpper
  def labelNode:NodeSeq = null
  
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
  val tableGridStyle:String = null

  def selectable = grouping != null

  //val actions = Seq(
    //"Group",          // multi-select
    //"Connection"      // multi-select
  //)

  val grouping:Grouping = null

  lazy val init = {
    Query.byName( name ) = this
  }

  def draw = {
    Query.synchronized { init }
    Session().reportFor( this.name ).draw
  }
  
  def extraActions: NodeSeq = Text("")
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
case class StringField( sec:String, path:Path, l:String = null, cellCls:String = null ) extends PathField {

  override def section = sec
  override def cellClass = cellCls
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
  def string( path:String, sec:String = "Standard", label:String = null, cellClass:String = null )      = StringField( sec, view.path( path ), l = label, cellCls = cellClass )
  def multistring( path:String, sec:String = "Standard", label:String = null ) = MultilineStringField( sec, view.path( path ), l = label )
  def link( path:String, sec:String = "Standard", label:String = null )        = LinkField( sec, view.path( path ), l = label )
  def thumbnail( path:String, sec:String = "Standard", label:String = null )   = ThumbnailField( sec, view.path( path ), l = label )
}

case class Report( query:Query ) {

  val id = "_report" // TODO:  modify this by session

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
  val selectedIds = mutable.Set[AnyRef]()

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

      case "text" =>
        val v = r.s( n )

        if ( v.notBlank ) search( n ) = v
        else              search.remove( n )
        
      case "textParam" =>
        val v = r.s( n )

        if ( v.notBlank ) parameters( n ) = v
        else              parameters.remove( n )
        
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
    searchProps( attr ) = "text"
    Input( attr, search.s( attr ), opts:_* )
  }

  def textParam( attr:String, opts:(String,String)* ) = {
    searchProps( attr ) = "textParam"
    Input( attr, parameters.s( attr ), opts:_* )
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

  def sectionDropdown =
    Select( "rSections", section, sections.map( sec => sec -> sec ), "style" -> "width:150px; max-width:150px;" )

  def fieldDropdown =
    if ( fields.size == 0 )
      Unparsed( "<select style='width:150px;' disabled><option>none left</option></select>" )
    else
      Select( "rFields", field, fields.map( f => f.name -> f.label ), "style" -> "width:150px; max-width:150px;" )

  def addBox:NodeSeq =
    if ( field.notBlank )
      <div id="_add" class="cola"><div><span>Add</span></div></div>
    else
      <div id="_add" class="colna"><div><span>N/A</span></div></div>


  /*
   * * *  Groups
   */

  var addGroupName = ""
  var newGroupName = ""
  var removeGroupName = ""

  var records:Seq[Record] = _
  var selectedRecords:Seq[Record] = _
  var groups:Seq[DBObject] = _


  /*
   * * *  Rendering
   */

  def innerDraw = {
    val run = new Run( this )
    records = query.run( run ).toSeq

    val tmp = selectedIds.clone
    selectedIds.clear
    records.filter( r => tmp( r.id ) ).foreach { selectedIds += _.id }

    ( if ( query.labelNode != null ) {
      query.labelNode
    } else {
      <div class="title">{ query.label }</div>
    } ) ++
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
              ( offset > 0 |* <td><button id="rPrev" class="greyBtn">Prev</button></td> ) ++
              <td><button id="rNext" class="greyBtn">Next</button></td> ++
              ( query.grouping != null |* <td><button id="rGroup" class="greyBtn">Group</button></td> ) }
            { query.extraActions } 
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
           Select( "rGroups", groupFilter, ( "" -> "All" ) +: run.groups.map( g => g.s( 'name ) -> g.s( 'name ) ), "style" -> "width:120px; max-width:120px;" )
         }</td>
        </tr>
       </table>
      </td>
      }
      <td style="width:410px; padding:0;">
      </td>
      <td>
      { if ( !B.PRODUCTION )
       <table class="tile" style="width:226px; height:54px;">
        <tr>
         <td id="searchTitle" class="label">{ searchTitle }</td>
        </tr>
        <tr>
         <td>
          <form method="get" class="searchBox" action="/search/results" onsubmit="this.submit();return false;">
	         <div>
            <input type="text" value="" placeholder="Search" class="field"/>
		        <input type="image" class="btn" name="submit" src="/images/search-btn.png" alt="Go"/>
	         </div>
          </form>
         </td>
        </tr>
       </table>
      }
      </td>
      <td>
       <table class="tile" style="width:298px; height:54px;">
        <tr>
         <td class="label">section</td>
         <td style="width:160px;">{ sectionDropdown }</td>
         <td rowspan="2" id="gab" style="width:130px;">{ addBox }</td>
        </tr>
        <tr>
         <td class="label">field</td>
         <td id="gfd" style="width:160px;">{ fieldDropdown }</td>
        </tr>
       </table>
      </td>
     </tr>
    </table> ++
    <div class="grid">
     <table style={ query.tableGridStyle }>
      <thead>
       { run.header }
      </thead>
      <tbody>
       { records.map( r => run.row( r ) ) }
      </tbody>
     </table>
    </div>
  }

  def draw =
    <head>
     <script src={ B.buildPrefix + "/js/report.js" } type="text/javascript"/>
     <script>{ Unparsed( "window.reportObj = { qn:'" + query.name + "', id:'" + id + "' };" ) }</script>
    </head> ++
    { query.grouping != null |*
      <div id="rGroupDlg" style="padding:8px; display:none;">
       <table id="rGroupDlg_c">
        <tr>
         <td style="width:160px;"><label for="rGroupAdd">Add to Existing Group</label></td>
         <td/>
        </tr>
        <tr>
         <td><label for="rGroupNew">Add to New Group</label></td>
         <td/>
        </tr>
        <tr>
         <td><label for="rGroupRemove">Remove Group</label></td>
         <td/>
        </tr>
       </table>
       <div class="btns">
        <button onclick="$('#rGroupDlg').dialog('close'); return false;" class="greyBtn">Cancel</button>
        <button id="rGroupOkay" class="greenBtn">Okay</button>
       </div>
      </div>
    } ++
    <div class="report greyBox" id={ id }>
    { recalcFields
      innerDraw }
    </div>
}

case class Run( report:Report ) {

  def query = report.query

  var odd = false
  val cache = mutable.HashMap[String,AnyRef]()

  def rowClass = {
    odd = !odd
    !odd |* "even"
  }

  val header =
    <tr>
     { query.selectable |* <td></td> }
     { report.columns.map( _.header( this ) ) }
    </tr>

  def row( rec:Record ) =
    <tr id={ rec.id.toString } class={ rowClass }>
     { query.selectable |* 
        <td>{ Unparsed( "<input class='rcb' type=\"checkbox\"" + ( report.selectedIds.contains( rec.id ) |* " checked" ) + "/>" ) }</td> }
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

  lazy val groups = query.grouping.list

  def groupsFor( id:AnyRef ) = groups.filter( _.a_?( 'ids ).contains( id ) ).map( _.s( 'name ) ).mkString( ", " )

  def groupFilter = groups.find( _.s( 'name ) == report.groupFilter )
}

object Reportlet extends Weblet {

  def handle( web:WebContext ) {
    redirectIfNotLoggedIn( web )
    val sess = Session()
    val report = sess.reportFor( web.req.s( 'q ) )
    val query = report.query

    rpath match {

    /*
     * * *  Navigation
     */

    case "/prev" =>
      report.offset -= report.pageSize
      if ( report.offset < 0 ) report.offset = 0
      web.res.html( report.innerDraw )

    case "/next" =>
      report.offset += report.pageSize
      web.res.html( report.innerDraw )

    case "/select" =>
      val fp = query.by( web.req.s( 'f ) )

      val empty = report.selectedColumns.isEmpty
      report.selectedColumns( fp.name ) = !report.selectedColumns( fp.name )

      web.res.html(
        empty != report.selectedColumns.isEmpty |* report.searchTitle )

    case "/drag" =>
      val js = web.req.s( 'js )
      val ( fn, tn ) = js.splitFirst( ':' )

      val efn =
        if ( fn == "_add" ) report.onField
        else                fn

      val fp = query.by( efn )

      if ( tn == "def" ) {
        if ( report.selectedColumns( efn ) ) {
          report.selectedColumns.foreach { n => report.remove( query.by( n ) ) }
          report.selectedColumns.clear
        } else {
          report.remove( fp )
        }
      } else if ( tn == "_end" ) {
        report.add( fp )
      } else {
        val tp = query.by( tn )

        if ( report.selectedColumns( efn ) && !report.selectedColumns( tn ) ) {
          val columns = report.columns.filter( f => report.selectedColumns( f.name ) )

          for ( c <- columns )
            report.insertBefore( insert = c, before = tp )

          //TODO:  if selections, insert everything before insert ... (if insert is selected, don't move it)
        } else {
          report.insertBefore( insert = fp, before = tp )
        }
      }

      report.recalcFields
      web.res.html( report.innerDraw )

    case "/selectRow" =>
      // TODO:  make this generic based on query's entity id field
      val rowId = new ObjectId( web.req.s( 'id ) )
      if ( report.selectedIds( rowId ) )
        report.selectedIds -= rowId
      else
        report.selectedIds += rowId

      web.res.ok

    case "/section" =>
      report.onSection = web.req.s( 'v )
      report.recalcFields
      web.res.json(
        Map(
          "gfd" -> report.fieldDropdown.toString,
          "gab" -> report.addBox.toString
        )
      )

    case "/field" =>
      report.onField = web.req.s( 'v )
      web.res.ok

    case "/groups" =>
      report.groupFilter = web.req.s( 'gn )
      web.res.html( report.innerDraw )

    case "/group" =>
      report.selectedRecords = report.records.filter( r => report.selectedIds( r.id ) )

      if ( report.selectedRecords.size == 0 ) {
        web.res.html( NodeSeq.Empty )
      } else {
        report.groups = query.grouping.list.toSeq

        report.addGroupName = ""
        report.newGroupName = ""
        report.removeGroupName = ""

        val intersection = report.groups.filter( g => report.selectedRecords.forall( r => g.a_?( 'ids ).contains( r.id ) ) )
        val union        = report.groups.filter( g => report.selectedRecords.exists( r => g.a_?( 'ids ).contains( r.id ) ) )

        web.res.html(
          <tr>
           <td style="width:160px;"><label for="rGroupAdd">Add to Existing Group</label></td>
           <td>
            { Select( "rGroupAdd", "", ( "" -> "" ) +: report.groups.filter( g => !intersection.contains( g ) ).map( g => ( g.s( 'name ), g.s( 'name ) ) ), "style" -> "width:160px; max-width:160px;" ) }
           </td>
          </tr> ++
          <tr>
           <td><label for="rGroupNew">Add to New Group</label></td>
           <td>
            { Input( "rGroupNew", "", "style" -> "width:120px;" ) }
           </td>
          </tr>
          <tr> ++
           <td><label for="rGroupRemove">Remove Group</label></td>
           <td>
            { Select( "rGroupRemove", "", ( "" -> "" ) +: union.filter( !_.b( 'builtin ) ).map( g => ( g.s( 'name ), g.s( 'name ) ) ), "style" -> "width:160px; max-width:160px;" ) }
           </td>
          </tr> )
      }

    case "/group/add" =>
      report.addGroupName = web.req.s( 'v )
      web.res.ok

    case "/group/new" =>
      report.newGroupName = web.req.s( 'v )
      web.res.ok

    case "/group/remove" =>
      report.removeGroupName = web.req.s( 'v )
      web.res.ok

    case "/group/okay" =>
      val grouping = query.grouping

      if ( report.addGroupName.notBlank ) {
        report.groups.find( _.s( 'name ) == report.addGroupName ) foreach { g =>
          g( 'ids ) = Mlist( ( g.a_?( 'ids ) ++ report.selectedIds ).distinct:_* )
          grouping.entity.db.save( g )
        }
      }

      if ( report.newGroupName.notBlank ) {
        grouping.entity.db.save(
          Mobj(
            "name" -> report.newGroupName,
             grouping.keyName -> grouping.value(),
             "ids" -> Mlist( report.selectedIds.toSeq:_* ) ) )
      }

      if ( report.removeGroupName.notBlank ) {
        report.groups.find( _.s( 'name ) == report.removeGroupName ) foreach { g =>
          g( 'ids ) = Mlist( g.a_?( 'ids ).filter( id => !report.selectedIds.exists( _ == id ) ):_* )
          grouping.entity.db.save( g )
        }
      }

      web.res.html( report.innerDraw )
    }
  }
}
