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

package org.tyranid.report

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.xml.{ NodeSeq, Text, Unparsed }

import org.bson.types.ObjectId
import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.db.{ Entity, Path, Record, Scope, ViewAttribute }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.MongoEntity
import org.tyranid.session.Session
import org.tyranid.ui.{ Button, Checkbox, Field, Glyph, Input, PathField, Search, Select }
import org.tyranid.web.{ Weblet, WebContext }


/*
 * * *   S o r t
 */

case class Sort( name:String, label:String, direction:Int ) {

  lazy val selectObj = ( name, label )
  lazy val sortObj   = Mobj( name -> direction )
}


/*
 * * *   G r o u p
 */

case class Grouping( entity:MongoEntity, nameKey:String, listKey:String, value: () => AnyRef ) {

  def list = entity.db.find( Mobj( nameKey -> value() ) ).toSeq

  def drawFilter( run:Run ) =
    <table class="tile" style="width:140px; height:54px;">
     <tr>
      <td class="label">view group</td>
     </tr>
     <tr>
      <td>{ 
        Select( "rGroups", run.report.groupFilter, ( "" -> "All" ) +: run.report.groups.map( g => g.s( 'name ) -> g.s( 'name ) ), "style" -> "width:120px; max-width:120px;" )
      }</td>
     </tr>
    </table>

  def prepareSearch( search:DBObject, run:Run ) =
    run.report.groupFilterObj foreach { gf =>
      search( "_id" ) = Mobj( $in -> gf.a_?( 'ids ) )
    }

  def draw( report:Report ) =
    <div id="rGroupDlg" style="padding:0; display:none;">
    { dialogContents( report ) }
    </div>

  def drawGroup( grouping:Grouping, group:DBObject ) = {



  }

  def dialogContents( report:Report ) = {
    <div class="left" style="float:left;">
     <select multiple="multiple" style="width:180px; max-width:180px; height:500px;">
      <option value="">All</option>
      { report.groups.map( g => <option value={ g.s( 'name ) }>{ g.s( 'name ) }</option> ) }
     </select>
    </div>
    <div>
     <div class="btns">
      <button onclick="$('#rGroupDlg').dialog('close'); return false;" class="greyBtn">Cancel</button>
      <button id="rGroupOkay" class="greenBtn">Okay</button>
     </div>
    </div>
  }

  def handle( weblet:Weblet, report:Report ):Boolean = {
    val web = T.web
    val query = report.query

    weblet.rpath match {
    case "/groups" =>
      report.groupFilter = web.req.s( 'gn )
      web.res.html( report.innerDraw )

    case "/group" =>
      report.resetGroups

      report.addGroupName = ""
      report.newGroupName = ""
      report.removeGroupName = ""

      web.res.html( dialogContents( report ) )

    case _ =>
      return false
    }

    true
  }
}


/*
 * * *   Q u e r y
 */

object Query {

  val byName = mutable.Map[String,Query]()
}

trait Query {

  val name:String
  val entity:Entity

  def label:AnyRef            = "Search Results" //name.camelCaseToSpaceUpper
  def labelNode:NodeSeq       = null

  def searchLabel:AnyRef      = "Search"
  def searchLabelNode:NodeSeq = labelNode

  val allFields:Seq[Field]

  val boundFields:Seq[Field]

  lazy val dataFields:Seq[Field]   = boundFields.filter( _.data )
  lazy val searchFields:Seq[Field] = boundFields.filter( _.search != null )

  lazy val allFieldsMap = {  
    val map = mutable.Map[String,Field]()
    for ( f <- allFields ) {
      if ( f.name.isBlank )
        throw new RuntimeException( "Field missing name in report: " + f.toString )

      if ( map.contains( f.name ) )
        throw new RuntimeException( "Duplicate field in report: " + f.name + " ... if the same field is in two searches, make sure data = true only one of them." )

      map( f.name ) = f
      if ( f.name != f.baseName )
        map( f.baseName ) = f
    }
    map
  }

  val defaultFields:Seq[Field]

  def prepareSearch( run:Run ) = {

    val rep = run.report
    val search = Mobj()

    for ( sf <- searchFields;
          value = rep.searchRec( sf.name )
          if value != null )
      sf.prepareSearch( run, search, value )

    val textSearches =
      rep.selectedColumns.
        toSeq.
        flatMap( name => dataFields.find( _.name == name ) ).
        flatMap( _.textSearch( run ) )

    textSearches.size match {
    case 0 =>
    case 1 => search.copy( textSearches( 0 ) )
    case n => search( $or ) = Mlist( textSearches:_* )
    }

    search
  }

  def run( run:Run ):Iterable[Record]

  def newReport = {
    var r = Report( this )
    r.columns ++= defaultFields
    r.hidden ++= dataFields.filter( p => !r.columns.contains( p ) ).sortBy( _.label )
    r
  }
  
  def by( name:String ) = dataFields.find( _.name == name ).get

  val tableGridStyle:String = null

  def selectable = false

  //val actions = Seq(
    //"Group",          // multi-select
    //"Connection"      // multi-select
  //)

  val grouping:Grouping = null

  lazy val init =
    Query.byName( name ) = this

  def draw = {
    Query.synchronized { init }
    Session().reportFor( this.name ).draw
  }
  
  def extraActions:NodeSeq = Text( "" )

  val orderBy:Seq[Sort] = Nil

  val searchForm =
   ( r:Report ) => {
     val s = Scope( r.searchRec )

   <form method="post" id="rSearchForm" style="padding-top:8px;">
     { searchFields.nonEmpty |*
     <div class="fieldsc" style="margin-top:8px; padding:4px;">
      <h3>Search By</h3>
      { searchFields map { f =>
          <div class="fieldc">
           <div class="labelc">{ f.labelUi }</div>
           <div class="inputc">{ f.ui( s ) }</div>
          </div>
        }
      }
     </div> }
     { orderBy.nonEmpty |*
     <div class="fieldsc" style="margin-top:8px; padding:4px;">
      <h3>Order By</h3>
      <div>{ Select( "sort", r.sort != null |* r.sort.keySet.head, orderBy.map( _.selectObj ) ) }</div>
     </div> }
    <div class="btns">
     <input type="submit" value="Search" class="greenBtn" name="saving"/>
    </div>
   </form>
 }

  def hasSearch = searchFields.nonEmpty || orderBy.nonEmpty
}

trait MongoQuery extends Query {
  val entity:MongoEntity

  lazy val view = entity.makeView

  lazy val boundFields = {
    for ( f <- allFields )
      f match {
      case pf:PathField =>
        pf.bind( view )

      case _ =>
      }

    allFields
  }
  
  override def prepareSearch( run:Run ) = {
    val report = run.report

    val search = super.prepareSearch( run )

    if ( grouping != null )
      grouping.prepareSearch( search, run )

    search
  }

  def run( run:Run ) = {
    val report = run.report

    val search = prepareSearch( run )
spam( "search=" + search )
spam( "sort=" + report.sort )
spam( "skip=" + report.offset )
spam( "pageSize=" + report.pageSize )

    var cursor = entity.db.find( search )//, Mobj() )
    if ( report.offset != 0 )
      cursor = cursor.skip( report.offset )
    cursor = cursor.limit( run.report.pageSize + 1 )

    if ( report.sort != null )
      cursor = cursor.sort( report.sort )
    
    val rows = cursor.toIterable.map( entity.apply )

    report.hasNext = rows.size > run.report.pageSize

    rows.take( run.report.pageSize )
  }
}



/*
 * * *  R e p o r t
 */

case class Report( query:Query ) {

  val id = "_report" // TODO:  modify this by session

  @volatile var name:String = _

  val searchRec = query.entity.as[MongoEntity].make

  @volatile var sort:DBObject = {

    if ( query.orderBy.nonEmpty )
      query.orderBy.head.sortObj
    else
      null
  }

  @volatile var offset:Int = 0
  @volatile var hasNext:Boolean = false
  @volatile var pageSize = 20

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


  /*
   * * *  Searching
   */

  def label( name:String ) = query.allFieldsMap( name ).labelUi
  def ui( name:String )    = query.allFieldsMap( name ).ui( Scope( searchRec ) )

  def searchTitle =
    if ( selectedColumns.isEmpty ) Text( "search all fields" )
    else                           Unparsed( "search <span class='hitext'>highlighted</span> fields" )

  val sections = "Standard" +: query.dataFields.map( _.section ).filter( _ != "Standard" ).sorted.distinct

  var textSearchValue:String = ""


  /*
   * * *  Field Dropdowns
   */

  def section = {
    if ( onSection.isBlank )
      onSection = sections( 0 )

    onSection
  }

  def calcFields = query.dataFields.filter( f => f.section == section && !columns.contains( f ) ).sortBy( _.label )

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
   * * *  Selection
   */

  var records:Seq[Record] = _
  var selectedRecords:Seq[Record] = _


  /*
   * * *  Groups
   */

  @volatile var groupFilter = ""

  var addGroupName = ""
  var newGroupName = ""
  var removeGroupName = ""

  private var latestGroups:Seq[DBObject] = null

  def resetGroups { latestGroups = null }
  def groups = {
    if ( latestGroups == null )
      latestGroups = query.grouping.list.toSeq

    latestGroups
  }

  def groupsFor( id:AnyRef ) = groups.filter( _.a_?( 'ids ).contains( id ) ).map( _.s( 'name ) ).mkString( ", " )
  def groupFilterObj = groups.find( _.s( 'name ) == groupFilter )


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
            { ( query.hasSearch        |* <td><button id="rSearch" class="greyBtn">Search</button></td> ) ++
                                          <td>{ Button.btn( "rPrev", "Prev", disabled = offset == 0 ) }</td> ++
                                          <td>{ Button.btn( "rNext", "Next", disabled = !hasNext ) }</td> ++
              ( query.grouping != null && !B.PRODUCTION |* <td><button id="rGroup" class="greyBtn">Group</button></td> ) }
            { query.extraActions } 
           </tr>
          </table>
         </td>
        </tr>
       </table>
      </td>
      { query.grouping != null |* <td>{ query.grouping.drawFilter( run ) }</td> }
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
          <form method="get" id="rTextSearchForm" class="searchBox">
	         <div>
            <input type="text" value={ textSearchValue } id="rTextSearch" name="rTextSearch" placeholder="Search" class="field"/>
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

  def innerDrawSearch =
    ( if ( query.searchLabelNode != null ) {
      query.searchLabelNode
    } else {
      <div class="title">{ query.searchLabel }</div>
    } ) ++
    <div class="search">
     { query.searchForm( this ) }
    </div>

  def draw =
    <head>
     <script src={ B.buildPrefix + "/js/report.js" } type="text/javascript"/>
     <script>{ Unparsed( "window.reportObj = { qn:'" + query.name + "', id:'" + id + "' };" ) }</script>
    </head> ++
    { query.grouping != null |* query.grouping.draw( this ) } ++
    <div class="report greyBox" id={ id }>
     { recalcFields }
     { innerDraw }
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

  def row( rec:Record ) = {
    val s = Scope( rec, run = this )

    <tr id={ rec.id.toString } class={ rowClass }>
     { query.selectable |* 
        <td>{ Unparsed( "<input class='rcb' type=\"checkbox\"" + ( report.selectedIds.contains( rec.id ) |* " checked" ) + "/>" ) }</td> }
     {
      for ( f <- report.columns ) yield {
        val cls = f.cellClass

        if ( cls != null )
          <td class={ cls }>{ f.effCell( s ) }</td>
        else
          <td>{ f.effCell( s ) }</td>
      }
     }
    </tr>
  }
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

    case "/textSearch" =>
      report.textSearchValue = web.req.s( 'ts )
      web.res.html( report.innerDraw )

    case "/editSearch" =>
      web.res.html( report.innerDrawSearch )

    case "/search" =>
      query.init

      val s = Scope( report.searchRec )
      report.searchRec.clear
      query.searchFields.foreach { _.extract( s ) }

      if ( query.orderBy.nonEmpty ) {
        val name = web.req.s( 'sort )
        report.sort = query.orderBy.find( _.name == name ).get.sortObj
      }
      report.offset = 0
      web.res.html( report.innerDraw )

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

    case _ if query.grouping != null && query.grouping.handle( this, report ) =>

    case _ =>
      _404
    }
  }
}

