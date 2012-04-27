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
import org.tyranid.collection.ConcurrentExpireAutoMap
import org.tyranid.db.{ DbArray, DbLink, DbTextLike, Entity, Path, Record, Scope, ViewAttribute }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.MongoEntity
import org.tyranid.db.ram.RamEntity
import org.tyranid.json.{ Js, JqHtml }
import org.tyranid.profile.{ GroupValue, GroupField, GroupingAddBy }
import org.tyranid.session.Session
import org.tyranid.time.Time
import org.tyranid.ui.{ Button, Checkbox, CustomField, Field, Glyph, Input, PathField, Search, Select, Show }
import org.tyranid.web.{ Weblet, WebContext }


/*
 * * *   S o r t
 */

case class Sort( name:String, label:String, fields:(String,Int)* ) {

  lazy val selectObj = ( name, label )
  lazy val sortObj   = Mobj( fields:_* )

  lazy val comparator = ( r1, r2 ) => lessThan( r1, r2 )

  def lessThan( r1:Record, r2:Record ):Boolean = {

    for ( f <- fields ) {
      val va = r1.view( f._1 )

      if ( va.domain.compare( r1( va ), r2( va ) ) < 0 )
        return true
    }

    false
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

  lazy val view = entity.makeView

  lazy val boundFields = {
    val allFields = fields

    for ( f <- allFields )
      f match {
      case pf:PathField =>
        pf.bind( view )

      case _ =>
      }

    allFields
  }
  
  def label:AnyRef            = "Search Results" //name.camelCaseToSpaceUpper
  def labelNode:NodeSeq       = null

  def searchLabel:AnyRef      = "Search"
  def searchLabelNode:NodeSeq = labelNode

  val fields:Seq[Field]

  lazy val dataFields:Seq[Field]   = boundFields.filter( f => f.data && f.show != Show.Hidden )
  lazy val searchFields:Seq[Field] = boundFields.filter( _.search != null )
  lazy val groupFields:Seq[Field]  = boundFields.filter( _.isInstanceOf[GroupField] )

  lazy val allFieldsMap = {  
    val map = mutable.Map[String,Field]()
    for ( f <- boundFields ) {
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

  def run( run:Run ):Iterable[Record] = {
    val report = run.report

    val rows = entity.query( run, report.offset, report.pageSize + 1, report.sort )

    report.hasNext = rows.size > report.pageSize

    rows.take( report.pageSize )
  }

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

  lazy val init =
    Query.byName( name ) = this

  def draw = {
    Query.synchronized { init }
    Session().reportFor( this.name ).draw
  }
  
  def extraActions:NodeSeq = Text( "" )
  val actionsStyle = "width:338px; height:54px;"
  val sectionStyle = "width:298px; height:54px;"

  val orderBy:Seq[Sort] = Nil

  val searchForm =
   ( r:Report ) => {
     val s = Scope( r.searchRec )

   <form method="post" id="rSearchForm" style="padding-top:8px;">
     { searchFields.nonEmpty |*
     <div class="fieldsc" style="margin-top:8px; padding:4px;">
      <h3>Search By</h3>
      { searchFields.filter( _.show == Show.Editable ).map { f =>
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
      <div>{ Select( "sort", r.sort != null |* r.sort.name, orderBy.map( _.selectObj ) ) }</div>
     </div> }
    <div class="btns">
     <input type="submit" value="Search" class="greenBtn" name="saving"/>
    </div>
   </form>
 }

  def hasSearch = searchFields.nonEmpty || orderBy.nonEmpty
}

object AutoQuery {

  val byEntity = new ConcurrentExpireAutoMap( 4 * Time.OneHourMs, ( en:Entity ) => AutoQuery( en ) )
}

case class AutoQuery( entity:Entity ) extends Query {

  val name = "auto" + entity.name.capitalize

  val fields = {
    /*
        TODO:  search properties are dynamically added to Views 
               this works for MongoEntities ... but not for the Tuple-based RamEntities

               if this entity is a Tuple entity, and we're adding search terms that are not ( data = true ), we will need to dynamically create
               the TupleView for this query before adding in these fields

     */

    ( new CustomField {
      def name = "tid"
      override lazy val label = "TID"
      override def cell( s:Scope ) = <a href={ "/admin/tid?tid=" + s.rec.tid } class="eyeBtn" style="margin:0 1px;">T</a>
    } ) +:
    ( entity.makeView.vas.filter( _.att.isLabel ).map( fieldFor ).toSeq.sortBy( _.label ) ++
      entity.makeView.vas.filter( va => !va.att.isLabel && !va.domain.isInstanceOf[Entity] ).map( fieldFor ).toSeq.sortBy( _.label ) )
  }

  def fieldFor( va:ViewAttribute ) =
    new PathField(
      va.name,
      search =
        va.domain match {
        case t:DbTextLike => Search.Subst
        case _            => Search.Equals
        }
    ).bind( va.view )

  val defaultFields = dataFields.take( 8 )

  override val orderBy = Seq( entity.defaultSort, Sort( "id", "Increasing ID", "id" -> 1 ) )
}



/*
 * * *  R e p o r t
 */

case class Report( query:Query ) {

  val id = "_report" // TODO:  modify this by session

  @volatile var name:String = _

  val searchRec = {
    val rec = query.entity.make
    for ( sf <- query.searchFields ) {
      sf match {
      case gf:GroupField =>
        val gd = GroupValue( this, gf )
        sf.default foreach { d => gd.selectedGroupTid = d()._s }
        rec( sf.name ) = gd

      case _ =>
        sf.default foreach { d => rec( sf.name ) = d() }
      }
    }

    rec 
  }

  def extractSearchRec = {
    for ( sf <- query.searchFields )
      sf match {
      case gf:GroupField => searchRec( sf.name ).as[GroupValue].selectedGroupTid = ""
      case _             => searchRec.remove( sf.name )
      }

    val s = Scope( searchRec )
    query.searchFields.foreach { _.extract( s ) }

    for ( sf <- query.searchFields )
      if ( sf.show != Show.Editable )
        sf.default foreach { d => searchRec( sf.name ) = d() }
  }

  @volatile var sort:Sort = {

    if ( query.orderBy.nonEmpty )
      query.orderBy.head
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

  val sections = "Standard" +: query.dataFields.map( _.section ).filter( _ != "Standard" ).sorted.distinct


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
   * * *  Rendering
   */

  def drawFilter( run:Run, sf:Field ) = {
    val s = Scope( searchRec, filtering = true )

    <table id={ sf.id } class="tile" style="width:344px; height:54px;">
     <tr>
      <td class="label">{ sf.label }</td>
      { sf.topActions( run ) }
     </tr>
     <tr>
      <td id="rGrpChooser">
       { sf.ui( s ) }
       { sf.bottomActions( run ) }
      </td>
     </tr>
    </table>
  }

  def groupValueFor( gf:Field ) = searchRec( gf.name ).as[GroupValue]

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
       <table class="tile" style={ query.actionsStyle }>
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
              ( query.groupFields.nonEmpty |* <td><button id="rGroup" class="greyBtn">Group</button></td> ) }
            { query.extraActions } 
           </tr>
          </table>
         </td>
        </tr>
       </table>
      </td>
      { query.groupFields.map( gf => <td>{ groupValueFor( gf ).drawFilter }</td> ) }
      { query.searchFields.filter( _.showFilter ).map( f => <td>{ drawFilter( run, f ) }</td> ) }
      <td style="width:410px; padding:0;"></td>
      <td></td>
      <td>
       <table class="tile" style={ query.sectionStyle }> 
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
    { // TODO:  This needs to be able to pass in a JS function to make a callback to the report JS to determine which column the label field is in.  The
      //        handler function is "alphaFilter" in main.js.  This probably needs to pass in another class to be stylized since it is on a different 
      //        type of table (than the types with filesharing).  Also, this is a JS filter, so it does it in the web browser.  In this case, the callback
      //        might need to be a server callback that can do the filtering since there may be more than one result page (pagination)
      //org.tyranid.ui.TableAlphaFilter( "grid_" + query.name, 0 ).draw
      NodeSeq.Empty
    } ++
    <div class="grid">
     <table id={ "grid_" + query.name } style={ query.tableGridStyle }>
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
     { query.groupFields.nonEmpty |* <script src={ B.buildPrefix + "/js/tag.js" } charset="utf-8"></script> }
     <script>{ Unparsed( "window.reportObj = { qn:'" + query.name + "', id:'" + id + "' };" ) }</script>
    </head> ++
    { query.groupFields.map( gf => groupValueFor( gf ).draw ) } ++
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

    case "/editSearch" =>
      web.res.html( report.innerDrawSearch )

    case "/search" =>
      query.init

      report.extractSearchRec

      if ( query.orderBy.nonEmpty ) {
        val name = web.req.s( 'sort )
        report.sort = query.orderBy.find( _.name == name ).get
      }
      report.offset = 0
      web.res.html( report.innerDraw )

    case "/filter" => 
      query.searchFields.find( _.id == web.s( 'f ) ).foreach { sf =>
        val v = web.s( 'v )
        if ( v.isBlank )
          report.searchRec.remove( sf.name )
        else
          report.searchRec( sf.name ) = sf.fromString( v )
      }

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

      web.res.ok

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
      web.js(
        JqHtml( "#gfd", report.fieldDropdown ),
        JqHtml( "#gab", report.addBox )
      )

    case "/field" =>
      report.onField = web.req.s( 'v )
      web.res.ok

    case s if s.startsWith( "/group" ) =>
      var gf = query.groupFields.find( _.id == web.s( 'gf ) ).getOrElse( query.groupFields.nonEmpty ? query.groupFields( 0 ) | null )
      if ( gf == null || !report.groupValueFor( gf ).handle( this ) )
        _404

    case _ =>
      _404
    }
  }
}

