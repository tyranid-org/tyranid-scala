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
import org.tyranid.db.mongo.{ MongoEntity, MongoRecord }
import org.tyranid.db.ram.RamEntity
import org.tyranid.json.{ Js, JqHtml }
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
 * * *   G r o u p
 */

/*

    Grouping
      .entity = OrgGroup
      .listKey = ids
      .forKey = org
      .forValue = current user's org

      .foreignKey = _id
      .addbys = [
        GroupingAddBy( "Name", "legalName" ),
        GroupingAddBy( "MC", "icc1", "icc2", "icc3" ),
        GroupingAddBy( "US DOT", "usdot" ),
        GroupingAddBy( "D&B Number", "dbNum" )
      ]


      +. lazily create Orgs when referenced in ExtendedProfile

      +. monitored vs. connection group

 */

abstract class Filter( val foreignKey:String ) {
  val label = "filter"
  val filterStyle = "width:344px; height:54px;"
    
  private lazy val searchNameKey = Search.Filter.makeSearchName( foreignKey )

  def selectedFilter( report:Report ) = report.searchRec.s( searchNameKey )
  def selectedFilter( report:Report, value:String ) = report.searchRec( searchNameKey ) = value
    
    
  def filterValues:Seq[ DBObject ]
  
  def draw( run:Run ) =
    <table class="tile" style={ filterStyle }>
     <tr>
      <td class="label">{ label }</td>
     </tr>
     <tr>
      <td id="rGrpChooser">
      { Select( "rFilter", selectedFilter( run.report ), ( "" -> "-Please Select-" ) +: filterValues.map( v => ( v.s( '_id ), v.s( 'name ) ) ) ) }
      </td>
     </tr>
    </table>
}

case class GroupingAddBy( label:String, keys:String* ) {

  val id = label.toIdentifier
}

case class Grouping( ofEntity:MongoEntity,
                     groupEntity:MongoEntity, foreignKey:String, listKey:String, forKey:String, forValue: () => AnyRef,
                     addBys:Seq[GroupingAddBy] = Nil ) {

  private lazy val searchNameKey = Search.Group.makeSearchName( foreignKey )

  def queryGroups = groupEntity.db.find( Mobj( forKey -> forValue() ) ).map( o => groupEntity( o ) ).toSeq

  def byId( report:Report, id:Any ) = report.groups.find( _.id == id ).get

  def selectedGroupId( report:Report ) = groupEntity.tidToId( selectedGroupTid( report ) )
  def selectedGroupTid( report:Report ) = report.searchRec.s( searchNameKey )
  def selectedGroup( report:Report ) = report.groups.find( g => g.tid == selectedGroupTid( report ) ).getOrElse( null )

  def selectGroup( report:Report, tid:String ) = report.searchRec( searchNameKey ) = tid

  def drawFilter( run:Run ) =
    <table class="tile" style="width:140px; height:54px;">
     <tr>
      <td class="label">view group</td>
     </tr>
     <tr>
      <td id="rGrpChooser">{ drawChooser( run.report ) }</td>
     </tr>
    </table>

  def drawChooser( report:Report ) =
    Select( "rGroups", selectedGroupTid( report ), ( "" -> "All" ) +: report.groups.map( g => g.tid -> g.s( 'name ) ), "style" -> "width:120px; max-width:120px;" )

  def draw( report:Report ) =
    <div id="rGrpDlg" style="padding:0; display:none;">
    { drawPanel( report ) }
    </div>

  def drawPanel( report:Report ) = {
    val stid = selectedGroupTid( report )

    <div id="rGrpLeft">
     <div id="rGrpSel">
      <ul class="noSelect">
       { report.groups.map( g => <li class={ "noSelect" + ( g.tid == stid |* " sel" ) } id={ g.tid }>{ g.s( 'name ) }</li> ) }
      </ul>
     </div>
     <div class="btns">
      <button id="rGrpAddGrp" class="greenBtn" style="float:left;">Add Group</button>
     </div>
    </div>
    <div id="rGrpMain">
     { drawGroup( report ) }
    </div>
  }

  def drawGroup( report:Report ) = {
    val group = selectedGroup( report )
    val editable = group != null && !group.b( 'builtin )
    val showAddBy = report.groupShowAddBy

    <div id="rGrpEdit" class={ showAddBy ? "shortTable" | "longTable" }>
     <div class="title">
      { if ( group != null )
          Text( group.s( 'name ) + ' ' ) ++ ( editable ? <a href="#" id="rGrpRename" style="font-size:12px;">rename</a> | <i style="font-size:12px;">(builtin)</i> )
        else
          <i>None selected</i> }
     </div>
     { group != null |*
     <div class="list">
      <table class="dtable">
       <thead>
        <tr>
         <th>Name</th>{ if ( !showAddBy ) addBys.filter( _.label != "Name" ).map( ab => <th>{ ab.label }</th> ) }{ editable |* <th/> }
         <th style="width:10px;"/>
        </tr>
       </thead>
       { val members = ofEntity.db.find( Mobj( "_id" -> Mobj( $in -> group.a_?( listKey ) ) ) ).map( o => ofEntity( o ) ).toSeq.sortBy( _.label )
         for ( el <- members ) yield
           <tr id={ el.tid }>
            <td>{ el.label }</td>
            { if ( !showAddBy ) addBys.filter( _.label != "Name" ).map( ab => <td>{ el.s( ab.keys( 0 ) ) }</td> ) }
            { editable |* <td><a href="#">remove</a></td> }
            <td/>
           </tr>
       }
      </table>
     </div> }
    </div> ++
    { showAddBy |*
    <div class="add">
     { editable |*
     <form method="post" id="rGrpAddForm">
      <div class="title">Add { ofEntity.label.plural }</div>
      <label for="rGrpAddBy">By:</label>
      { Select( "rGrpAddBy", report.groupAddBy != null |* report.groupAddBy.id, ( "" -> "Select" ) +: addBys.map( ab => ( ab.id, ab.label ) ) ) }
      <div id="rGrpAddBox">
       { drawAddBy( report ) }
      </div>
     </form> }
    </div> } ++
    <div class="btns">
     { editable |* <button id="rGrpDelGrp" class="redBtn" style="float:left;">Delete</button> }
     <button onclick="$('#rGrpDlg').dialog('close'); return false;" class="greyBtn" style="float:right;">Done</button>
     { editable || showAddBy |* <button id="rGrpToggleAddBy" class="greenBtn" style="float:right;">{ if ( showAddBy ) "Show Table" else "Add Members" }</button> }
    </div>
  }

  def drawAddBy( report:Report  ) = {
    val addBy = report.groupAddBy

    addBy != null |* {
    <div class="stitle">Enter { ofEntity.label } { addBy.label.plural } To Add</div> ++
    { addBy.label match {
    case "Name" => // TODO:  should match on something better
      <ul id="rGrpAddName"></ul>

    case ab =>
      <div class="note">(separate multiple entries with commas)</div>
      <textarea id="rGrpAddByInput" name="rGrpAddByInput" style="height:292px; width:322px;"/>
    } } ++
    <div class="btns"><a id="rGrpAddImport" class="greenBtn">Add</a></div>
    }
  }

  def drawAddGroup( report:Report ) = {

    <div id="rGrpEdit">
     <div class="title" style="margin-bottom:16px;">Add New Group</div>
     <form method="post">
      <label for="rGrpName">Enter Group Name:</label>
      <div class="title"><input type="text" name="rGrpName" id="rGrpName" style="font-size:20px;"/></div>
      <div class="btns" style="width:370px;"><a href="#" class="greenBtn" id="rGrpAddGrpSave">Add Group</a></div>
     </form>
    </div>
    <div class="btns">
     <button onclick="$('#rGrpDlg').dialog('close'); return false;" class="greyBtn" style="float:right;">Cancel</button>
    </div>
  }

  def drawRename( report:Report ) = {

    <div id="rGrpEdit">
     <div class="title" style="margin-bottom:16px;">Rename Group</div>
     <form method="post">
      <label for="rGrpName">Enter Group Name:</label>
      <div class="title"><input type="text" name="rGrpName" id="rGrpName" style="font-size:20px;" value={ selectedGroup( report ).s( 'name ) }/></div>
      <div class="btns" style="width:370px;"><a href="#" class="greenBtn" id="rGrpRenameSave">Rename Group</a></div>
     </form>
    </div>
    <div class="btns">
     <button onclick="$('#rGrpDlg').dialog('close'); return false;" class="greyBtn" style="float:right;">Cancel</button>
    </div>
  }

  def handle( weblet:Weblet, report:Report ):Boolean = {
    val web = T.web
    val query = report.query
    val sg = selectedGroup( report )

    weblet.rpath match {
    case "/groups" =>
      selectGroup( report, web.s( 'id ) )
      web.res.html( report.innerDraw )

    case "/group" =>
      report.resetGroups
      web.js( JqHtml( "#rGrpDlg", drawPanel( report ) ) )

    case "/group/addGroup" =>
      web.js( JqHtml( "#rGrpMain", drawAddGroup( report ) ) )

    case "/group/addGroupSave" =>
      val group = Mobj()
      group( forKey ) = forValue()
      group( 'name ) = web.s( 'rGrpName ) or "Unnamed Group"
      groupEntity.db.save( group )
      report.resetGroups
      selectGroup( report, groupEntity( group ).tid )

      web.js(
        JqHtml( "#rGrpDlg", drawPanel( report ) ),
        JqHtml( "#rGrpChooser", drawChooser( report ) )
      )

    case "/group/rename" =>
      web.js( JqHtml( "#rGrpMain", drawRename( report ) ) )

    case "/group/renameSave" =>
      if ( !sg.b( 'builtin ) ) {
        sg( 'name ) = web.s( 'rGrpName ) or "Unnamed Group"
        groupEntity.db.save( sg )
        report.resetGroups
      }

      web.js(
        JqHtml( "#rGrpDlg", drawPanel( report ) ),
        JqHtml( "#rGrpChooser", drawChooser( report ) )
      )

    case "/group/deleteGroup" =>
      if ( !sg.b( 'builtin ) ) {
        groupEntity.remove( Mobj( "_id" -> sg.id ) )
        report.resetGroups
      }

      web.js(
        JqHtml( "#rGrpDlg", drawPanel( report ) ),
        JqHtml( "#rGrpChooser", drawChooser( report ) )
      )

    case "/group/addBy" =>
      val id = web.s( 'v )
      report.groupAddBy = null
      report.query.grouping.addBys.find( _.id == id ).foreach { report.groupAddBy = _ }
      web.js( JqHtml( "#rGrpAddBox", drawAddBy( report ) ) )

    case "/group/addMember" =>
      val ab = report.groupAddBy

      if ( ab != null && sg != null && !sg.b( 'builtin ) ) {

        val ids:Seq[Any] =
          ab.label match {
          case "Name" => // TODO:  should match on something better
            web.a_?( 'addTids ).map( ofEntity.tidToId )

          case _ =>
            val keyAtts = ab.keys.map( ofEntity.attrib )

            val altIds = web.s( 'rGrpAddByInput ).split( "," ).map( _.trim )

            val keys = keyAtts map { att =>
              // TODO:  use att.domain to convert these strings to ints or whatever else is needed based on the domain
              val nativeAltIds = altIds

              Mobj( att.name -> (
                if ( nativeAltIds.size == 1 )
                  nativeAltIds( 0 )
                else
                  Mobj( $in -> Mlist( nativeAltIds:_* ) )
              ) )
            }

            val where =
              if ( keys.size == 1 ) keys( 0 )
              else                  Mobj( $or -> Mlist( keys:_* ) )

            ofEntity.db.find( where, Mobj( "_id" -> 1 ) ).map( _( '_id ) ).toSeq
          }

        sg( listKey ) = Mlist( ( sg.a_?( listKey ) ++ ids ).distinct:_* )
        groupEntity.db.save( sg )
      }

      web.js( JqHtml( "#rGrpMain", drawGroup( report ) ) )

    case "/group/remove" =>
      if ( !sg.b( 'builtin ) ) {
        groupEntity.db.update( Mobj( "_id" -> sg.id ), Mobj( $pull -> Mobj( listKey -> ofEntity.tidToId( web.s( 'id ) ) ) ) )
        report.resetGroups
      }

      web.js( JqHtml( "#rGrpMain", drawGroup( report ) ) )

    case "/group/toggleAddBy" =>
      report.groupShowAddBy = !report.groupShowAddBy
      web.js( JqHtml( "#rGrpMain", drawGroup( report ) ) )

    case "/group/select" =>
      selectGroup( report, web.s( 'id ) )
      web.js( JqHtml( "#rGrpMain", drawGroup( report ) ) )

    case "/group/addSearch" =>
      val terms = web.s( 'term )

      val labelKey = ofEntity.labelAtt.get.name
      val regex = terms.toLowerCase.tokenize.map { term => Mobj( labelKey -> Mobj( $regex -> term, $options -> "i" ) ) }
      val where =
        if ( regex.size == 1 ) regex( 0 )
        else                   Mobj( $and -> Mlist( regex:_* ) )

      val json =
        ofEntity.db.find( where, Mobj( labelKey -> 1 ) ).
          limit( 16 ).
          toSeq.
          map( o => Map( "id"        -> ofEntity( o ).tid,
                         "label"     -> o.s( labelKey ) ) )

      web.res.json( json )

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

  lazy val view = entity.makeView

  lazy val boundFields = {
    val allFields =
      fields ++
      ( grouping != null |* List( new PathField( grouping.foreignKey, l = "Group", data = false, search = Search.Group ) ) ) ++ 
      ( filter != null |* List( new PathField( filter.foreignKey, l = filter.label, data = false, search = Search.Filter ) ) ) 

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

  lazy val dataFields:Seq[Field]   = boundFields.filter( _.data )
  lazy val searchFields:Seq[Field] = boundFields.filter( _.search != null )

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

  val grouping:Grouping = null
  val filter:Filter = null

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
      def cell( s:Scope ) = <a href={ "/admin/tid?tid=" + s.rec.tid } class="eyeBtn" style="margin:0 1px;">T</a>
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
    for ( sf <- query.searchFields;
          default <- sf.default )
      rec( sf.name ) = default()

     rec 
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
   * * *  Groups
   */

  private var latestGroups:Seq[MongoRecord] = null

  @volatile var groupAddBy:GroupingAddBy = null

  @volatile var groupShowAddBy:Boolean = false

  def resetGroups { latestGroups = null }
  def groups = {
    if ( latestGroups == null )
      latestGroups = query.grouping.queryGroups.toSeq

    latestGroups
  }
  def groupsFor( id:AnyRef ) = groups.filter( _.a_?( 'ids ).contains( id ) ).map( _.s( 'name ) ).mkString( ", " )


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
              ( query.grouping != null && !B.PRODUCTION |* <td><button id="rGroup" class="greyBtn">Group</button></td> ) }
            { query.extraActions } 
           </tr>
          </table>
         </td>
        </tr>
       </table>
      </td>
      { query.grouping != null |* <td>{ query.grouping.drawFilter( run ) }</td> }
      { query.filter != null |* <td>{ query.filter.draw( run ) }</td> }
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
     { query.grouping != null |* <script src={ B.buildPrefix + "/js/tag.js" } charset="utf-8"></script> }
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

    case "/editSearch" =>
      web.res.html( report.innerDrawSearch )

    case "/search" =>
      query.init

      val s = Scope( report.searchRec )
      report.searchRec.clear
      query.searchFields.foreach { _.extract( s ) }

      if ( query.orderBy.nonEmpty ) {
        val name = web.req.s( 'sort )
        report.sort = query.orderBy.find( _.name == name ).get
      }
      report.offset = 0
      web.res.html( report.innerDraw )

    case "/filter" => 
      query.filter.selectedFilter( report, web.s( 'id ) )
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

    case _ if query.grouping != null && query.grouping.handle( this, report ) =>

    case _ =>
      _404
    }
  }
}

