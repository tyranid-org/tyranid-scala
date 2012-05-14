/** )
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

package org.tyranid.profile

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.xml.{ NodeSeq, Text, Unparsed }

import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.db.{ Record, Scope }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ MongoEntity, MongoRecord }
import org.tyranid.json.JqHtml
import org.tyranid.math.Base62
import org.tyranid.report.{ Report, Run }
import org.tyranid.ui.{ Field, Select, Search, Show, Valuable }
import org.tyranid.web.Weblet


/*

      +. group types:

         in-network         vs. out-of-network     ( monitored vs. connection group )
         intra-organization vs. inter-organization

      +. implement monitoring groups in freight iq

 */

case class GroupingAddBy( label:String, keys:String* ) {
  val id = label.toIdentifier
}

case class GroupField( baseName:String, l:String = null,
                       ofEntity:MongoEntity,
                       groupEntity:MongoEntity, foreignKey:String, listKey:String, forKey:String, forValue: () => AnyRef,
                       addBys:Seq[GroupingAddBy] = Nil,
                       nameSearch: ( String ) => Any = null,
                       opts:Seq[(String,String)] = Nil ) extends Field {

  val id = Base62.make( 8 )
  val name = baseName + "$cst"

  val data = true
  val default = None

  val search = Search.Custom

  val showFilter = true
  val show = Show.Editable

  def groupValueFor( rec:Record ) = rec( name ).as[GroupValue]

  override lazy val label = if ( l.notBlank ) l else "Group"

  override def init( rec:Record, report:Report ) = {
    val gv = GroupValue( report, this )
    gv.set( super.init( rec, report ) )
    rec( name ) = gv
  }

  override def ui( s:Scope ) = groupValueFor( s.rec ).drawSelect( "" )

  override def extract( s:Scope ) = groupValueFor( s.rec ).selectedGroupTid = T.web.s( id )

  override def cell( s:Scope ) =
    groupValueFor( s.run.report.searchRec ).groupsFor( s.rec.id ).toNodeSeq

  override def mongoSearch( run:Run, searchObj:DBObject, value:Any ) = {
    if ( value != null ) {
      val group = groupValueFor( run.report.searchRec ).selectedGroup
      if ( group != null ) {
        val fk = foreignKey
        searchObj( fk ) = Mobj( $in -> group.a_?( listKey ) )
      }
    }
  }

  def matchesSearch( run:Run, value:Any, rec:Record ):Boolean =
    // TODO:  implement this
    false

  override def drawPreamble( report:Report ):NodeSeq =
    // TODO:  need to have some mechanism so that we don't include things like tags more than once
    <head><script src={ B.buildPrefix + "/js/tag.js" } charset="utf-8"></script></head>
    <div id={ "rGrpDlg" + id } class="rGrpDlg" style="padding:0; display:none;"/>;

  override def drawFilter( run:Run ) = {
    val groupValue = groupValueFor( run.report.searchRec )
    
    <table class="tile" style="width:180px; height:54px;">
     <tr>
      <td class="label">view group</td>
      <td rowspan="2" style="padding-right:4px;"><a id={ "rGrpBtn" + id } href="#" class="rGrpBtn btn" style="height:40px; padding-top:6px;"><span title="View Group" class="tip bigIcon groupIcon"/><span class="label"></span></a></td>
      { ( groupValue.selectedGroup != null ) |* <td rowspan="2" style="padding-right:4px;"><a id={ "rGrpBtn" + id } href="#" class="rGrpBtn btn" style="height:40px; padding-top:6px;"><span title="View Group" class="tip bigIcon groupIcon"/><span class="label"></span></a></td> }
     </tr>
     <tr>
      <td id="rGrpChooser">{ groupValue.drawSelect() }</td>
     </tr>
    </table>
  }

  def queryGroups                         = groupEntity.db.find( Mobj( forKey -> forValue() ) ).map( o => groupEntity( o ) ).toSeq
  def queryGroupMembers( group:DBObject ) = ofEntity.db.find( Mobj( "_id" -> Mobj( $in -> group.a_?( listKey ) ) ) ).map( o => ofEntity( o ) ).toIterable

  override def handle( weblet:Weblet, rec:Record ) = {
    val gv = groupValueFor( rec )
    val report = gv.report

    val web = T.web
    val query = report.query
    val sg = gv.dialogGroup

    weblet.rpath match {
    case "/group" =>
      gv.resetGroups
      web.js( JqHtml( "#rGrpDlg" + id, gv.drawPanel ) )

    case "/group/select" =>
      gv.selectGroup( web.s( 'id ) )
      web.res.html( report.innerDraw )

    case "/group/dlgSelect" =>
      gv.setDialogGroup( web.s( 'id ) )
      web.js( JqHtml( "#rGrpMain" + id, gv.drawGroup ) )

    case "/group/addGroup" =>
      web.js( JqHtml( "#rGrpMain" + id, gv.drawAddGroup ) )

    case "/group/addGroupSave" =>
      val group = Mobj()
      group( forKey ) = forValue()
      group( 'name ) = web.s( "rGrpAddName" + id ) or "Unnamed Group"
      groupEntity.db.save( group )
      gv.resetGroups
      gv.setDialogGroup( groupEntity( group ).tid )

      web.js(
        JqHtml( "#rGrpDlg" + id, gv.drawPanel ),
        JqHtml( "#rGrpChooser", gv.drawSelect() )
      )

    case "/group/rename" =>
      web.js( JqHtml( "#rGrpMain" + id, gv.drawRename ) )

    case "/group/renameSave" =>
      if ( !sg.b( 'builtin ) ) {
        sg( 'name ) = web.s( "rGrpRenameName" + id ) or "Unnamed Group"
        groupEntity.db.save( sg )
        gv.resetGroups
      }

      web.js(
        JqHtml( "#rGrpDlg" + id, gv.drawPanel ),
        JqHtml( "#rGrpChooser", gv.drawSelect() )
      )

    case "/group/deleteGroup" =>
      if ( !sg.b( 'builtin ) ) {
        groupEntity.remove( Mobj( "_id" -> sg.id ) )
        gv.resetGroups
      }

      web.js(
        JqHtml( "#rGrpDlg" + id, gv.drawPanel ),
        JqHtml( "#rGrpChooser", gv.drawSelect() )
      )

    case "/group/addBy" =>
      val abid = web.s( 'v )
      gv.groupAddBy = null
      addBys.find( _.id == abid ).foreach { gv.groupAddBy = _ }
      web.js( JqHtml( "#rGrpAddBox" + id, gv.drawAddBy ) )

    case "/group/addMember" =>
      val ab = gv.groupAddBy

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

      web.js( JqHtml( "#rGrpMain" + id, gv.drawGroup ) )

    case "/group/remove" =>
      if ( !sg.b( 'builtin ) ) {
        groupEntity.db.update( Mobj( "_id" -> sg.id ), Mobj( $pull -> Mobj( listKey -> ofEntity.tidToId( web.s( 'id ) ) ) ) )
        gv.resetGroups
      }

      web.js( JqHtml( "#rGrpMain" + id, gv.drawGroup ) )

    case "/group/toggleAddBy" =>
      gv.groupShowAddBy = !gv.groupShowAddBy
      web.js( JqHtml( "#rGrpMain" + id, gv.drawGroup ) )

    case "/group/addSearch" =>
      val terms = web.s( 'term )

      val json =
        if ( nameSearch != null ) {
          nameSearch( terms )
        } else {
          val labelKey = ofEntity.labelAtt.get.name
          val regex = terms.toLowerCase.tokenize.map { term => Mobj( labelKey -> Mobj( $regex -> term, $options -> "i" ) ) }
          val where =
            if ( regex.size == 1 ) regex( 0 )
            else                   Mobj( $and -> Mlist( regex:_* ) )

          ofEntity.db.find( where, Mobj( labelKey -> 1 ) ).
            limit( 16 ).
            toSeq.
            map( o => Map( "id"    -> ofEntity( o ).tid,
                           "label" -> o.s( labelKey ) ) )
        }

      web.res.json( json )

    case _ =>
      weblet._404
    }
  }
}

case class GroupValue( report:Report, gf:GroupField ) extends Valuable {

  private var latestGroups:Seq[MongoRecord] = null

  @volatile var groupAddBy:GroupingAddBy = gf.addBys( 0 )

  @volatile var groupShowAddBy:Boolean = false

  def resetGroups { latestGroups = null }
  def groups = {
    if ( latestGroups == null )
      latestGroups = gf.queryGroups.toSeq

    latestGroups
  }
  def groupsFor( id:AnyRef ) = groups.filter( _.a_?( 'ids ).contains( id ) ).map( _.s( 'name ) ).mkString( ", " )

  def byId( id:Any ) = groups.find( _.id == id ).get

  var selectedGroupTid:String = null

  def get          = selectedGroupTid
  def set( v:Any ) = selectedGroupTid = v._s

  def selectedGroupId = gf.groupEntity.tidToId( selectedGroupTid )
  def selectedGroup = groups.find( g => g.tid == selectedGroupTid ).getOrElse( null )
  def selectGroup( tid:String ) { selectedGroupTid = tid; dialogGroupTid = tid }

  var dialogGroupTid:String = null
  def dialogGroupId = gf.groupEntity.tidToId( dialogGroupTid )
  def dialogGroup = groups.find( g => g.tid == dialogGroupTid ).getOrElse( null )
  def setDialogGroup( tid:String ) = dialogGroupTid = tid

  def drawSelect( cls:String = "rGrpChr" ) =
    Select( gf.id, selectedGroupTid, ( "" -> "All" ) +: groups.map( g => g.tid -> g.s( 'name ) ), "class" -> cls, "style" -> "width:120px; max-width:120px;" )

  def drawPanel = {
    val stid = dialogGroupTid

    <div class="rGrpLeft">
     <div class="rGrpSel">
      <ul class="noSelect">
       { groups.map( g => <li class={ "noSelect" + ( g.tid == stid |* " sel" ) } id={ g.tid }>{ g.s( 'name ) }</li> ) }
      </ul>
     </div>
     <div class="btns">
      <button class="rGrpAddGrp go btn" style="float:left;">Add Group</button>
     </div>
    </div>
    <div id={ "rGrpMain" + gf.id } class="rGrpMain">
     { drawGroup }
    </div>
  }

  def drawGroup = {
    val group = dialogGroup
    val editable = group != null && !group.b( 'builtin )
    val showAddBy = groupShowAddBy

    <div class={ "rGrpEdit " + ( showAddBy ? "shortTable" | "longTable" ) }>
     <div class="title">
      { if ( group != null )
          Text( group.s( 'name ) + ' ' ) ++ ( editable ? <a href="#" id={ "rGrpRename" + gf.id } class="rGrpRename" style="font-size:12px;">rename</a> | <i style="font-size:12px;">(builtin)</i> ) ++ group.eye
        else
          <i>None selected</i> }
     </div>
     { group != null |*
     <div class="list">
      <table class="dtable">
       <thead>
        <tr>
         <th>Name</th>{ if ( !showAddBy ) gf.addBys.filter( _.label != "Name" ).map( ab => <th>{ ab.label }</th> ) }{ editable |* <th/> }
         <th style="width:10px;"/>
        </tr>
       </thead>
       { val members = gf.queryGroupMembers( group ).toSeq.sortBy( _.label )
         for ( el <- members ) yield
           <tr id={ el.tid }>
            <td>{ el.label }</td>
            { if ( !showAddBy ) gf.addBys.filter( _.label != "Name" ).map( ab => <td>{ el.s( ab.keys( 0 ) ) }</td> ) }
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
      <div class="title">Add { gf.ofEntity.label.plural }</div>
      <label for="rGrpAddBy">By:</label>
      { Select( "rGrpAddBy", groupAddBy != null |* groupAddBy.id, gf.addBys.map( ab => ( ab.id, ab.label ) ) ) }
      <div id={ "rGrpAddBox" + gf.id } class="rGrpAddBox">
       { drawAddBy }
      </div>
     </form> }
    </div> } ++
    <div class="btns">
     { editable |* <button class="rGrpDelGrp stop btn" style="float:left;">Delete</button> }
     <button onclick={ "$('#rGrpDlg" + gf.id + "').dialog('close'); return false;" } class="btn" style="float:right;">Done</button>
     { editable || showAddBy |* <button class="rGrpToggleAddBy go btn" style="float:right;">{ if ( showAddBy ) "Show Table" else "Add Members" }</button> }
    </div>
  }

  def drawAddBy = {
    val addBy = groupAddBy

    addBy != null |* {
    <div class="stitle">Enter { gf.ofEntity.label } { addBy.label.plural } To Add</div> ++
    { addBy.label match {
    case "Name" => // TODO:  should match on something better
      <ul class="rGrpAddName" id={ "rGrpAddName" + gf.id }></ul>

    case ab =>
      <div class="note">(separate multiple entries with commas)</div>
      <textarea id="rGrpAddByInput" name="rGrpAddByInput" style="height:292px; width:322px;"/>
    } } ++
    <div class="btns"><a class="rGrpAddImport go btn">Add</a></div>
    }
  }

  def drawAddGroup =
    <div class="rGrpEdit">
     <div class="title" style="margin-bottom:16px;">Add New Group</div>
     <form method="post">
      <label for={ "rGrpAddName" + gf.id }>Enter Group Name:</label>
      <div class="title"><input type="text" class="rGrpAddName" name={ "rGrpAddName" + gf.id } id={ "rGrpAddName" + gf.id } style="font-size:20px;"/></div>
      <div class="btns" style="width:370px;"><a href="#" class="rGrpAddGrpSave go btn">Add Group</a></div>
     </form>
    </div>
    <div class="btns">
     <button onclick={ "$('#rGrpDlg" + gf.id + "').dialog('close'); return false;" } class="btn" style="float:right;">Cancel</button>
    </div>;

  def drawRename =
    <div class="rGrpEdit">
     <div class="title" style="margin-bottom:16px;">Rename Group</div>
     <form method="post">
      <label for={ "rGrpRenameName" + gf.id }>Enter Group Name:</label>
      <div class="title"><input type="text" class="rGrpRenameName" name={ "rGrpRenameName" + gf.id } id={ "rGrpRenameName" + gf.id } style="font-size:20px;" value={ dialogGroup.s( 'name ) }/></div>
      <div class="btns" style="width:370px;"><a href="#" class="rGrpRenameSave go btn">Rename Group</a></div>
     </form>
    </div>
    <div class="btns">
     <button onclick={ "$('#rGrpDlg" + gf.id + "').dialog('close'); return false;" } class="btn" style="float:right;">Cancel</button>
    </div>;
}

