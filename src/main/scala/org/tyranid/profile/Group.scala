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

package org.tyranid.profile

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.xml.{ NodeSeq, Text, Unparsed }

import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ MongoEntity, MongoRecord }
import org.tyranid.json.JqHtml
import org.tyranid.report.{ Report, Run }
import org.tyranid.ui.{ Field, Select, Search }
import org.tyranid.web.Weblet


/*

      +. move GroupData to searchRec ?

         a potential problem with this is that regular PathFields will use the Domain to determine how to interact with the searchRec, they don't usually look at Search

      +. what if you have more than one Grouping ?

         ... need to associate the Grouping with the Field, not with the Report ?

         +. move Grouping from Query -> Field

         +. move GroupData from Report -> ... searchRec ?

         +. generate the group control with an ID, and use that ID to pass in the ID on jQuery calls

      +. add user groups

      +. group types:

         in-network         vs. out-of-network     ( monitored vs. connection group )
         intra-organization vs. inter-organization

      +. implement monitoring groups in freight iq

 */

case class Grouping( ofEntity:MongoEntity,
                     groupEntity:MongoEntity, foreignKey:String, listKey:String, forKey:String, forValue: () => AnyRef,
                     addBys:Seq[GroupingAddBy] = Nil ) {

  lazy val searchNameKey = Search.Group.makeSearchName( foreignKey )

  def queryGroups                         = groupEntity.db.find( Mobj( forKey -> forValue() ) ).map( o => groupEntity( o ) ).toSeq
  def queryGroupMembers( group:DBObject ) = ofEntity.db.find( Mobj( "_id" -> Mobj( $in -> group.a_?( listKey ) ) ) ).map( o => ofEntity( o ) ).toIterable
}

case class GroupingAddBy( label:String, keys:String* ) {

  val id = label.toIdentifier
}

case class GroupData( report:Report, gf:Field ) {

  def grouping = gf.grouping

  private var latestGroups:Seq[MongoRecord] = null

  @volatile var groupAddBy:GroupingAddBy = null

  @volatile var groupShowAddBy:Boolean = false

  def resetGroups { latestGroups = null }
  def groups = {
    if ( latestGroups == null )
      latestGroups = grouping.queryGroups.toSeq

    latestGroups
  }
  def groupsFor( id:AnyRef ) = groups.filter( _.a_?( 'ids ).contains( id ) ).map( _.s( 'name ) ).mkString( ", " )

  def byId( id:Any ) = groups.find( _.id == id ).get

  def selectedGroupId = grouping.groupEntity.tidToId( selectedGroupTid )
  def selectedGroupTid = report.searchRec.s( grouping.searchNameKey )
  def selectedGroup = groups.find( g => g.tid == selectedGroupTid ).getOrElse( null )

  def selectGroup( tid:String ) = report.searchRec( grouping.searchNameKey ) = tid

  def drawFilter =
    <table class="tile" style="width:140px; height:54px;">
     <tr>
      <td class="label">view group</td>
     </tr>
     <tr>
      <td id="rGrpChooser">{ drawChooser }</td>
     </tr>
    </table>

  def drawChooser =
    Select( "rGroups", selectedGroupTid, ( "" -> "All" ) +: groups.map( g => g.tid -> g.s( 'name ) ), "style" -> "width:120px; max-width:120px;" )

  def draw =
    <div id="rGrpDlg" style="padding:0; display:none;">
     { drawPanel }
    </div>

  def drawPanel = {
    val stid = selectedGroupTid

    <div id="rGrpLeft">
     <div id="rGrpSel">
      <ul class="noSelect">
       { groups.map( g => <li class={ "noSelect" + ( g.tid == stid |* " sel" ) } id={ g.tid }>{ g.s( 'name ) }</li> ) }
      </ul>
     </div>
     <div class="btns">
      <button id="rGrpAddGrp" class="greenBtn" style="float:left;">Add Group</button>
     </div>
    </div>
    <div id="rGrpMain">
     { drawGroup }
    </div>
  }

  def drawGroup = {
    val group = selectedGroup
    val editable = group != null && !group.b( 'builtin )
    val showAddBy = groupShowAddBy

    <div id={ gf.id } class={ "rGrpEdit " + ( showAddBy ? "shortTable" | "longTable" ) }>
     <div class="title">
      { if ( group != null )
          Text( group.s( 'name ) + ' ' ) ++ ( editable ? <a href="#" id="rGrpRename" style="font-size:12px;">rename</a> | <i style="font-size:12px;">(builtin)</i> ) ++ group.eye
        else
          <i>None selected</i> }
     </div>
     { group != null |*
     <div class="list">
      <table class="dtable">
       <thead>
        <tr>
         <th>Name</th>{ if ( !showAddBy ) grouping.addBys.filter( _.label != "Name" ).map( ab => <th>{ ab.label }</th> ) }{ editable |* <th/> }
         <th style="width:10px;"/>
        </tr>
       </thead>
       { val members = grouping.queryGroupMembers( group ).toSeq.sortBy( _.label )
         for ( el <- members ) yield
           <tr id={ el.tid }>
            <td>{ el.label }</td>
            { if ( !showAddBy ) grouping.addBys.filter( _.label != "Name" ).map( ab => <td>{ el.s( ab.keys( 0 ) ) }</td> ) }
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
      <div class="title">Add { grouping.ofEntity.label.plural }</div>
      <label for="rGrpAddBy">By:</label>
      { Select( "rGrpAddBy", groupAddBy != null |* groupAddBy.id, ( "" -> "Select" ) +: grouping.addBys.map( ab => ( ab.id, ab.label ) ) ) }
      <div id="rGrpAddBox">
       { drawAddBy }
      </div>
     </form> }
    </div> } ++
    <div class="btns">
     { editable |* <button id="rGrpDelGrp" class="redBtn" style="float:left;">Delete</button> }
     <button onclick="$('#rGrpDlg').dialog('close'); return false;" class="greyBtn" style="float:right;">Done</button>
     { editable || showAddBy |* <button id="rGrpToggleAddBy" class="greenBtn" style="float:right;">{ if ( showAddBy ) "Show Table" else "Add Members" }</button> }
    </div>
  }

  def drawAddBy = {
    val addBy = groupAddBy

    addBy != null |* {
    <div class="stitle">Enter { grouping.ofEntity.label } { addBy.label.plural } To Add</div> ++
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

  def drawAddGroup = {

    <div id={ gf.id } class="rGrpEdit">
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

  def drawRename = {

    <div id={ gf.id } class="rGrpEdit">
     <div class="title" style="margin-bottom:16px;">Rename Group</div>
     <form method="post">
      <label for="rGrpName">Enter Group Name:</label>
      <div class="title"><input type="text" name="rGrpName" id="rGrpName" style="font-size:20px;" value={ selectedGroup.s( 'name ) }/></div>
      <div class="btns" style="width:370px;"><a href="#" class="greenBtn" id="rGrpRenameSave">Rename Group</a></div>
     </form>
    </div>
    <div class="btns">
     <button onclick="$('#rGrpDlg').dialog('close'); return false;" class="greyBtn" style="float:right;">Cancel</button>
    </div>
  }

  def handle( weblet:Weblet ):Boolean = {
    val web = T.web
    val query = report.query
    val sg = selectedGroup

    weblet.rpath match {
    case "/group" =>
      resetGroups
      web.js( JqHtml( "#rGrpDlg", drawPanel ) )

    case "/group/select" =>
      selectGroup( web.s( 'id ) )
      web.res.html( report.innerDraw )

    case "/group/dlgSelect" =>
      selectGroup( web.s( 'id ) )
      web.js( JqHtml( "#rGrpMain", drawGroup ) )

    case "/group/addGroup" =>
      web.js( JqHtml( "#rGrpMain", drawAddGroup ) )

    case "/group/addGroupSave" =>
      val group = Mobj()
      group( grouping.forKey ) = grouping.forValue()
      group( 'name ) = web.s( 'rGrpName ) or "Unnamed Group"
      grouping.groupEntity.db.save( group )
      resetGroups
      selectGroup( grouping.groupEntity( group ).tid )

      web.js(
        JqHtml( "#rGrpDlg", drawPanel ),
        JqHtml( "#rGrpChooser", drawChooser )
      )

    case "/group/rename" =>
      web.js( JqHtml( "#rGrpMain", drawRename ) )

    case "/group/renameSave" =>
      if ( !sg.b( 'builtin ) ) {
        sg( 'name ) = web.s( 'rGrpName ) or "Unnamed Group"
        grouping.groupEntity.db.save( sg )
        resetGroups
      }

      web.js(
        JqHtml( "#rGrpDlg", drawPanel ),
        JqHtml( "#rGrpChooser", drawChooser )
      )

    case "/group/deleteGroup" =>
      if ( !sg.b( 'builtin ) ) {
        grouping.groupEntity.remove( Mobj( "_id" -> sg.id ) )
        resetGroups
      }

      web.js(
        JqHtml( "#rGrpDlg", drawPanel ),
        JqHtml( "#rGrpChooser", drawChooser )
      )

    case "/group/addBy" =>
      val id = web.s( 'v )
      groupAddBy = null
      gf.grouping.addBys.find( _.id == id ).foreach { groupAddBy = _ }
      web.js( JqHtml( "#rGrpAddBox", drawAddBy ) )

    case "/group/addMember" =>
      val ab = groupAddBy

      if ( ab != null && sg != null && !sg.b( 'builtin ) ) {

        val ids:Seq[Any] =
          ab.label match {
          case "Name" => // TODO:  should match on something better
            web.a_?( 'addTids ).map( grouping.ofEntity.tidToId )

          case _ =>
            val keyAtts = ab.keys.map( grouping.ofEntity.attrib )

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

            grouping.ofEntity.db.find( where, Mobj( "_id" -> 1 ) ).map( _( '_id ) ).toSeq
          }

        sg( grouping.listKey ) = Mlist( ( sg.a_?( grouping.listKey ) ++ ids ).distinct:_* )
        grouping.groupEntity.db.save( sg )
      }

      web.js( JqHtml( "#rGrpMain", drawGroup ) )

    case "/group/remove" =>
      if ( !sg.b( 'builtin ) ) {
        grouping.groupEntity.db.update( Mobj( "_id" -> sg.id ), Mobj( $pull -> Mobj( grouping.listKey -> grouping.ofEntity.tidToId( web.s( 'id ) ) ) ) )
        resetGroups
      }

      web.js( JqHtml( "#rGrpMain", drawGroup ) )

    case "/group/toggleAddBy" =>
      groupShowAddBy = !groupShowAddBy
      web.js( JqHtml( "#rGrpMain", drawGroup ) )

    case "/group/addSearch" =>
      val terms = web.s( 'term )

      val labelKey = grouping.ofEntity.labelAtt.get.name
      val regex = terms.toLowerCase.tokenize.map { term => Mobj( labelKey -> Mobj( $regex -> term, $options -> "i" ) ) }
      val where =
        if ( regex.size == 1 ) regex( 0 )
        else                   Mobj( $and -> Mlist( regex:_* ) )

      val json =
        grouping.ofEntity.db.find( where, Mobj( labelKey -> 1 ) ).
          limit( 16 ).
          toSeq.
          map( o => Map( "id"        -> grouping.ofEntity( o ).tid,
                         "label"     -> o.s( labelKey ) ) )

      web.res.json( json )

    case _ =>
      return false
    }

    true
  }
}

