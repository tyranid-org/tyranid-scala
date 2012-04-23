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

import org.tyranid.Imp._
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.MongoEntity
import org.tyranid.json.JqHtml
import org.tyranid.report.{ Report, Run }
import org.tyranid.ui.{ Select, Search }
import org.tyranid.web.Weblet


/*

      +. move group data in Report to a new case class on Report

      +. move the ^^^ case class to searchRec ?

      +. monitored vs. connection group

      +. add user groups

      +. implement monitoring groups in freight iq

 */

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

  // TODO:  merge this with the regular filtering
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
          Text( group.s( 'name ) + ' ' ) ++ ( editable ? <a href="#" id="rGrpRename" style="font-size:12px;">rename</a> | <i style="font-size:12px;">(builtin)</i> ) ++ group.eye
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

