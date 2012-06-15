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

import org.bson.types.ObjectId

import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.db.{ DbArray, DbBoolean, DbChar, DbInt, DbLink, DbTid, Entity, EnumEntity, Record, Scope }
import org.tyranid.db.meta.{ Tid, TidItem }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity, MongoRecord }
import org.tyranid.db.ram.RamEntity
import org.tyranid.db.tuple.Tuple
import org.tyranid.json.JqHtml
import org.tyranid.math.Base62
import org.tyranid.report.{ Report, Run }
import org.tyranid.ui.{ Checkbox, Field, Help, Select, Search, Show, Valuable }
import org.tyranid.web.{ WebContext, Weblet }


/*

      +. group types:

         in-network         vs. out-of-network     ( monitored vs. connection group )
         intra-organization vs. inter-organization

      +. implement monitoring groups in freight iq

 */


object GroupType extends RamEntity( tid = "a0Nt" ) with EnumEntity[GroupType] {
  "_id"    is DbInt      is 'id;
  "name"   is DbChar(64) is 'label;

  def apply( id:Int, name:String ) = {
    val t = new GroupType
    t( '_id )  = id
    t( 'name ) = name
    t
  }

  val Org  = apply( 1, "Org" )
  val User = apply( 2, "User" )

  static( Org, User )
}

class GroupType extends Tuple( GroupType.makeView ) {

  lazy val ofEntity =
    this match {
    case GroupType.Org  => B.Org
    case GroupType.User => B.User
    case _              => problem( "invalid group type" )
    }

  def iconClass16x16 =
    this match {
    case GroupType.Org  => "linkIcon connectionsIcon"
    case GroupType.User => "linkIcon personIcon"
    }

  def iconClass32x32 =
    this match {
    case GroupType.Org  => "headerIcon headerConnectionsIcon"
    case GroupType.User => "headerIcon headerContactsIcon"
    }
}


object GroupMode extends RamEntity( tid = "a0Ot" ) with EnumEntity[GroupType] {
  "_id"    is DbInt      is 'id;
  "name"   is DbChar(64) is 'label;

  def apply( id:Int, name:String ) = {
    val t = new GroupType
    t( '_id )  = id
    t( 'name ) = name
    t
  }

  def monitorHelp =
    Text( "Monitor groups are groups that are not visible to their members, and are used only for personal or organizational purposes.  They are generally not used for collaboration." )

  val Monitor       = apply( 1, "Monitor"   )
  val Moderated     = apply( 2, "Moderated" )
  val Collaborative = apply( 3, "Open"      ) // A collaborative group is one which everyone inside the group can see each others things.

  static( Monitor, Moderated, Collaborative )
}

class GroupMode extends Tuple( GroupMode.makeView )


object Group extends MongoEntity( tid = "a0Yv" ) {
  type RecType = Group
  override def convert( obj:DBObject, parent:MongoRecord ) = new Group( obj, parent )


  "_id"      is DbMongoId         is 'id;
  "name"     is DbChar(60)        is 'label;
  "builtin"  is DbBoolean         help Text( "A builtin group is maintained by the system and is not editable by end users." );
  "type"     is DbLink(GroupType) ;
  "mode"     is DbLink(GroupMode) ;
  "pk"       is DbChar(10)        help Text( "A private-key, generated on-demand.  Used where a group URL needs to be hard-to-guess-yet-publicly-accessible.  For example, RSS Feeds." );

  override def init = {
    super.init
    "tids"   is DbArray(DbTid(B.Org,B.User)) ;
    "owners" is DbArray(DbTid(B.Org,B.User)) is 'owner;
  }

  // these fields are implicit:
//"<orgtid>Ids"  is DbArray(DbMongoId)        ;
//"<usertid>Ids" is DbArray(DbMongoId)        ;

  //"color"          // future ... colored labels
  //"search"         { search criteria } // future ... list search for a group, rather than each id explicitly


  db.ensureIndex( Mobj( "owners" -> 1, "name" -> 1 ) )

  def flatten( tids:Seq[String] ) =
    tids.
      flatMap(
      _ match {
     case tid if tid.startsWith( Group.tid ) =>
       Group.byTid( tid ).flatten( _.a_?( 'tids ).toSeq.of[String], Nil )

     case tid =>
       Seq( tid )
     } ).
     distinct

  def flattenMonitor( tids:Seq[String] ) =
    tids.
      flatMap(
      _ match {
     case tid if tid.startsWith( Group.tid ) =>
       Group.byTid( tid ).flatten(
         grp => {
           if ( grp.monitor )
             grp.a_?( 'tids ).toSeq.of[String]
           else
             Seq( tid )
         },
         Nil
       )

     case tid =>
       Seq( tid )
     } ).
     distinct

  def idsFor( groupType:GroupType ) = groupType.ofEntity.tid + "Ids"

  def visibleTo( user:User ) = {
    val tids =
      if ( user.org != null ) Mobj( $in -> Array( user.tid, user.org.tid ) )
      else                    user.tid

    val myGroups =
      db.find(
        Mobj( "owners" -> tids )
      ).map( apply ).toSeq

    val memberGroups =
      db.find( 
        Mobj( "tids" -> tids,
              "mode" -> Mobj( $ne -> GroupMode.Monitor.id ) )
      ).map( apply ).toSeq.filter( memberGroup => !myGroups.exists( _.id == memberGroup.id ) )

    myGroups ++ memberGroups 
  }

  def byPrivateId( privateId:String ) = {
    val split = privateId.length - 10
    val tid = privateId.substring( 0, split )
    val pk  = privateId.substring( split )

    byTid( tid ).filter( _.pk == pk )
  }
}

class Group( obj:DBObject, parent:MongoRecord ) extends MongoRecord( Group.makeView, obj, parent ) {

  def name     = s( 'name )
  def fullName = name + " (" + ( isOwner( T.user ) ? "me" | ownerNames ) + ")"

  override def id:ObjectId = super.apply( "_id" ).as[ObjectId]
  

  def mode = GroupMode.byId( i( 'mode ) ).orNull

  def monitor       = mode == GroupMode.Monitor
  def moderated     = mode == GroupMode.Moderated
  def collaborative = mode == GroupMode.Collaborative

  // TODO:  remove this method once it is no longer used
  def updateMode( monitor:Boolean ) = {
    this( 'mode ) =
      if ( monitor )                         GroupMode.Monitor.id
      else if ( groupType == GroupType.Org ) GroupMode.Moderated.id
      else                                   GroupMode.Collaborative.id
  }

  def isOwner( user:User ) = {
    val owners = a_?( 'owners )
    owners.has( user.tid ) ||
    ( user.org != null && owners.has( user.org.tid ) )
  }

  def isOwner( tid:String ) = {
    val owners = a_?( 'owners )
    owners.has( tid ) || (
      B.User.hasTid( tid ) && {
        val org = TidItem.by( tid ).org
        org != null && owners.has( B.Org.idToTid( org ) )
      }
    )
  }

  def firstOwnerTid( notTids:String* ):String = {
    val owners = a_?( 'owners )
    
    owners.foreach( t => {
      val tid = t._s
      
      if ( !notTids.contains( tid ) )
        return tid
    } )
    
    null
  }

  def ownerNames = a_?( 'owners ).map( tid => TidItem.by( tid.as[String] ).name ).mkString( ", " )

  def updateIds =
    for ( en <- Group.attrib( 'tids ).domain.as[DbArray].of.as[DbTid].of ) {

      val seq = a_?( 'tids ).map( _.as[String] ).filter( _.startsWith( en.tid ) ).map( en.tidToId )
      val field = en.tid + "Ids"

      if ( seq.nonEmpty )
        obj( field ) = seq.toMlist
      else
        obj.remove( field )
    }

  def groupType = GroupType.byId( i( 'type ) ).getOrElse( GroupType.Org ).as[GroupType]

  def iconClass16x16 = groupType.iconClass16x16
  def iconClass32x32 = groupType.iconClass32x32

  def entities = a_?( 'tids ).toSeq.of[String].map( _.substring( 0, 4 ) ).distinct.map( tid => Entity.byTid( tid ).get )

  def members =
   for ( e <- entities;
         en = e.as[MongoEntity];
         r <- en.db.find( Mobj( "_id" -> Mobj( $in -> obj.a_?( en.tid + "Ids" ) ) ) );
         rec = en( r ) )
      yield rec

  def isMember( tid:String ) = a_?( 'tids ).toSeq.find( _ == tid ) != None
  
  def isMember( user:User ) = {
    val tids = a_?( 'tids )
    tids.has( user.tid ) ||
    ( user.org != null && tids.has( user.org.tid ) )
  }
  
  def canSee( member:Record ):Boolean = canSee( T.user, member )

  def canSee( user:User, member:Record ) =
    if ( isOwner( user ) ) {
      true
    } else {
      mode match {
      case GroupMode.Monitor =>
        false

      case GroupMode.Moderated =>
        isMember( user ) &&
        ( groupType match {
          case GroupType.Org  => member.tid == user.tid || isOwner( member.tid ) || isOwner( user ) || member.tid == user.orgTid || B.Org.orgIdFor( member ) == user.orgId
          case GroupType.User => member.tid == user.tid || isOwner( member.tid ) || isOwner( user )
          } )

      case GroupMode.Collaborative =>
        isMember( user )
      }
    }

  // "private" key
  def pk = {
    var v = s( 'pk )
    if ( v.isBlank ) {
      v = Base62.make( 10 )
      update( 'pk, v )

      if ( !isNew )
        db.update( Mobj( "_id" -> id ), Mobj( $set -> Mobj( "pk" -> v ) ) )
    }

    v
  }

  def privateId = tid + pk

  def about = {
    val sb = new StringBuilder

    sb ++= "<i style=\"font-size:80%;\">("

    if ( b( 'builtin ) )
      sb ++= "Built-in "

    sb ++= mode.label += ' '
    sb ++= groupType.label
    sb ++= " Group)</i>"

    Unparsed( sb.toString )
  }
  
  val isBuiltin = b( 'builtin )
}



case class GroupingAddBy( label:String, keys:String* ) {
  val id = label.toIdentifier
}

case class GroupField( baseName:String, l:String = null,
                       ofEntity:MongoEntity,
                       groupType:GroupType, foreignKey:String,
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

  lazy val idsField = Group.idsFor( groupType )

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
    groupValueFor( s.run.report.searchRec ).groupsFor( s.rec.tid ).toNodeSeq

  override def mongoSearch( run:Run, searchObj:DBObject, value:Any ) = {
    if ( value != null ) {
      val group = groupValueFor( run.report.searchRec ).selectedGroup
      if ( group != null ) {
        val fk = foreignKey
        searchObj( fk ) = Mobj( $in -> group.obj.a_?( idsField ) )
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
      <td rowspan="2" style="padding-right:4px;">
       <a id={ "rGrpBtn" + id } href="#" class="rGrpBtn btn" style="height:40px; padding-top:6px;">
        <span tip="Groups" class={ "tip " + /* bigIcon groupIcon */ groupType.iconClass32x32 } style="padding:0;"/>
        <span class="label"/>
       </a>
      </td>
     </tr>
     <tr>
      <td id="rGrpChooser">{ groupValue.drawSelect() }</td>
     </tr>
    </table>
  }

  def userOwnerTid = 
    groupType match {
    case GroupType.Org  => T.session.orgTid
    case GroupType.User => T.session.user.tid
    }

  def accessTidsQuery = {
    val s = T.session
    if ( s.orgTid.notBlank ) Mobj( $in -> Mlist( s.orgTid, s.user.tid ) )
    else                     s.user.tid
  }


  def queryGroups                         = Group.db.find( Mobj( "owners" -> accessTidsQuery, "type" -> groupType.id ) ).map( o => Group( o ) ).toSeq
  def queryGroupMembers( group:DBObject ) = ofEntity.db.find( Mobj( "_id" -> Mobj( $in -> group.a_?( idsField ) ) ) ).map( o => ofEntity( o ) ).toIterable

  override def handle( weblet:Weblet, rec:Record ) = {
    val gv = groupValueFor( rec )
    val report = gv.report

    val web = T.web
    val query = report.query
    val dg = gv.dialogGroup

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
      val monitor = web.b( "rGrpMonitor" + id )
      val group = Group.make
      group( 'name )    = web.s( "rGrpAddName" + id ) or "Unnamed Group"
      group( 'type )    = groupType.id

      group.updateMode( monitor )

      group( 'owners ) = Mlist( userOwnerTid )

      if ( !monitor ) {
        group( 'tids ) =
          ( groupType match {
            case GroupType.Org  => Seq( T.session.orgTid )
            case GroupType.User => Seq( T.session.user.tid )
            } ).toMlist
        group.updateIds
      }

      group.save
      gv.resetGroups
      gv.setDialogGroup( Group( group ).tid )

      web.js(
        JqHtml( "#rGrpDlg" + id, gv.drawPanel ),
        JqHtml( "#rGrpChooser", gv.drawSelect() )
      )

    case "/group/edit" =>
      web.js( JqHtml( "#rGrpMain" + id, gv.drawEdit ) )

    case "/group/editSave" =>
      if ( !dg.b( 'builtin ) ) {
        dg( 'name )    = web.s( "rGrpRenameName" + id ) or "Unnamed Group"
        dg.updateMode( web.b( "rGrpMonitor" + id ) )
        dg.save
        gv.resetGroups
      }

      web.js(
        JqHtml( "#rGrpDlg" + id, gv.drawPanel ),
        JqHtml( "#rGrpChooser", gv.drawSelect() )
      )

    case "/group/deleteGroup" =>
      if ( !dg.b( 'builtin ) ) {
        //Group.remove( Mobj( "_id" -> dg.id ) )
        Tid.deleteCascade( dg.tid )
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

      if ( ab != null && dg != null && !dg.b( 'builtin ) ) {

        val tids:Seq[String] =
          ab.label match {
          case "Name" => // TODO:  should match on something better
            web.a_?( 'addTids )

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

            ofEntity.db.find( where, Mobj( "_id" -> 1 ) ).map( of => groupType.ofEntity.idToTid( of( '_id ) ) ).toSeq
          }

        // DRAGON-MIXED-TID:  this is a hack because we've got mixed tids inside groups (both Org and ExtendedOrg tids) ... there should only be Org tids!
        //                    TODO:  remove all references to DRAGON-MIXED-TID once this problem is cleared up
        assert( tids.forall( _.startsWith( groupType.ofEntity.tid ) ) )

        dg( "tids" ) = ( dg.a_?( 'tids ) ++ tids ).distinct.toMlist
        dg.updateIds
        dg.save
      }

      web.js( JqHtml( "#rGrpMain" + id, gv.drawGroup ) )

    case "/group/remove" =>
      if ( !dg.b( 'builtin ) ) {
        val tid = web.s( 'id )

        // TODO:  we should be able to do both of these in a single update, but need to figure out how to do two $pulls in a single update's DBObject ... does $and work ?
        Group.db.update( Mobj( "_id" -> dg.id ), Mobj( $pull -> Mobj( "tids" -> tid ) ) )
        Group.db.update( Mobj( "_id" -> dg.id ), Mobj( $pull -> Mobj( idsField -> Tid.tidToId( tid ) ) ) )

        // DRAGON-MIXED-TID
        if ( tid.startsWith( ofEntity.tid ) ) // bad !
          Group.db.update( Mobj( "_id" -> dg.id ), Mobj( $pull -> Mobj( "tids" -> groupType.ofEntity.idToTid( Tid.tidToId( tid ) ) ) ) )

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
            map( o => Map( "id"    -> groupType.ofEntity.idToTid( o( '_id ) ),
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
  def groupsFor( tid:String ) = groups.filter( _.a_?( 'tids ).contains( tid ) ).map( _.s( 'name ) ).mkString( ", " )

  def byId( id:Any ) = groups.find( _.id == id ).get

  var selectedGroupTid:String = null
  def get          = selectedGroupTid
  def set( v:Any ) = selectedGroupTid = v._s

  def selectedGroupId = Group.tidToId( selectedGroupTid )
  def selectedGroup = Group( groups.find( g => g.tid == selectedGroupTid ).getOrElse( null ) )
  def selectGroup( tid:String ) { selectedGroupTid = tid; dialogGroupTid = tid }

  var dialogGroupTid:String = null
  def dialogGroupId = Group.tidToId( dialogGroupTid )
  def dialogGroup = Group( groups.find( g => g.tid == dialogGroupTid ).getOrElse( null ) )
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
          Text( group.s( 'name ) + ' ' ) ++
          group.about ++
          ( editable |* Text( " " ) ++ <a href="#" id={ "rGrpEdit" + gf.id } class="rGrpEditLink" style="font-size:12px;">edit</a> ) ++
          group.eye
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
            <td>{ editable && !group.isOwner( el.tid ) |* <a href="#">remove</a> }</td>
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
      <div class="title">Add { gf.groupType.ofEntity.label.plural }</div>
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
    <div class="stitle">Enter { gf.groupType.ofEntity.label } { addBy.label.plural } To Add</div> ++
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
      <div style="padding:8px 0; width:130px;">
       { Checkbox( "rGrpMonitor" + gf.id, false ) }
       <label for={ "rGrpMonitor" + gf.id }>Monitor Group</label>
       { Help( GroupMode.monitorHelp ) }
      </div>
      <div class="btns" style="width:370px;"><a href="#" class="rGrpAddGrpSave go btn">Add Group</a></div>
     </form>
    </div>
    <div class="btns">
     <button onclick={ "$('#rGrpDlg" + gf.id + "').dialog('close'); return false;" } class="btn" style="float:right;">Cancel</button>
    </div>;

  def drawEdit =
    <div class="rGrpEdit">
     <div class="title" style="margin-bottom:16px;">Edit Group</div>
     <form method="post">
      <label for={ "rGrpRenameName" + gf.id }>Enter Group Name:</label>
      <div class="title"><input type="text" class="rGrpRenameName" name={ "rGrpRenameName" + gf.id } id={ "rGrpRenameName" + gf.id } style="font-size:20px;" value={ dialogGroup.s( 'name ) }/></div>
      <div style="padding:8px 0; width:130px;">
       { Checkbox( "rGrpMonitor" + gf.id, dialogGroup.monitor ) }
       <label for={ "rGrpMonitor" + gf.id }>Monitor Group</label>
       { Help( GroupMode.monitorHelp ) }
      </div>
      <div class="btns" style="width:370px;"><a href="#" class="rGrpEditSave go btn">Save</a></div>
     </form>
    </div>
    <div class="btns">
     <button onclick={ "$('#rGrpDlg" + gf.id + "').dialog('close'); return false;" } class="btn" style="float:right;">Cancel</button>
    </div>;
}

object Grouplet extends Weblet {

  def handle( web: WebContext ) {
    //val sess = T.session

    rpath match {

    case _ =>
      _404
    }
  }
}

