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
import scala.collection.mutable.Buffer
import scala.xml.{ NodeSeq, Text, Unparsed }

import java.io.File

import org.bson.types.ObjectId

import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.content.{ ContentMeta, Content, ContentType }
import org.tyranid.db.{ DbArray, DbBoolean, DbChar, DbInt, DbLong, DbLink, DbTid, DbUrl, Entity, Record, Scope }
import org.tyranid.db.meta.{ Tid, TidItem }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity, MongoRecord }
import org.tyranid.db.ram.RamEntity
import org.tyranid.db.tuple.{ Tuple, TupleView }
import org.tyranid.http.Http
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


object GroupType extends RamEntity( tid = "a0Nt" ) {
  type RecType = GroupType
  override def convert( view:TupleView ) = new GroupType( view )

  "_id"    is DbInt      is 'id;
  "name"   is DbChar(64) is 'label;


  override val addNames = Seq( "_id", "name" )

  val Org  = add( 1, "Org" )
  val User = add( 2, "User" )
}

case class GroupType( override val view:TupleView ) extends Tuple( view ) {

  lazy val ofEntity =
    this match {
    case GroupType.Org  => B.Org
    case GroupType.User => B.User
    case _              => problem( "invalid group type" )
    }

  def iconClass16x16 =
    this match {
    case GroupType.Org  => "linkIcon connectionsIcon"
    case GroupType.User => "linkIcon contactsIcon"
    }

  def iconClass32x32 =
    this match {
    case GroupType.Org  => "headerIcon headerConnectionsIcon"
    case GroupType.User => "headerIcon headerContactsIcon"
    }
}


object GroupMode extends RamEntity( tid = "a0Ot" ) {
  type RecType = GroupMode
  override def convert( view:TupleView ) = new GroupMode( view )

  "_id"    is DbInt      is 'id;
  "name"   is DbChar(64) is 'label;


  def monitorHelp =
    Text( "Monitor groups are groups that are not visible to their members, and are used only for personal or organizational purposes.  They are generally not used for collaboration." )

  override val addNames = Seq( "_id", "name" )

  val Monitor       = add( 1, "Monitor"   )
  val Moderated     = add( 2, "Moderated" )
  val Collaborative = add( 3, "Open"      ) // A collaborative group is one which everyone inside the group can see each others things.
}

case class GroupMode( override val view:TupleView ) extends Tuple( view )


object Group extends MongoEntity( tid = "a0Yv" ) with ContentMeta {
  type RecType = Group
  override def convert( obj:DBObject, parent:MongoRecord ) = new Group( obj, parent )



  //"color"          // future ... colored labels
  //"search"         { search criteria } // future ... list search for a group, rather than each id explicitly


  db.ensureIndex( Mobj( "o" -> 1, "name" -> 1 ) )

  def flatten( tids:Seq[String] ) =
    tids.
      flatMap(
      _ match {
     case tid if tid.startsWith( Group.tid ) =>
       Group.byTid( tid ).flatten( _.a_?( 'v ).toSeq.of[String], Nil )

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
             grp.a_?( 'v ).toSeq.of[String]
           else
             Seq( tid )
         },
         Nil
       )

     case tid =>
       Seq( tid )
     } ).
     distinct

  def ownedBy( orgTid: String ) = {
    val tids = Mobj( $in -> Array( orgTid ) )
    db.find( Mobj( "o" -> tids ) ).map( apply ).toSeq
  }
  
  def canSeeOther( orgTid:String ) = ownedBy( orgTid ).filter( g => Group( g ).canSee( T.user ) )
  
  def visibleTo( user:User, contentType:ContentType = ContentType.Group ) = {
    val tids =
      if ( user.org != null ) Mobj( $in -> Array( user.tid, user.org.tid ) )
      else                    user.tid

    val myGroups =
      db.find(
        Mobj( "o" -> tids,
              "type" -> contentType.id )
      ).map( apply ).toSeq

    val memberGroups =
      db.find( 
        Mobj( "v" -> tids,
              "groupMode" -> Mobj( $ne -> GroupMode.Monitor.id ),
              "type" -> contentType.id )
      ).map( apply ).toSeq.filter( memberGroup => !myGroups.exists( _.id == memberGroup.id ) )

    myGroups ++ memberGroups 
  }
}

class Group( obj:DBObject, parent:MongoRecord ) extends Content( Group.makeView, obj, parent ) {

  def name     = s( 'name )
  def fullName = name + " (" + ( isOwner( T.user ) ? "me" | ownerNames ) + ")"

  override def id:ObjectId = super.apply( "_id" ).as[ObjectId]
  

  def groupMode = GroupMode.byId( i( 'groupMode ) ).orNull

  def monitor       = groupMode == GroupMode.Monitor
  def moderated     = groupMode == GroupMode.Moderated
  def collaborative = groupMode == GroupMode.Collaborative

  // TODO:  remove this method once it is no longer used
  def updateMode( monitor:Boolean ) = {
    this( 'groupMode ) =
      if ( monitor )                         GroupMode.Monitor.id
      else if ( groupType == GroupType.Org ) GroupMode.Moderated.id
      else                                   GroupMode.Collaborative.id
  }

  def idsForEntity( en:Entity ) = a_?( 'v ).map( _._s ).filter( _.startsWith( en.tid ) ).map( en.tidToId )
  def idsForGroupType = idsForEntity( groupType.ofEntity )

  def groupType = GroupType.byId( i( 'groupType ) ).getOrElse( GroupType.Org ).as[GroupType]


  def iconClass16x16 = groupType.iconClass16x16
  def iconClass32x32 = groupType.iconClass32x32

  def canSee( member:Record ):Boolean = canSee( T.user, member )

  def canSee( viewer:User, member:Record ) = {
    val owner = isOwner( viewer ) 

    if ( owner ) {
      true
    } else {
      groupMode match {
      case GroupMode.Monitor =>
        false

      case GroupMode.Moderated =>
        isReader( viewer ) &&
        ( groupType match {
          case GroupType.Org  => member.tid == viewer.tid || isOwner( member.tid ) || owner || member.tid == viewer.orgTid || B.Org.orgIdFor( member ) == viewer.orgId
          case GroupType.User => member.tid == viewer.tid || isOwner( member.tid ) || owner
          } )

      case GroupMode.Collaborative =>
        isReader( viewer )
      }
    }
  }
  
  def about = {
    val sb = new StringBuilder

    sb ++= "<i style=\"font-size:80%;\">("

    if ( b( 'builtin ) )
      sb ++= "Built-in "

    sb ++= groupMode.label += ' '
    sb ++= groupType.label
    sb ++= " Group)</i>"

    Unparsed( sb.toString )
  }
  
  override def imageForThumbs:File = {
    val file = super.imageForThumbs
    ( file != null ) ? file | B.getS3Bucket( "public" ).file( "images/default_project_image.png" )
  }  

    // TODO: Replace this with Carl's pallete
  def getRandomColor = { 
    import java.util.Random
    val random = new Random()
    val hue = random.nextFloat()
    val saturation = random.nextFloat()
    val luminance = random.nextFloat()
    val color = java.awt.Color.getHSBColor(hue, saturation, luminance)
    color.darker()
    color.darker()
    color.darker()
    Integer.toHexString( color.getRGB() ).substring(2, 8)
  }

  override def thumbHtml( size:String ) = {
    val imageUrl = s( 'img )
    val style:String = imageUrl.isBlank ? {
      var color = s( 'color )
      
      if ( color.isBlank ) {
        color = getRandomColor
        this( 'color ) = color
        save
      } 
      
      "background-color:#" + color 
    } | null

    val projectSettings = this.settingsFor( T.user )
    val newBox = size == "l" && !projectSettings.hasVisited // Only care about this if it is a large thumb
    
    val inner = 
      <div class={ thumbClass( size ) } style={ style }>
       { imageUrl.notBlank ? <img src={ "/io/thumb/" + tid + "/" + size }/> | <div class="text">{ s( 'name ) }</div> }
      </div>

    newBox ? <div class="newBox">{ inner }<div class="newOverlay"><div class="text">NEW</div></div></div> | inner
  }
  
  def settingsFor( user:User ) = GroupSettings( GroupSettings.db.findOrMake( Mobj( "u" -> user.id, "g" -> this.id ) ) )
}

/*
 * * *  GroupMaker
 */


case class GroupMaker( groupType:GroupType, 
                       ofEntity:MongoEntity,
                       addBys:Seq[GroupingAddBy] = Nil,
                       nameSearch: ( String ) => Any = null ) {

  def queryGroupMembers( group:Group ) =
    ofEntity.db.find( Mobj( "_id" -> Mobj( $in -> group.idsForGroupType ) ) ).map( ofEntity.apply ).toIterable
}

case class GroupingAddBy( label:String, keys:String* ) {
  val id = label.toIdentifier
}



/*
 * * *  GroupField
 */

case class GroupField( baseName:String, l:String = null,
                       makers:Seq[GroupMaker],
                       opts:Seq[(String,String)] = Nil,
                       data:Boolean = true ) extends Field {

  val id = Base62.make( 8 )
  val name = baseName + "$cst"

  val default = None

  val search = Search.Custom

  val showFilter = true
  val show = Show.Editable

  def groupValueFor( rec:Record ) = rec( name ).as[GroupValue]

  def makerFor( grp:Group ) = makers.find( _.groupType == grp.groupType ).get

  override lazy val label = if ( l.notBlank ) l else "Group"

  override def init( rec:Record ) = {
    val gv = GroupValue( this )
    gv.set( super.init( rec ) )
    rec( name ) = gv
  }

  override def ui( s:Scope ) = groupValueFor( s.rec ).selectUi( "" )

  override def extract( s:Scope ) = groupValueFor( s.rec ).selectedGroupTid = T.web.s( id )

  override def cell( s:Scope ) =
    groupValueFor( s.run.report.searchRec ).groupsFor( s.rec.tid ).toNodeSeq

  override def mongoSearch( run:Run, searchObj:DBObject, value:Any ) = {
    if ( value != null ) {
      val group = groupValueFor( run.report.searchRec ).selectedGroup
      if ( group != null )
        searchObj( baseName ) = Mobj( $in -> group.idsForGroupType.toMlist )
    }
  }

  def matchesSearch( run:Run, value:Any, rec:Record ):Boolean =
    // TODO:  implement this
    false

  def closeJs =
    // TODO:  the grpNavDrag should be less hard-coded
    "$('#grpDlg" + id + "').dialog('close'); grpNavDrag( 'redraw' ); return false"


  override def drawPreamble:NodeSeq =
    <head id="tag.js"><script src={ B.buildPrefix + "/js/tag.js" } charset="utf-8"></script></head>
    <div id={ "grpDlg" + id } class="grpDlg" style="padding:0; display:none;"/>;

  override def drawFilter( run:Run ) = {
    val groupValue = groupValueFor( run.report.searchRec )
    
    <table class="tile" style="width:180px; height:54px;">
     <tr>
      <td class="vlabel">view group</td>
      <td rowspan="2" style="padding-right:4px;">
       <a id={ "grpBtn" + id } href="#" class="grpBtn btn" style="height:40px; padding-top:6px;">
        <span tip="Groups" class={ "tip " + /* bigIcon groupIcon */ makers.head.groupType.iconClass32x32 } style="padding:0;"/>
        <span class="vlabel"/>
       </a>
      </td>
     </tr>
     <tr>
      <td id="grpChooser">{ groupValue.selectUi() }</td>
     </tr>
    </table>
  }

  def accessTidsQuery = {
    val s = T.session
    if ( s.orgTid.notBlank ) Mobj( $in -> Mlist( s.orgTid, s.user.tid ) )
    else                     s.user.tid
  }

  def queryGroups =
    Group.db.find(
      Mobj(
        "o" -> accessTidsQuery,
        "groupType" -> ( if ( makers.length > 1 ) Mobj( $in -> makers.map( _.groupType.id ).toMlist ) else makers.head.groupType.id )
      )
    ).map( Group.apply ).toSeq

  override def handle( weblet:Weblet, rec:Record ) = {
    val gv = groupValueFor( rec )

    val web = T.web
    val dg = gv.dialogGroup

    weblet.rpath match {
    case "/fld" =>
      gv.resetGroups

      /*
      if saving
      GroupFavorite.saveSelected

      */

      web.js( JqHtml( "#grpDlg" + id, gv.panelUi ) )

    case "/fld/editTab" =>
      gv.tab = "/edit"
      web.js( JqHtml( "#grpDlg" + id, gv.panelUi ) )

    case "/fld/favTab" =>
      gv.tab = "/fav"
      web.js( JqHtml( "#grpDlg" + id, gv.panelUi ) )

    case "/fld/availType" =>
      gv.availType = web.s( 'availType )
      web.js( JqHtml( "#grpFavAvail", GroupFavorite.favAvailableUi( gv.availType ) ) )

    case "/fld/favDrag" => 
      GroupFavorite.favDrag( gv, web )

    case "/fld/select" =>
      val report = T.session.reportFor( T.web.s( 'q ) )
      var query = report.query
      
      query.init
      report.extractSearchRec

      // May need to do something here with sort order
      //if ( query.orderBy.nonEmpty ) {
      //  val name = web.s( 'sort )
      //  report.sort = query.orderBy.find( _.name == name ).get
      //}
      
      report.offset = 0
      
      gv.selectGroup( web.s( 'id ) )
      web.res.html( report.innerDraw )

    case "/fld/dlgSelect" =>
      gv.setDialogGroup( web.s( 'id ) )
      web.js( JqHtml( "#grpMain" + id, gv.groupUi ) )

    case "/fld/addGroup" =>
      web.js( JqHtml( "#grpMain" + id, gv.addGroupUi ) )

    case "/fld/addGroupSave" =>
      val monitor = web.b( "grpMonitor" + id )
      val group = Group.make

      val gt =
        if ( makers.size == 1 )
          makers.head.groupType
        else
          GroupType.getById( web.i( "grpType" + id ) )

      group( 'name )      = web.s( "grpAddName" + id ) or "Unnamed Group"
      group( 'type )      = ContentType.Group.id
      group( 'groupType ) = gt.id

      group.updateMode( monitor )

      group( 'o ) = Mlist(
        gt match {
        case GroupType.Org  => T.session.orgTid
        case GroupType.User => T.session.user.tid
        } )

      if ( !monitor ) {
        group( 'v ) =
          ( gt match {
            case GroupType.Org  => Seq( T.session.orgTid )
            case GroupType.User => Seq( T.session.user.tid )
            } ).toMlist
      }

      group.save
      gv.resetGroups
      gv.setDialogGroup( Group( group ).tid )

      web.js(
        JqHtml( "#grpDlg" + id, gv.panelUi ),
        JqHtml( "#grpChooser", gv.selectUi() )
      )

    case "/fld/edit" =>
      web.js( JqHtml( "#grpMain" + id, gv.editUi ) )

    case "/fld/editSave" =>
      if ( !dg.b( 'builtin ) ) {
        dg( 'name )    = web.s( "grpRenameName" + id ) or "Unnamed Group"
        dg.updateMode( web.b( "grpMonitor" + id ) )
        dg.save
        gv.resetGroups
      }

      web.js(
        JqHtml( "#grpDlg" + id, gv.panelUi ),
        JqHtml( "#grpChooser", gv.selectUi() )
      )

    case "/fld/deleteGroup" =>
      if ( !dg.b( 'builtin ) ) {
        //Group.remove( Mobj( "_id" -> dg.id ) )
        Tid.deleteCascade( dg.tid )
        gv.resetGroups
      }

      web.js(
        JqHtml( "#grpDlg" + id, gv.panelUi ),
        JqHtml( "#grpChooser", gv.selectUi() )
      )

    case "/fld/addBy" =>
      val abid = web.s( 'v )
      val maker = makerFor( dg )
      gv.groupAddBys( maker ) = null
      maker.addBys.find( _.id == abid ).foreach { gv.groupAddBys( maker ) = _ }
      web.js( JqHtml( "#grpAddBox" + id, gv.addByUi ) )

    case "/fld/addMember" =>
      val maker = makerFor( dg )
      val ab = gv.groupAddByFor( maker )

      if ( ab != null && dg != null && !dg.b( 'builtin ) ) {

        val addTids:Seq[String] =
          ab.label match {
          case "Name" => // TODO:  should match on something better
            web.a_?( 'addTids )

          case _ =>
            val ofEntity = maker.ofEntity
            val keyAtts = ab.keys.map( ofEntity.attrib )

            val altIds = web.s( 'grpAddByInput ).split( "," ).map( _.trim )

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

            ofEntity.db.find( where, Mobj( "_id" -> 1 ) ).map( of => dg.groupType.ofEntity.idToTid( of( '_id ) ) ).toSeq
          }

        // DRAGON-MIXED-TID:  this is a hack because we've got mixed tids inside groups (both Org and ExtendedOrg tids) ... there should only be Org tids!
        //                    TODO:  remove all references to DRAGON-MIXED-TID once this problem is cleared up
        assert( addTids.forall( _.startsWith( dg.groupType.ofEntity.tid ) ) )

        dg( "v" ) = ( dg.a_?( 'v ) ++ addTids ).distinct.toMlist
        dg.save
        addTids foreach { tid => B.groupMemberAdded( dg, tid ) }
      }

      web.js( JqHtml( "#grpMain" + id, gv.groupUi ) )

    case "/fld/remove" =>
      if ( !dg.b( 'builtin ) ) {
        val tid = web.s( 'id )

        Group.db.update( Mobj( "_id" -> dg.id ), Mobj( $pull -> Mobj( "v" -> tid ) ) )

        // DRAGON-MIXED-TID
        if ( tid.startsWith( makerFor( dg ).ofEntity.tid ) ) // bad !
          Group.db.update( Mobj( "_id" -> dg.id ), Mobj( $pull -> Mobj( "v" -> dg.groupType.ofEntity.idToTid( Tid.tidToId( tid ) ) ) ) )

        gv.resetGroups
        B.groupMemberRemoved( dg, tid )
      }

      web.js( JqHtml( "#grpMain" + id, gv.groupUi ) )

    case "/fld/toggleAddBy" =>
      gv.groupShowAddBy = !gv.groupShowAddBy
      web.js( JqHtml( "#grpMain" + id, gv.groupUi ) )

    case "/fld/addSearch" =>
      val terms = web.s( 'term )
      val maker = makerFor( dg )

      val json =
        if ( maker.nameSearch != null ) {
          maker.nameSearch( terms )
        } else {
          val labelKey = maker.ofEntity.labelAtt.get.name
          val regex = terms.toLowerCase.tokenize.map { term => Mobj( labelKey -> Mobj( $regex -> term, $options -> "i" ) ) }
          val where =
            if ( regex.size == 1 ) regex( 0 )
            else                   Mobj( $and -> Mlist( regex:_* ) )

          maker.ofEntity.db.find( where, Mobj( labelKey -> 1 ) ).
            limit( 16 ).
            toSeq.
            map( o => Map( "id"    -> dg.groupType.ofEntity.idToTid( o( '_id ) ),
                           "label" -> o.s( labelKey ) ) )
        }

      web.res.json( json )

    case _ =>
      weblet._404
    }
  }
}

case class GroupValue( gf:GroupField ) extends Valuable {

  private var latestGroups:Seq[Group] = null

  var tab = "/edit"
  var availType:String = null // "g", "m", or blank

  val groupAddBys = mutable.Map[GroupMaker,GroupingAddBy]()

  def groupAddByFor( maker:GroupMaker ) =
    groupAddBys.getOrElseUpdate(
      maker,
      maker.addBys( 0 ) )

  @volatile var groupShowAddBy:Boolean = false

  def resetGroups { latestGroups = null }
  def groups = {
    if ( latestGroups == null )
      latestGroups = gf.queryGroups.toSeq

    latestGroups
  }
  def groupsFor( tid:String ) = groups.filter( _.a_?( 'v ).contains( tid ) ).map( _.s( 'name ) ).mkString( ", " )

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

  def selectUi( cls:String = "grpChr" ) =
    Select( gf.id, selectedGroupTid, ( "" -> "All" ) +: groups.map( g => g.tid -> g.s( 'name ) ), "class" -> cls, "style" -> "width:120px; max-width:120px;" )

  def panelUi = {
    val stid = dialogGroupTid

    org.tyranid.session.Notification.box ++
    <div class="tabbar">
     <ul>
      <li><a id="grpEditTab" href="#" class={ tab == "/edit" |* "selected" }>Edit</a></li>
      <li><a id="grpFavTab"  href="#" class={ tab == "/fav"  |* "selected" }>Favorites</a></li>
     </ul>
    </div> ++
    { tab match {
      case "/edit" =>
        <div class="grpLeft">
         <div class="grpSel">
          <ul class="noSelect">
           { groups.map( g => <li class={ "noSelect" + ( g.tid == stid |* " sel" ) } id={ g.tid }><span style="margin-right:4px;" class={ g.iconClass16x16 } />{ g.s( 'name ) }</li> ) }
          </ul>
         </div>
         <div class="btns">
          <button class="grpAddGrp btn-success btn" style="float:left;" id="addGroupOpen">Add Group</button>
         </div>
        </div>
        <div id={ "grpMain" + gf.id } class="grpMain">
         { groupUi }
        </div>

      case "/fav" =>
        GroupFavorite.favUi( this )
      }
    }
  }

  def groupUi = {
    val group = dialogGroup
    val editable = group != null && !group.b( 'builtin )
    val showAddBy = groupShowAddBy
    val maker = if ( group != null ) gf.makerFor( group ) else null

    <div class={ "grpEdit " + ( showAddBy ? "shortTable" | "longTable" ) }>
     <div class="title">
      { if ( group != null )
          Text( group.s( 'name ) + ' ' ) ++
          group.about ++
          ( editable |* Text( " " ) ++ <a href="#" id={ "grpEdit" + gf.id } class="grpEditLink" style="font-size:12px;">edit</a> ) ++
          group.eye
        else
          <em>None selected</em> }
     </div>
     { group != null |*
     <div class="list">
      <table class="dtable">
       <thead>
        <tr>
         <th>Name</th>{ if ( !showAddBy ) maker.addBys.filter( _.label != "Name" ).map( ab => <th>{ ab.label }</th> ) }{ editable |* <th/> }
         <th style="width:10px;"/>
        </tr>
       </thead>
       { val members = maker.queryGroupMembers( group ).toSeq.sortBy( _.label )
         for ( el <- members ) yield
           <tr id={ el.tid }>
            <td>{ el.label }</td>
            { if ( !showAddBy ) maker.addBys.filter( _.label != "Name" ).map( ab => <td>{ el.s( ab.keys( 0 ) ) }</td> ) }
            <td>{ editable &&
                   // TODO:  this should really be something like: && !group.isLASTOwner( el.tid )
                   !group.isOwner( el.tid ) |* <a href="#">remove</a> }</td>
            <td/>
           </tr>
       }
      </table>
     </div> }
    </div> ++
    { group != null && showAddBy |*
    <div class="add">
     { editable |*
     <form method="post" id="grpAddForm">
      <div class="title">Add { group.groupType.ofEntity.label.plural }</div>
      <label for="grpAddBy">By:</label>
      { val addBy = groupAddByFor( maker )
        Select( "grpAddBy", addBy != null |* addBy.id, maker.addBys.map( ab => ( ab.id, ab.label ) ) ) }
      <div id={ "grpAddBox" + gf.id } class="grpAddBox">
       { addByUi }
      </div>
     </form> }
    </div> } ++
    <div class="btns">
     { editable |* <button class="grpDelGrp btn-danger btn" style="float:left;">Delete</button> }
     <button onclick={ gf.closeJs } class="btn" style="float:right;" id="doneBtn">Done</button>
     { editable || showAddBy |* <button class="grpToggleAddBy btn-success btn" style="float:right;">{ if ( showAddBy ) "Show Table" else "Add Members" }</button> }
    </div>
  }

  def addByUi = {
    val group = dialogGroup
    val addBy = groupAddByFor( gf.makerFor( group ) )

    addBy != null |* {
    <div class="stitle">Enter { group.groupType.ofEntity.label } { addBy.label.plural } To Add</div> ++
    { addBy.label match {
    case "Name" => // TODO:  should match on something better
      <ul class="grpAddName" id={ "grpAddName" + gf.id }></ul>

    case ab =>
      <div class="note">(separate multiple entries with commas)</div>
      <textarea id="grpAddByInput" name="grpAddByInput" style="height:292px; width:322px;"/>
    } } ++
    <div class="btns"><a class="grpAddImport btn-success btn">Add</a></div>
    }
  }

  def addGroupUi =
    <div class="grpEdit">
     <div class="title" style="margin-bottom:16px;">Add New Group</div>
     <form method="post" id="newGroup">
      <label for={ "grpAddName" + gf.id }>Enter Group Name:</label>
      <div class="title"><input type="text" class="grpAddName" name={ "grpAddName" + gf.id } id={ "grpAddName" + gf.id } style="font-size:20px;"/></div>
      { gf.makers.size > 1 |*
      <div style="padding:8px 0; width:130px;">
       <label for={ "grpType" + gf.id }>Group Type:</label>
       { Select( "grpType" + gf.id, null, gf.makers.map( m => m.groupType.id.toString -> m.groupType.label ), "style" -> "width:120px; max-width:120px;" ) }
      </div> }
      <div style="padding:8px 0; width:130px;">
       { Checkbox( "grpMonitor" + gf.id, false ) }
       <label for={ "grpMonitor" + gf.id }>Monitor Group</label>
       { Help( GroupMode.monitorHelp ) }
      </div>
      <div class="btns" style="width:370px;"><a href="#" class="grpAddGrpSave btn-success btn" id="addGroupSave">Add Group</a></div>
     </form>
    </div>
    <div class="btns">
     <button onclick={ "$('#grpDlg" + gf.id + "').dialog('close'); return false;" } class="btn" style="float:right;">Cancel</button>
    </div>;

  def editUi =
    <div class="grpEdit">
     <div class="title" style="margin-bottom:16px;">Edit Group</div>
     <form method="post">
      <label for={ "grpRenameName" + gf.id }>Enter Group Name:</label>
      <div class="title"><input type="text" class="grpRenameName" name={ "grpRenameName" + gf.id } id={ "grpRenameName" + gf.id } style="font-size:20px;" value={ dialogGroup.s( 'name ) }/></div>
      <div style="padding:8px 0; width:130px;">
       { Checkbox( "grpMonitor" + gf.id, dialogGroup.monitor ) }
       <label for={ "grpMonitor" + gf.id }>Monitor Group</label>
       { Help( GroupMode.monitorHelp ) }
      </div>
      <div class="btns" style="width:370px;"><a href="#" class="grpEditSave btn-success btn">Save</a></div>
     </form>
    </div>
    <div class="btns">
     <button onclick={ "$('#grpDlg" + gf.id + "').dialog('close'); return false;" } class="btn" style="float:right;">Cancel</button>
    </div>;
}

object GroupFavorite extends MongoEntity( tid = "a0Iv" ) {
  type RecType = GroupFavorite
  override def convert( obj:DBObject, parent:MongoRecord ) = new GroupFavorite( obj, parent )

  "groups"         is DbArray(DbLink(Group)) ;

  override def init {
    super.init
    "_id"          is DbLink(B.User)         is 'id is 'owner;
  }

  db.ensureIndex( Mobj( "user" -> 1 ) )


  val availTypes = Map(
    "g" -> "My Groups",    // my groups
    "m" -> "Member Groups" // groups we are a member of
  )


  val FAV_SEL_KEY = "GRP_FAV_SEL"   // the user's favorites currently selected in the dialog

  def favorites = {
    val sess = T.session
    val u = sess.user

    var favorites = GroupFavorite.getById( u.id )

    if ( favorites == null ) {
      favorites = GroupFavorite.make
      favorites( '_id ) = u.id
      favorites.setDefaults
      favorites.save
    } else if ( favorites.a_!( 'groups ).size == 0 ) {
      favorites.setDefaults
      favorites.save
    }

    favorites
  }

  def selections = {
    val sess = T.session
    var sels = sess.get( FAV_SEL_KEY ).as[Buffer[String]]
    
    if ( sels == null ) {
      sels = Buffer( favorites.a_?( 'groups ).toSeq.of[ObjectId].map( Group.idToTid ):_* )
      sess.put( FAV_SEL_KEY, sels )
    }
    
    sels
  }
  
  def available = B.availableFavorites.sortBy( _.name.toLowerCase )

  def saveSelected {
    val sels = selections
    
    clearFav

    val f = favorites
    f( 'groups ) = sels.map( Group.tidToId ).toMlist
    f.save
  }



  /*
   * * *    Favorite Groups Dialog (*fav*)
   */
  
  def clearFav = T.session.clear( FAV_SEL_KEY )
  
  def favUi( gv:GroupValue ) = {
    val selectedType = T.web.s( 'grpAvailType ) or ""
    
    <div class="favDlg noSelect">
     <div class="favLeft">
      <h2>Current</h2>
      <div class="favSel" id="grpFavSel">
       { favSelectedUi }
      </div>
     </div>
     <div class="favGroup">
      <h2>Available: { Select( "grpAvailType", selectedType, ( "" -> "- All Types -" ) +: availTypes.toSeq ) }</h2>
      <div class="favSel" id="grpFavAvail">
      { favAvailableUi( gv.availType ) }
      </div>
     </div>
     <div style="display:inline-block; width:258px; padding-right:2px;">
      <button onclick={ gv.gf.closeJs } class="btn" style="margin-top:436px; float:right;">Done</button>
     </div>
    </div>
  }
  
  def favSelectedUi = {
    val orgId = T.session.user.org.id.as[ObjectId]

    <ul class="noSelect">
     { selections.map( Group.getByTid ).filter( g => g != null ).map( g => <li class='noSelect cgf' id={ g.tid }><span class={ g.iconClass16x16 } /><span class="vlabel"> { g.fullName } { Tid.eye( g.tid ) }</span></li> ) }
    </ul>
  }

  def favAvailableUi( availType:String ) =
    <ul class="noSelect">
     { ( availType match {
         case at if at.isBlank => available
         case "g"              => available.filter( _.isOwner( T.user ) )
         case "m"              => available.filter( !_.isOwner( T.user ) )
         } ).
         map( g => <li class='noSelect cgf' id={ g.tid }><span class={ g.iconClass16x16 } /> <span class="vlabel">{ g.fullName } { Tid.eye( g.tid ) }</span></li> ) 
     }
    </ul>

  def favDrag( gv:GroupValue, web:WebContext ) {
    val ( fromId, toId ) = web.s( 'js ).splitFirst( ':' )
  
    val avails = available
    val sels   = selections
    
    toId match {
    case "grpFavAvail" => // Removing a current one
      val o = sels.find( _ == fromId ).get
      sels -= o
    case "grpFavSel"   => // Adding one to the end
      val o = avails.find( _.tid == fromId ).get
      sels += o.tid
    case id            => // Reordering one
      val fromCurrent = sels.find( _ == fromId ) != None
       
      if ( fromCurrent )
        sels -= fromId

      val to = sels.find( _ == toId ).get
      sels.insert( sels.indexOf( to ), fromId )
    }

    GroupFavorite.saveSelected

    web.js(
      JqHtml( "#grpFavSel",   GroupFavorite.favSelectedUi ),
      JqHtml( "#grpFavAvail", GroupFavorite.favAvailableUi( availType = gv.availType ) ) )
  }
  

  /*
   * * *   Group Navigation (*nav*)
   */
  
  def navUi =
    { B.commonGroupField.drawPreamble } ++
    <section class="grpNav">
     { innerNavUi }
    </section>
  
  def innerNavUi:NodeSeq = {
    val user = T.session.user
    val org = user.org

    <h1 id="grpNavRemove"><span>GROUPS <a href="#" id={ "grpBtn" + B.commonGroupField.id } tip="Configure Groups" class="tip grpBtn"><span class="linkIcon plusGearIcon"/></a></span>
    </h1> ++
    { for ( g <- favorites.groups; if ( g != null ); firstOwnerTid = g.firstOwnerTid( null ); if firstOwnerTid.notBlank ) yield {
        val isOwner = g.isOwner( user )
        <a class={ "gf" + ( !isOwner |* " memberGroup tip" ) } tip={ !isOwner |* ( "Owned by " + TidItem.by( firstOwnerTid ).name ) } id={ g.tid } href={ "/group?g=" + g.tid }>
         <span class={ g.iconClass16x16 }/><span class="vlabel">{ g.name } </span>
        </a>
      }
    }
  }
  
  def navIconUi = {
    val user = T.session.user
    val org = user.org

    <section>
     <h1>GR</h1>
     { for ( g <- favorites.groups; if ( g != null ); firstOwnerTid = g.firstOwnerTid( null ); if firstOwnerTid.notBlank ) yield {
        val isOwner = g.isOwner( user )
        <a class={ "tip gf" + ( !isOwner |* " memberGroup" ) } tip={ isOwner ? g.name | ( g.name + " - Owned by " + TidItem.by( firstOwnerTid ).name ) } id={ g.tid } href={ "/group?g=" + g.tid }>
         <span class={ g.iconClass16x16 }/>
        </a>
     } }
    </section>
  }

  def navDrag( web:WebContext ) = {
    val js = web.s( "js" )
    
    if ( js != "redraw" ) {
      val ( fromTid, toTid ) = js.splitFirst( ':' )
      
      val favs = favorites
      val groups = favs.groups.toBuffer.filter( g => g != null )
      
      val from = groups.find( _.tid == fromTid ).get
      groups -= from

      toTid match {
      case "grpNavRemove" =>

      case _ =>  
        val to = groups.find( _.tid == toTid ).get
        groups.insert( groups.indexOf( to ), from )
      }

      favs( 'groups ) = groups.map( _.id ).toMlist
      favs.save
      
      clearFav
    }

    web.html( innerNavUi )
  }
}

class GroupFavorite( obj:DBObject, parent:MongoRecord ) extends MongoRecord( GroupFavorite.makeView, obj, parent ) {
  def groups = a_?( 'groups ).map( Group.getById )
  
  def setDefaults = {
    val defFavs = B.defaultFavorites

    if ( defFavs.nonEmpty )
      obj( 'groups ) = defFavs.toMlist
  }
}

object Grouplet extends Weblet {

  def handle( web: WebContext ) {
    val sess = T.session

    redirectIfNotLoggedIn( web )

    rpath match {
    case "/nav/drag" => 
      redirectIfNotHasOrg( web )
      GroupFavorite.navDrag( web )
    
    case _ =>
      val fld = web.s( 'fld )

      if ( fld == B.commonGroupField.id ) {
        val groupRec = sess.cache.getOrElseUpdate(
          "groupRec", {
            val rec = org.tyranid.db.AdHoc.make
            rec( B.commonGroupField.name ) = GroupValue( B.commonGroupField )
            rec
          }
        ).as[Record]

        B.commonGroupField.handle( this, groupRec )
      } else {
        val queryId = web.s( 'q )
        if ( queryId.notBlank ) {
          val report = sess.reportFor( queryId )
          val query = report.query

          query.fields.find( _.id == fld ) match {
          case Some( f ) => f.handle( this, report.searchRec )
          case None      => _404
          }
        } else {
          _404
        }
      }
    }
  }
}


/*
 * * *  Group Settings
 */

object GroupSettings extends MongoEntity( tid = "a0Rt" ) {
  type RecType = GroupSettings
  override def convert( obj:DBObject, parent:MongoRecord ) = new GroupSettings( obj, parent )

  override def init {
    super.init

  "_id"            is DbMongoId                              ;

  "u"              is DbLink(B.User)                         as "User";
  "g"              is DbLink(Group)                          as "Group";

  "order"          is DbArray(DbTid( B.ContentEntities:_* )) as "Ordering";
  
  "flags"          is DbLong                                 as "Flags";
  }

  db.ensureIndex( Mobj( "g" -> 1, "u" -> 1 ) )
  
  val FLAG_VISITED = 1
}

class GroupSettings( obj:DBObject, parent:MongoRecord ) extends MongoRecord( GroupSettings.makeView, obj, parent ) {

  def order = a_?( 'order )

  def update( content:Seq[Content] ) = {

    val oldOrder = order
    val newOrder = Buffer[Content]()

    for ( tid <- oldOrder;
          c <- content.find( _.tid == tid ) )
      newOrder += c

    for ( c <- content )
      if ( !newOrder.exists( _.tid == c.tid ) )
        newOrder += c

    if (   oldOrder.size != newOrder.size
        || ( 0 until oldOrder.size ).exists( i => oldOrder( i ) != newOrder( i ).tid ) ) {
      obj( 'order ) = newOrder.map( _.tid ).toMlist
      save
    }

    newOrder
  }
  
  def hasVisited =
    ( l( 'flags ) & GroupSettings.FLAG_VISITED ) == GroupSettings.FLAG_VISITED
  
  def setVisited = this( 'flags ) = l( 'flags ) | GroupSettings.FLAG_VISITED
}

