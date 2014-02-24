/**
 * Copyright (c) 2008-2014 Tyranid <http://tyranid.org>
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
import java.util.{ Calendar, Date }

import org.bson.types.ObjectId

import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.content.{ ContentMeta, Content, ContentEdit, ContentType }
import org.tyranid.db.{ DbArray, DbBoolean, DbChar, DbDate, DbDouble, DbInt, DbLong, DbLink, DbTid, DbUrl, Entity, Record, Scope }
import org.tyranid.db.meta.{ Tid, TidItem }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity, MongoRecord }
import org.tyranid.db.ram.RamEntity
import org.tyranid.db.tuple.{ Tuple, TupleView }
import org.tyranid.http.Http
import org.tyranid.json.JqHtml
import org.tyranid.math.Base62
import org.tyranid.report.{ Report, Run }
import org.tyranid.time.Time
import org.tyranid.ui.{ Checkbox, Field, Help, Select, Search, Show, Valuable }
import org.tyranid.web.{ WebContext, Weblet }


object GroupCategory extends RamEntity( tid = "a1Nv" ) {
  type RecType = GroupCategory
  override def convert( view:TupleView ) = new GroupCategory( view )

  "_id"     is DbInt      is 'id is 'client;
  "name"    is DbChar(64) is 'label is 'client;

  override val addNames = Seq( "_id", "name" )
  
  add( 1, "Accounting & Finance" )
  add( 2, "HR, Legal & Logistics" )
  add( 3, "Marketing & Analytics" )
  add( 4, "Sales & CRM" )
  add( 5, "Education" )
  add( 6, "Books" )
  add( 7, "Photos" )
  add( 8, "Video & Music" )
  add( 9, "Games" )
  add( 10, "Lifestyle" )
  add( 11, "News" )
  add( 12, "Sports" )
  add( 13, "Creative" )
  add( 14, "Developer" )
  add( 15, "Office" )
  add( 16, "Task Managment" )
  add( 17, "Project Managment" )
  add( 18, "Social Networking" )
  add( 19, "Blogging" )
  add( 20, "Utilities" )  
  add( 21, "Food" )
  add( 22, "Retail" )
  add( 23, "Assets" )
  add( 24, "Other" )
}

case class GroupCategory( override val view:TupleView ) extends Tuple( view )


object Group extends MongoEntity( tid = "a0Yv" ) with ContentMeta {
  type RecType = Group
  
  override def convert( obj:DBObject, parent:MongoRecord ) = new Group( obj, parent )

  "org"          is DbLink(B.Org)                      as "Organization" is 'client;
  //"orgId"        is DbChar(20)                         is 'temporary is 'client computed{ rec => val orgId = rec.oid( 'org ); ( orgId == null ) ? null | B.Org.idToTid( orgId ) };

  "parent"       is DbLink(Group)                      as "Parent" is 'client is 'owner;

  "members"      is DbArray(DbTid(B.Org,B.User,Group)) as "Members" is 'client;
  "private"      is DbBoolean                          is 'client;
  "ssoSynced"    is DbBoolean                          is 'client;

  //"settings"     is GroupSettings                      is 'temporary is 'client computed { _.as[Group].settingsFor( T.user ) }
  
  "canSso"       is DbBoolean                          is 'temporary is 'client computed { _.as[Group].canBeSsoSynced }

  "website"      is DbChar(80)                         is 'temporary is 'client computed { _.as[Group].website }
  "userIdleDays" is DbInt                              is 'temporary is 'client computed { _.as[Group].userIdleDays }

  "onlineCount"  is DbInt                              is 'temporary is 'auth computed { _.as[Group].onlineMembers.size }
  
  "category"     is DbLink(GroupCategory);//              is 'client;
  "categoryId"   is DbInt                              is 'client computed { _.as[Group].i( 'category ) }
  //"search"         { search criteria } // future ... list search for a group, rather than each id explicitly
  
  "tmpl"         is DbBoolean                          is 'client as "Template Project"
  "canComment"   is DbBoolean                          is 'temporary computed { _.as[Group].canComment }
  "ooab"         is DbBoolean                          is 'client as "Only Owner can add boards / sub-projects"

  db.ensureIndex( Mobj( "o" -> 1, "name" -> 1 ) )
  db.ensureIndex( Mobj( "members" -> 1 ) )

  def flatten( tids:Seq[String] ) =
    tids.
      flatMap(
      _ match {
     case tid if tid.startsWith( Group.tid ) =>
       Group.byTid( tid ).pluck( _.a_?( 'v ).toSeq.of[String], Nil )

     case tid =>
       Seq( tid )
     } ).
     distinct

  def ownedBy( orgTid: String ) = {
    val tids = Mobj( $in -> Array( orgTid ) )
    db.find( Mobj( "o" -> tids ) ).map( apply ).toSeq
  }
  
  def visibleTo( user:User ) = {

    def in( tids:Seq[String] ) =
      if ( tids.size == 1 )
        tids( 0 )
      else
        Mobj( $in -> tids.toArray )

    def query( tids:Seq[String] ) = {
      val obj = Mobj(
        $or -> Mlist(
          Mobj( "o" -> in( tids ) ),
          Mobj( "v" -> in( tids ) )
        )
      )

      db.find( obj )
    }

    var tids =
      if ( user.org != null ) Seq( user.tid, user.org.tid )
      else                    Seq( user.tid )


    var visitedGroups = Buffer[Group]()

    // Recursively query groups if we have any groups that can be nested.
    var newGroups = query( tids ).map( apply ).toSeq

    while ( newGroups.nonEmpty ) {
      visitedGroups ++= newGroups

      val inGroups = newGroups.filter( _.canNest ).map( _.tid )

      newGroups =
        if ( inGroups.nonEmpty )
          query( inGroups).map( apply).filter( grp => !visitedGroups.exists( _.id == grp.id ) ).toSeq
        else
          Nil
    }

    visitedGroups
  }

  def publicProjects =
    db.find(
      Mobj(
        "type" -> ContentType.Project.id,
        $or -> Mlist(
          Mobj( "o" -> B.publicGroup.tid ),
          Mobj( "v" -> B.publicGroup.tid )
        )
      )
    ).map( apply ).toSeq

  def ensureVisibility( groupTid:String, tid:String ) {
    val group = Group.getByTid( groupTid )
    val visibleTids = group.a_!( 'v )
    
    if ( !visibleTids.contains( tid ) ) {
      visibleTids.add( tid )
      group.save
    }
  }

  def ensureInOrgGroup( user:User ) {
    assert( user.org != null )
    
    val org = B.Org.getById( user.org.id )
    val grp = Group( Group.db.findOrMake( Mobj( "org" -> org.id, "name" -> org.s( 'name ), "type" -> ContentType.Organization.id ) ) )
    
    if ( grp.isNew ) { 
      grp.a_!( 'o ).add( user.tid )
      grp.a_!( 'v ).add( user.tid )
      grp.save
    } else {
      val viewers = grp.a_!( 'v )
      
      if ( !viewers.contains( user.tid ) ) {
        viewers.add( user.tid )
        grp.save
      }
    }
  }
  
  def getRandomColor = {
    import java.util.Random
    val rand = new Random( System.currentTimeMillis )
    val pick = rand.nextInt( Content.defaultColors.length )
    Content.defaultColors( pick )
    
    /*
    val random = new Random()
    val hue = random.nextFloat()
    val saturation = random.nextFloat()
    val luminance = random.nextFloat()
    val color = java.awt.Color.getHSBColor(hue, saturation, luminance)
    color.darker()
    color.darker()
    color.darker()
    Integer.toHexString( color.getRGB() ).substring(2, 8)
    */
  }
}

class Group( obj:DBObject, parent:MongoRecord ) extends Content( Group.makeView, obj, parent ) {

  def name     = s( 'name )
  def fullName( user:User ) = name + " (" + ( isOwner( user ) ? "me" | ownerNames ) + ")"

  override def id:ObjectId = super.apply( "_id" ).as[ObjectId]
 

  /*
   * * *   Nested Groups
   */

  /**
   * Only teams can be nested currently, not projects.
   */
  def canNest = contentType == ContentType.Team

  def hasParent = get( 'parent ) != null

  def parentGroup = Group.getById( get( 'parent ) )

  def isContainedIn( groupTid:String ):Boolean =
    this.tid == groupTid || {
      val p = parentGroup
      p != null && p.isContainedIn( groupTid )
    }

  def childGroups =
    Group.db.find( Mobj( "parent" -> id ) ).map( Group.apply )
    
  def ownedChildGroups =
    childGroups.filter( _.isOwner( T.user ) )

  def ownedDescendentGroups = {
    val groups = Buffer[Group]()

    def visit( group:Group ) {
      for ( childGroup <- group.ownedChildGroups ) {
        groups += childGroup
        visit( childGroup )
      }
    }

    visit( this )
    groups
  }

  def fullPath( sep:String = " / " ) = {
    val path = new StringBuilder( this.label )
    var parentGroup = this.parentGroup

    while ( parentGroup != null ) {
      path.insert( 0, parentGroup.label + sep )
      parentGroup = parentGroup.parentGroup
    }

    path._s
  }

  // This is for >= startDate, and < endDate
  // So, 1st to the 2nd would only include 1 day
  def compactCapMap( userCaps: mutable.Map[String,Array[Double]], startDate:Date, endDate:Date ) = {
    val capStart = startDate.toMonday    
    var capEnd = endDate.toMonday

    // Since the is a <, then add a week to ensure the start date is not excluded
    capEnd = capEnd.add( Calendar.DAY_OF_YEAR, 7 )
    
//    println( "cap start: "+  capStart.getTime )
//    println( "cap end: "+  capEnd.getTime )
        
    val teamMemberTids = memberTids
    
    // Caps are stored by weeks, so the startDate is always midnight on Monday
    val caps = GroupCapacity.forGroupUsers( tid, teamMemberTids, capStart, capEnd )
    val defCaps = GroupSettings.capMapForGroupUsers( tid, teamMemberTids, 40 )
    
    val startMillis = startDate.getTime
    val endMillis = endDate.getTime
    
    val diffMillis = endMillis - startMillis
    
    val numDays = ( diffMillis / Time.OneDayMs )._i
    
    // This may change-- will have to do something about weekend being zeroed out if it does
    val numWorkDays = 5.0
    
    // Produce a compact map, user to array of caps for each day
    for ( memberTid <- teamMemberTids ) {
      var userCap = userCaps.getOrElse( memberTid, null )
      
      if ( userCap == null ) {
        val defCap = defCaps( memberTid )
        val cap = ( defCap > 0.0 ) ? ( defCap / numWorkDays ) | 0.0
        userCap = Array.fill(numDays){ cap }
        userCaps( memberTid ) = userCap
      }
  
      val myCap = caps.filter( _( 'u ) == memberTid )
      
      for ( i <- 0 until numDays ) {
        val qDate = new Date( startMillis + ( i * Time.OneDayMs ) )
        
        
        if ( qDate.isUtcWeekend ) {
          userCap(i) = 0.0
        } else {
          val mondayMillis = qDate.toMonday.getTime
          
          val foundCap = myCap.find( c => {
            c.t( 'start ).getTime == mondayMillis
          } )
          
          if ( foundCap != None ) {
            val cap = foundCap.get.d( 'cap )
            userCap(i) = ( cap > 0.0 ) ? ( cap / numWorkDays ) | 0.0
          }
        }
      }
    }
  }
  
  def idsForEntity( en:Entity ) = a_?( 'v ).map( _._s ).filter( _.startsWith( en.tid ) ).map( en.tidToId )

  def memberTids =
    contentType match {
    case ContentType.Project => obj.a_?( 'members ).toSeq.of[String]
    case _                   => obj.a_?( 'v ).toSeq.of[String]
    }

  /* Default members are really only used in projects.  Default members are the "official members" of the project.  For example, users that
   * get added to folders inside the project that might not get added to the "default members" list but they would still be in the "v"/members
   * list.
   */
  def members = Record.getByTids( memberTids )
 

  def isTemplate = b( 'tmpl )
  def isSsoSynced = b( 'ssoSynced )
  
  def onlineMembers = { 
    val members = new mutable.ArrayBuffer[Record]
    collectOnlineMembers( members )
    members
  }
  
  override def canComment = !isLocked && !isTemplate
  
  private def collectOnlineMembers( omembers:mutable.ArrayBuffer[Record] ) = {
    val meTid = T.user.tid
    
    def isOnlineMember( userTid:String ) = omembers.exists( _.tid == userTid )
    
    def tidValid( tid:String ) = tid != meTid && B.User.hasTid( tid ) && !isOnlineMember( tid ) && B.User.isOnline( tid )

    members foreach { m =>
      m.tid match {
        case userTid if tidValid( userTid ) =>
          omembers += m
        case groupTid if Group.hasTid( groupTid ) =>
          val g = Group.getByTid( groupTid )

          g.members foreach { gu =>
            if ( tidValid( gu.tid ) )
              omembers += gu
          }
        case _ =>
          // nop
      }
    }
  }

  def canBeSsoSynced:Boolean = T.session.get( "sso" ) != null
  
  override def imageUrl( editing:ContentEdit = null ) =
    contentType match {
    case ContentType.Organization =>
      val org = B.Org.getById( oid( 'org ) )
      org.s( 'thumbnail )

    case ContentType.LiteProject =>
      val content = rec( 'thumbc ).as[Content]
      ( content == null ) ? null | content.imageUrl( editing )

    case _ =>
      super.imageUrl( editing )
    }

  
  override def imageForThumbs:File = {
    val file = super.imageForThumbs
    ( file != null ) ? file | B.getS3Bucket( "public" ).file( "images/default_project_image.png" )
  }  

  val newOverlay = <div class="new-overlay"><span class="text">NEW</span></div>
  val privateOverlay = <div class="private-overlay"><span class="fa fa-minus"></span><span class="text">PRIVATE</span></div>

  override def thumbHtml( size:String, extraHtml:NodeSeq = null ) = {
    val url = imageUrl( null )

    val style:String = url.isBlank ? {
      var color = s( 'color )
      
      if ( color.isBlank ) {
        color = Group.getRandomColor
        this( 'color ) = color
        save
      }
      
      "background-color:#" + color 
    } | null

    val groupSettings = this.settingsFor( T.user )
    val isNew = size == "l" && !groupSettings.hasVisited // Only care about this if it is a large thumb
    
    val inner = 
      <div class={ thumbClass( size ) } style={ style }>
       { url.notBlank ? <img src={ thumbUrl( size ) }/> | <div class="text">{ s( 'name ) }</div> }
       { extraHtml != null |* extraHtml } 
      </div>

    if ( isNew || isPrivate ) {
      val overlays = ( isPrivate && isNew ) ? ( newOverlay ++ privateOverlay ) | ( isNew ? newOverlay | privateOverlay )
      <div class={ ( ( isNew && isDist ) |* "new-box" ) + ( isPrivate |* ( " private-box sz-" + size ) ) }>{ inner }{ overlays }</div>
    } else {
      inner 
    }
  }
  
  def settingsFor( user:User )      = GroupSettings( GroupSettings.db.findOrMake( Mobj( "u" -> user.id, "g" -> this.id ) ) )
  def settingsFor( userTid:String ) = GroupSettings( GroupSettings.db.findOrMake( Mobj( "u" -> B.User.tidToId( userTid ), "g" -> this.id ) ) )

  def markVisited = {
    val settings = settingsFor( T.user )
          
    if ( !T.session.isIncognito && !settings.hasVisited ) {
      settings.setVisited
      
      if ( contentType == ContentType.Team )
        settings( 'cap ) = 40.0
        
      settings.save
    }
  }

  override def contents = B.groupContents( this )

  def hasOrg = oid( 'org ) != null
  def org    = B.Org.getById( oid( 'org ) )

  def website =
    if ( hasOrg )
      org.s( 'website )
    else
      null

  def userIdleDays =
    if ( hasOrg )
      org.i( 'userIdleDays )
    else
      0

  override def copy( ownerTid:String ): Content = {
    val group = super.copy( ownerTid ).as[Group]
    group( 'org ) = T.user.org.id 
    group
  }  
}

object GroupCapacity extends MongoEntity( tid = "a1Av" ) {
  type RecType = GroupCapacity
  override def convert( obj:DBObject, parent:MongoRecord ) = new GroupCapacity( obj, parent )

  override def init {
    super.init

    "_id"            is DbMongoId         is 'id is 'client;
  
    "u"              is DbLink(B.User)    as "User" is 'client;
    "g"              is DbLink(Group)     as "Group" is 'client;
  
    "start"          is DbDate            as "Start" is 'client;
    
    "cap"            is DbDouble          as "Capacity" is 'client;                  
  }

  def forGroupUsers( tid:String, userTids:Seq[String], startDate:Date, endDate:Date ) = {
    //spam( "ending: " + endDate.getTime )
    GroupCapacity.db.find( Mobj( 
        "g" -> Group.tidToId( tid ), 
        "u" -> Mobj( $in -> userTids.map( B.User.tidToId ).toMlist ),
        "start" -> Mobj( $gte -> startDate, $lt -> endDate )
    ) ).toSeq.map( gc => 
        Map(
          "u" -> B.User.idToTid( gc.oid( 'u ) ),
          "cap" -> gc.d( 'cap ),
          "start" -> gc.t( 'start ).getTime()
          ) 
       )
  }
    
  val index = {
    db.ensureIndex( Mobj( "g" -> 1, "u" -> 1, "start" -> 1 ) )
    db.ensureIndex( Mobj( "u" -> 1, "g" -> 1, "start" -> 1  ) )
  }    
}

class GroupCapacity( obj:DBObject, parent:MongoRecord ) extends MongoRecord( GroupCapacity.makeView, obj, parent ) {
}

  
/*
 * * *  Group Settings
 */

object GroupSettings extends MongoEntity( tid = "a0Rt" ) {
  type RecType = GroupSettings
  override def convert( obj:DBObject, parent:MongoRecord ) = new GroupSettings( obj, parent )

  override def init {
    super.init

    "_id"            is DbMongoId                              is 'id is 'client;
  
    "u"              is DbLink(B.User)                         as "User";
    "g"              is DbLink(Group)                          as "Group" is 'client;
  
    "order"          is DbArray(DbTid( B.ContentEntities:_* )) as "Ordering";
    
    "flags"          is DbLong                                 as "Flags" is 'client;
    
    "cap"            is DbDouble                               as "Capacity"                  
  }

  db.ensureIndex( Mobj( "g" -> 1, "u" -> 1 ) )
  
  val FLAG_VISITED          = 1
  val FLAG_HIDDEN_COMMENTS  = 2
  
  def forGroupUserId( tid:String, userId:ObjectId ) = 
    GroupSettings( GroupSettings.db.findOrMake( Mobj( "u" -> userId, "g" -> Group.tidToId( tid ) ) ) )
  
  def capsForGroupUsers( tid:String, userTids:Seq[String], defCap:Double ) = {
    val settings = GroupSettings.db.find( Mobj( 
        "g" -> Group.tidToId( tid ), 
        "u" -> Mobj( $in -> userTids.map( B.User.tidToId ).toMlist ) 
    ) ).toSeq.map( gc => 
        Map(
          "u" -> B.User.idToTid( gc.oid( 'u ) ),
          "cap" -> gc.d( 'cap )
          ) 
       )
   
    val allSettings = mutable.Set[Map[String,Any]]()
    
    userTids.foreach( u => {
      val found = settings.find( o => o.s( 'u ) == u )
     
      if ( found == None ) {
        allSettings += Map( "u" -> u, "cap" -> defCap )
      } else {
        allSettings += found.get
      }
    } )
   
    allSettings.toSeq
  }

  def capMapForGroupUsers( tid:String, userTids:Seq[String], defCap:Double ) = {
    val capMap = mutable.Map[String,Double]()
    
    GroupSettings.db.find( Mobj( 
        "g" -> Group.tidToId( tid ), 
        "u" -> Mobj( $in -> userTids.map( B.User.tidToId ).toMlist ) 
    ) ).toSeq.foreach( gc => capMap( B.User.idToTid( gc.oid( 'u ) ) ) = gc.d( 'cap ) )
   
    userTids.foreach( u => {
      if ( !capMap.containsKey( u ) )
        capMap( u ) = defCap
    } )
   
    capMap
  }
  
  def forGroupTid( tid:String, user:User ) = forGroupUserId( tid, user.id._oid )
    
  val index = {
    db.ensureIndex( Mobj( "u" -> 1, "g" -> 1 ) )
  }    
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
  
  def hasHiddenComments =
    ( l( 'flags ) & GroupSettings.FLAG_HIDDEN_COMMENTS ) == GroupSettings.FLAG_HIDDEN_COMMENTS
  
  def toggleHiddenComments = {
    var flags = this.l( 'flags )
    this( 'flags ) = hasHiddenComments ? ( flags ^ GroupSettings.FLAG_HIDDEN_COMMENTS ) | ( flags | GroupSettings.FLAG_HIDDEN_COMMENTS )
  }  
}

object Grouplet extends Weblet {
  def handle( web: WebContext ) {
    rpath match {
    case _ =>
        _404
    }
  }
}
