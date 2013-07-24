/**
 * Copyright (c) 2008-2013 Tyranid <http://tyranid.org>
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
import org.tyranid.content.{ ContentMeta, Content, ContentEdit, ContentType }
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
import org.tyranid.sso.SsoMapping
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

  "parent"       is DbLink(Group)                      as "Parent" is 'client;

  "members"      is DbArray(DbTid(B.Org,B.User,Group)) as "Members" is 'client;
  "private"      is DbBoolean                          is 'client;
  "ssoSynced"    is DbBoolean                          is 'client;

  //"settings"     is GroupSettings                      is 'temporary is 'client computed { _.as[Group].settingsFor( T.user ) }
  
  "canSso"       is DbBoolean                          is 'temporary is 'client computed { _.as[Group].canBeSsoSynced( T.user ) }

  "website"      is DbChar(80)                         is 'temporary is 'client computed { _.as[Group].website }
  "userIdleDays" is DbInt                              is 'temporary is 'client computed { _.as[Group].userIdleDays }

  "onlineCount"  is DbInt                              is 'temporary is 'auth computed { _.as[Group].onlineMembers.size }
  
  "category"     is DbLink(GroupCategory);//              is 'client;
  "categoryId"   is DbInt                              is 'client computed { _.as[Group].i( 'category ) }
  //"search"         { search criteria } // future ... list search for a group, rather than each id explicitly
  
  "tmpl"         is DbBoolean                          is 'client as "Template Project"
  "canComment"   is DbBoolean                          is 'temporary computed { _.as[Group].canComment }

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
  
  def canSeeOther( orgTid:String, user:User ) = ownedBy( orgTid ).filter( g => Group( g ).canSee( user ) )
  
  def visibleTo( user:User, contentType:ContentType = ContentType.Group, allowBuiltins:Boolean = true, publicGroup:Boolean = false ) = {

    def in( tids:Seq[String] ) =
      if ( tids.size == 1 )
        tids( 0 )
      else
        Mobj( $in -> tids.toArray )

    def query( obj:DBObject ) = {
      if ( contentType != null )
        obj( "type" ) = contentType.id

      if ( !allowBuiltins )
        obj( "builtin" ) = Mobj( $ne -> true )

      obj
    }

    var tids =
      if ( user.org != null ) Seq( user.tid, user.org.tid )
      else                    Seq( user.tid )

    if ( contentType != ContentType.Group )
      tids ++= user.groups.map( _.tid )
    
    if ( publicGroup )
      tids ++= Seq( B.publicGroup.tid )
      
    val myGroups =
      db.find(
        query( Mobj( "o" -> in( tids ) ) )
      ).map( apply ).toSeq

    val memberGroups =
      db.find( 
        query( Mobj( "v" -> in( tids ) ) )
      ).map( apply ).toSeq.filter( memberGroup => !myGroups.exists( _.id == memberGroup.id ) )

    var newGroups = myGroups ++ memberGroups


    /*
     * Currently, groups of type Group are the only type of group allowed to contain other groups.
     *
     * This means the query needs to be a recursive query since each new query can find new groups
     * that we, in turn, need to check.
     *
     *
     */
    if ( contentType == ContentType.Group ) {
      var visitedGroups = Buffer[Group]()

      while ( newGroups.nonEmpty ) {
        visitedGroups ++= newGroups

        newGroups =
          db.find( 
            query( Mobj( "v" -> in( newGroups.map( _.tid ) ) ) )
          ).map( apply ).filter( grp => !visitedGroups.exists( _.id == grp.id ) ).toSeq
      }

      visitedGroups
    } else {
      newGroups
    }
  }

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
    val grp = Group( Group.db.findOrMake( Mobj( "org" -> user.org.id, "name" -> user.org.name, "type" -> ContentType.Organization.id ) ) )
    
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
}

class Group( obj:DBObject, parent:MongoRecord ) extends Content( Group.makeView, obj, parent ) {

  def name     = s( 'name )
  def fullName( user:User ) = name + " (" + ( isOwner( user ) ? "me" | ownerNames ) + ")"

  override def id:ObjectId = super.apply( "_id" ).as[ObjectId]
 

  /*
   * * *   Nested Groups
   */

  def hasParent = get( 'parent ) != null

  def parentGroup = Group.getById( get( 'parent ) )

  def isContainedIn( groupTid:String ):Boolean =
    this.tid == groupTid || {
      val p = parentGroup
      p != null && p.isContainedIn( groupTid )
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
  
  def canSee( member:Record ):Boolean = canSee( T.user, member )

  def isSsoSynced = b( 'ssoSynced )
  
  def onlineMembers = { 
    val members = new mutable.ArrayBuffer[Record]
    collectOnlineMembers( members )
    members
  }
  
  override def canComment = !isLocked && !b( 'tmpl )
  
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

  def canSee( viewer:User, member:Record ) = isOwner( viewer ) || canView( viewer )
  
  def canBeSsoSynced( user:User ):Boolean = {
    val userOrgId = user.orgId 
    val ssoExists = userOrgId != null && SsoMapping.db.exists( Mobj( "org" -> userOrgId ) )
    
    if ( !ssoExists )
      return false
      
    if ( isNew )
      return true
    
    var orgId:Any = null
      
    for ( user <- owners if B.User.hasTid( user.tid ) ) {
      if ( orgId == null ) {
        orgId = user.oid( 'org )
      } else if ( orgId != user.oid( 'org ) ) {
        return false
      }
    }
    
    return true
  }
  
  override def imageUrl( editing:ContentEdit = null ) =
    if ( contentType == ContentType.Organization ) {
      val org = B.Org.getById( oid( 'org ) )
      org.s( 'thumbnail )
    } else {
      super.imageUrl( editing )
    }

  
  override def imageForThumbs:File = {
    val file = super.imageForThumbs
    ( file != null ) ? file | B.getS3Bucket( "public" ).file( "images/default_project_image.png" )
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

  val newOverlay = <div class="new-overlay"><div class="text">NEW</div></div>
  val privateOverlay = <div class="private-overlay"><span class="icon-minus"></span><div class="text">PRIVATE</div></div>

  override def thumbHtml( size:String, extraHtml:NodeSeq = null ) = {
    val url = imageUrl( null )

    val style:String = url.isBlank ? {
      var color = s( 'color )
      
      if ( color.isBlank ) {
        color = getRandomColor
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
       { ( extraHtml != null ) |* extraHtml } 
      </div>

    if ( isNew || isPrivate ) {
      val overlays = ( isPrivate && isNew ) ? ( newOverlay ++ privateOverlay ) | ( isNew ? newOverlay | privateOverlay )
      <div class={ ( isNew |* "new-box" ) + ( isPrivate |* ( " private-box sz-" + size ) ) }>{ inner }{ overlays }</div>
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
  }

  db.ensureIndex( Mobj( "g" -> 1, "u" -> 1 ) )
  
  val FLAG_VISITED          = 1
  val FLAG_HIDDEN_COMMENTS  = 2
  
  def forGroupTid( tid:String, user:User ) = 
    GroupSettings( GroupSettings.db.findOrMake( Mobj( "u" -> user.id , "g" -> Group.tidToId( tid ) ) ) )
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
