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

package org.tyranid.content

import java.io.File
import java.util.Date

import scala.collection.mutable
import scala.collection.mutable.Buffer
import scala.xml.{ Text, NodeSeq }

import org.bson.types.ObjectId
import com.mongodb.{ BasicDBList, DBCollection, DBObject }

import org.tyranid.Imp._
import org.tyranid.cloud.aws.{ S3Bucket, S3 }
import org.tyranid.db.{ DbArray, DbBoolean, DbChar, DbDate, DbDateTime, DbDouble, DbInt, DbLink, DbLong, DbTid, DbText, DbUrl, Entity, Record, ViewAttribute }
import org.tyranid.db.es.{ SearchAuth, SearchText, SearchTextLike, SearchToken }
import org.tyranid.db.meta.TidItem
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity, MongoRecord, MongoView }
import org.tyranid.db.ram.RamEntity
import org.tyranid.db.tuple.{ Tuple, TupleView }
import org.tyranid.image.{ Dimensions, Thumbnail }
import org.tyranid.http.Http
import org.tyranid.io.{ HasText, MimeType }
import org.tyranid.profile.{ Group, Tag, User }
import org.tyranid.secure.{ PrivateKeyEntity, PrivateKeyRecord }

object ViewType extends RamEntity( tid = "a13v" ) {
  type RecType = ViewType
  override def convert( view:TupleView ) = new ViewType( view )

  "_id"    is DbInt      is 'id;
  "name"   is DbChar(64) is 'label;


  override val addNames = Seq( "_id", "name" )

  val Post   = add( 1, "Post"  )
  val Card   = add( 2, "Card"  )
  val Table  = add( 3, "Table" )
  val Grid   = add( 4, "Grid"  )
  val Kanban = add( 5, "Kanban"  )
}

case class ViewType( override val view:TupleView ) extends Tuple( view )


object Orientation extends RamEntity( tid = "a14v" ) {
  type RecType = Orientation
  override def convert( view:TupleView ) = new Orientation( view )

  "_id"    is DbInt      is 'id;
  "name"   is DbChar(64) is 'label;


  override val addNames = Seq( "_id", "name" )

  val Horizontal = add( 1, "Horizontal" )
  val Vertical   = add( 2, "Vertical"   )
}

case class Orientation( override val view:TupleView ) extends Tuple( view ) {

  def cycle =
    this match {
    case Orientation.Vertical   => Orientation.Horizontal
    case Orientation.Horizontal => Orientation.Vertical
    }
}


object ContentType extends RamEntity( tid = "a10v" ) {
  type RecType = ContentType
  override def convert( view:TupleView ) = new ContentType( view )

  "_id"    is DbInt      is 'id;
  "name"   is DbChar(64) is 'label;


  override val addNames = Seq( "_id", "name" )

  val ChangeLog          = add( 1, "ChangeLog" )
  val Message            = add( 2, "Message" )
  val Content            = add( 3, "Content" )
  val Folder             = add( 4, "Folder" )
  val Document           = add( 5, "Document" )
  val Group              = add( 6, "Group" )
  val Project            = add( 7, "Project" )
  val Organization       = add( 8, "Organization" )
}

case class ContentType( override val view:TupleView ) extends Tuple( view )

object ContentOrder extends RamEntity( tid = "a0St" ) {
  type RecType = ContentOrder
  override def convert( view:TupleView ) = new ContentOrder( view )

  "_id"    is DbInt      is 'id;
  "name"   is DbChar(64) is 'label;


  override val addNames = Seq( "_id", "name" )

  val Manual          = add( 1, "Manual" )
  val LastModified    = add( 2, "Last Modified" )
  val Label           = add( 3, "Label" )
}

case class ContentOrder( override val view:TupleView ) extends Tuple( view ) {

  def sort( content:Seq[Content] ) =
    this match {
    case ContentOrder.Manual       => content.sortBy( _.i( 'pos ) )
    case ContentOrder.LastModified => content.sortBy( -_.lastModifiedMs )
    case ContentOrder.Label        => content.sortBy( _.label )
    }
}



object ContentMoveMode extends RamEntity( tid = "a0Tt" ) {
  type RecType = ContentMoveMode
  override def convert( view:TupleView ) = new ContentMoveMode( view )

  "_id"    is DbInt      is 'id;
  "name"   is DbChar(64) is 'label;


  override val addNames = Seq( "_id", "name" )

  val Before  = add( 1, "Before" )
  val Into    = add( 2, "Into" )
  val End     = add( 3, "End" )
}

case class ContentMoveMode( override val view:TupleView ) extends Tuple( view )


/**
 * This code manages the 'pos property in Content.
 */
object Positioning {

  def apply( container:Content, newOrderTids:Seq[String] ):Positioning = {

    val contents = ContentOrder.Manual.sort( container.contents )


    val newOrder:Seq[Content] = newOrderTids.map( tid => contents.find( _.tid == tid ).get )

    var moving:Content = null
    var before:Content = null

    /**
     * Reverse-Engineer what was moved where to where by comparing the newOrder to the oldOrder
     * 
     * Example #1, given:
     * 
     *  full original order: A B C D E
     *    new partial order: A D C E
     * 
     * this algorithm yields:
     * 
     * moving: D
     * before: C
     *
     *
     * Example #2, given:
     *
     *  full original order: A B C D E
     *    new partial order: C D E A
     * 
     * this algorithm yields:
     * 
     * moving: A
     * before: null
     *
     *
     * Algorithm:  Find the first out of order in the partial order
     *
     *   for each element X in the new order ...
     *     find X's position in the old order
     *           are any elements BEFORE X in the new order AFTER X in the old order
     *     -OR - are any elements AFTER X in the new order BEFORE X in the old order ?
     *       if so,
     *         moving = X
     *         to = element after X in newOrder or else null if X is already at the end
     */
    def findMoving {
      val olen = contents.size
      val nlen = newOrder.size

      for ( ni <- 0 until nlen ) {
        val nic = newOrder( ni ) // nc = new content

        val oi = contents.indexWhere( _.id == nic.id )

        if (   // are any elements BEFORE ni in the new order while AFTER oi in the old order ?
               newOrder.slice( 0, ni - 1 ).exists( njc =>
                 contents.slice( oi+1, olen ).exists( _.id == njc.id )
               )
            || // are any elements AFTER ni in the new order while BEFORE oi in the old order ?
               newOrder.slice( ni + 1, nlen ).exists( njc =>
                 contents.slice( 0, oi ).exists( _.id == njc.id )
               )
            ) {
          moving = nic
          before = if ( ni + 1 < nlen ) newOrder( ni + 1 )
                   else                 null
          return
        }
      }
    }

    findMoving

    if ( moving == null ) {
      null
    } else {
      val repos = Positioning( moving = moving, before = before, beforeEnd = ( before == null ), toContainer = container, toContents = contents, fromContainer = container, fromContents = null )
      repos.reposition
      repos
    }
  }

  def apply( moving:Content, to:Content, mode:ContentMoveMode ):Positioning = {

    // PERFORMANCE-TODO:  instead of using contents, add a query method that just returns a Seq[DBObject] just containing '_id and 'pos ...

    if ( to != null && to.id == moving.id )
      return null

    var fromContainer:Content = null
    var fromContents:Seq[Content] = null

    var toContainer:Content = null
    var toContents:Seq[Content] = null

    var before:Content = null
    var beforeEnd = false

    mode match {
    case ContentMoveMode.Before =>
      fromContainer = moving.container

      toContainer = to.container
      toContents = ContentOrder.Manual.sort( toContainer.contents )

      before = to

    case ContentMoveMode.Into =>
      fromContainer = moving.container

      toContainer = to
      toContents = ContentOrder.Manual.sort( toContainer.contents )

      before =
        if ( toContents.nonEmpty ) toContents.head
        else                       null

    case ContentMoveMode.End =>
      fromContainer = moving.container

      toContainer = if ( to != null ) to else fromContainer
      toContents = ContentOrder.Manual.sort( toContainer.contents )

      beforeEnd = true
    }

    val repos = Positioning( moving = moving, before = before, beforeEnd = beforeEnd, toContainer = toContainer, toContents = toContents, fromContainer = fromContainer, fromContents = fromContents )
    repos.reposition
    repos
  }

  def updatePos( c:Content, pos:Int ) {
    val ePos = c.i( 'pos )

    if ( !c.obj.has( 'pos ) || ePos != pos ) {
      c( 'pos ) = pos
      c.db.update( Mobj( "_id" -> c.id ), Mobj( $set -> Mobj( "pos" -> pos ) ) )
    }
  }

  def nextFirstPositionFor( content:Content ):Int =
    content match {
    case group:Group => throw new UnsupportedOperationException() // we don't have access to FileSystem.db, projects are in Group, their contents are in FileSystem // nextFirstPositionForGroup ( null,          content.oid )
    case _           => nextFirstPositionForFolder( content.db,    content.oid )
    }

  def nextLastPositionFor( content:Content ):Int =
    content match {
    case group:Group => throw new UnsupportedOperationException() // we don't have access to FileSystem.db, projects are in Group, their contents are in FileSystem // nextFirstPositionForGroup ( null,          content.oid )
    case _           => nextLastPositionForFolder( content.db,    content.oid )
    }

  def nextFirstPositionForGroup( db:DBCollection, groupId:ObjectId ):Int = {
    for ( rec <- db.find( Mobj( $and -> Mlist( Mobj( "parentGroup" -> groupId, "parentFolder" -> null ), Mobj( "pos" -> Mobj( $exists -> true ) ) ) ), Mobj( "pos" -> 1 ) ).sort( Mobj( "pos" -> 1 ) ).limit( 1 ) )
      return rec.i( 'pos ) - 1

    0
  }

  def nextLastPositionForGroup( db:DBCollection, groupId:ObjectId ):Int = {
    for ( rec <- db.find( Mobj( $and -> Mlist( Mobj( "parentGroup" -> groupId, "parentFolder" -> null ), Mobj( "pos" -> Mobj( $exists -> true ) ) ) ), Mobj( "pos" -> 1 ) ).sort( Mobj( "pos" -> -1 ) ).limit( 1 ) )
      return rec.i( 'pos ) + 1

    0
  }

  def nextFirstPositionForFolder( db:DBCollection, folderId:ObjectId ):Int = {
    for ( rec <- db.find( Mobj( $and -> Mlist( Mobj( "parentFolder" -> folderId ), Mobj( "pos" -> Mobj( $exists -> true ) ) ) ), Mobj( "pos" -> 1 ) ).sort( Mobj( "pos" -> 1 ) ).limit( 1 ) )
      return rec.i( 'pos ) - 1

    0
  }

  def nextLastPositionForFolder( db:DBCollection, folderId:ObjectId ):Int = {
    for ( rec <- db.find( Mobj( $and -> Mlist( Mobj( "parentFolder" -> folderId ), Mobj( "pos" -> Mobj( $exists -> true ) ) ) ), Mobj( "pos" -> 1 ) ).sort( Mobj( "pos" -> -1 ) ).limit( 1 ) )
      return rec.i( 'pos ) + 1

    0
  }
}

case class Positioning( var moving:Content,
                        var before:Content,
                        var beforeEnd:Boolean,
                        var toContainer:Content,
                        var toContents:Seq[Content],
                        var fromContainer:Content,
                        var fromContents:Seq[Content] ) {

  def reposition {
//sp am( "BEFORE: " + toContents.map( _.label ).mkString( ", " ) )

    if ( toContainer.id != fromContainer.id ) {

      fromContents = ContentOrder.Manual.sort( fromContainer.contents ).filter( _.id != moving.id )

      for ( i <- 0 until fromContents.size )
        Positioning.updatePos( fromContents( i ), i )
    }

    toContents = toContents.filter( _.id != moving.id )

    if ( before != null ) {
      val idx = toContents.indexWhere( _.id == before.id )

      toContents = ( toContents.slice( 0, idx ) :+ moving ) ++ toContents.slice( idx, toContents.size )
    } else if ( beforeEnd ) {
      toContents = toContents :+ moving
    } else {
      toContents = moving +: toContents
    }

//sp am( " AFTER: " + toContents.map( _.label ).mkString( ", " ) )
    for ( i <- 0 until toContents.size )
      Positioning.updatePos( toContents( i ), i )
  }
}



/*
 * * *  Comments
 */

object Comment extends MongoEntity( tid = "b00w", embedded = true ) {
  type RecType = Comment
  override def convert( obj:DBObject, parent:MongoRecord ) = new Comment( obj, parent )

  "_id"            is DbInt                  is 'id;

  "id"             is DbInt                  is 'client computed { _.i( '_id ) };

  "on"             is DbDateTime             is 'client;
  "m"              is DbChar(1024)           as "Message"     is 'client is 'label;

  "pn"             is DbInt                  as "Page Number" is 'client;
  "x"              is DbDouble               as "X"           is 'client;
  "y"              is DbDouble               as "Y"           is 'client;
  "w"              is DbDouble               as "When"        is 'client; // Used for timeline (video annotation)
  "wi"             is DbDouble               as "Width"       is 'client; // Used for boxed annotations
  "hi"             is DbDouble               as "Height"      is 'client; // Used for boxed annotations

  "r"              is DbArray(Comment)       as "Replies"     is 'client;

  "pri"            is DbBoolean              as "Priority"    is 'client; // a.k.a. "important" or "urgent"

  override def init = {
    super.init
    "u"            is DbLink(B.User)         as "From"        is 'client; // user who created the reply
  }

  def maxId( comments:BasicDBList ):Int = {
    val candidates = comments.toSeq.map( obj => Comment.apply( obj.as[DBObject] ).maxId )

    if ( candidates.size > 0 )
      candidates.max
    else
      0
  }

  def idify( comments:BasicDBList ):Boolean = {

    var nextId = maxId( comments ) + 1
    var didAnything = false

    def enterComment( comment:Comment ) {
      if ( comment.i( '_id ) == 0 ) {
        comment( '_id ) = nextId
        didAnything = true
        nextId += 1
      }

      enterList( comment.a_?( 'r ) )
    }

    def enterList( comments:BasicDBList ) {
      for ( c <- asComments( comments ) )
        enterComment( c )
    }

    enterList( comments )
    didAnything
  }

  def asComments( comments:BasicDBList, pageNumber:Int = 0 ) = {
    var seq = comments.toSeq.map( obj => Comment( obj.as[DBObject] ) )

    if ( pageNumber != 0 )
      seq = seq.filter { comment =>
        val pn = comment.i( 'pn )

        pageNumber match {
        case 1 => pn == 0 || pn == 1 // OMEGA-7
        case n => pn == n
        }
      }

    seq
  }

  def find( comments:BasicDBList, id:Int ):Comment = {

    for ( c <- asComments( comments) ) {
      val found = c.find( id )
      if ( found != null )
        return found
    }

    null
  }

  def mostRecent( comments:Seq[Comment] ):Comment = {

    var mostRecent:Comment = null

    for ( c <- comments ) {
      val mrc = c.mostRecent

      if ( mostRecent == null || mrc.id._i > mostRecent.id._i )
        mostRecent = mrc
    }

    mostRecent
  }

  def remove( comments:BasicDBList, id:Int ) {
    for ( c <- comments.toSeq.of[DBObject] )
      if ( c.i( '_id ) == id )
        comments.remove( c )
      else
        remove( c.a_?( 'r ), id )
  }

  def sort( comments:Seq[Comment], newestFirst:Boolean = false, byPageXy:Boolean = false, byWhen:Boolean = false ) = {

    if ( newestFirst )
      comments.sortBy( _.mostRecentOn ).reverse
    else if ( byWhen )
      comments.sortWith { ( a:Comment, b:Comment ) =>
        val aw = a.d( 'w )
        val bw = b.d( 'w )
        
        if ( !a.hasAnnotation && !b.hasAnnotation )
          a.mostRecentOn < b.mostRecentOn 
        else if ( !a.hasAnnotation )
          false
        else if ( aw != bw ) {
          aw < bw
        } else {
          val ay = a.y
          val by = b.y

          if ( ay != by )
            ay < by
          else
            a.x < b.x
        }
      }
    else if ( byPageXy )
      comments.sortWith { ( a:Comment, b:Comment ) =>
        val apn = a.pn
        val bpn = b.pn

        if ( apn != bpn ) {
          apn < bpn
        } else {
          val ay = a.y
          val by = b.y

          if ( ay != by )
            ay < by
          else
            a.x < b.x
        }
      }
    else
      comments.sortBy( _.on )
  }

  def count( comments:Seq[Comment] ):Int = comments.map( _.count ).sum
}

class Comment( obj:DBObject, parent:MongoRecord ) extends MongoRecord( Comment.makeView, obj, parent ) {

  def pn = i( 'pn )
  def x  = d( 'x )
  def y  = d( 'y )
  def w  = d( 'w )
  def wi = d( 'wi )
  def hi = d( 'hi )

  def isPriority = b( 'pri )

  def fromUser = {
    val uid = oid( 'u )
    val user = B.User.getById( uid )

    if ( user == null )
      B.systemUser
    else
      user
  }

  def maxId:Int = i( '_id ) max Comment.maxId( a_?( 'r ) )

  def find( id:Int ):Comment =
    if ( i( '_id ) == id )
      this
    else
      Comment.find( a_?( 'r ), id )

  def comments = {
    val ea = Mongo.EmptyArray 
    Comment.asComments( a_?( 'r ) )
    }

  def hasAnnotation = ( has( 'w ) && d( 'w ) > -1 ) || has( 'x ) || has ( 'y )

  //def annotationType:String = has( 'pn ) ? "page" | ( has( 'w ) ? "timeline" | ( ( has( 'x ) ? "xy" ) | null ) )

  def on = t( 'on )
  def displayDate = t( 'on )

  def mostRecentOn:Date = {
    val c = comments

    if ( c.nonEmpty )
      on max c.map( _.mostRecentOn ).max
    else
      on
  }

  def mostRecent:Comment = {
    var mostRecentChild = Comment.mostRecent( comments )

    if ( mostRecentChild != null && mostRecentChild.id._i > id._i )
      mostRecentChild
    else
      this
  }

  def count = 1 + Comment.count( comments )
}


/*
 * * *  Content
 */

trait ContentMeta extends PrivateKeyEntity {

  "_id"               is DbMongoId            is 'id is 'client;

  override def init = {
    super.init

  "builtin"           is DbBoolean            help Text( "Builtin content is maintained by the system and is not editable by end users." );

  "on"                is DbDateTime           is 'required is 'client;
  "type"              is DbLink(ContentType)  is 'required is 'client;

  "tags"              is DbArray(DbLink(Tag)) is ( new SearchTextLike() { override def extract( rec:Record, va:ViewAttribute ) = rec.as[Content].netTags } ) is 'client;
  "ctags"             is DbArray(DbLink(Tag)) help Text( "Child Tags -- these tags are inherited by child content." ) is 'client;

  "name"              is DbChar(50)           is 'label is 'required is SearchText is 'client;

  "pos"               is DbInt                ; // the position of this content within its parent (group, folder, board, etc.) ... see the class "Positioning"

  "o"                 is DbArray(DbTid(B.Org,B.User,Group)) as "Owners" is 'owner is 'client;
  "ownerTid"          is DbTid(B.User)        is 'temporary is 'client computed( _.as[Content].firstOwnerTid() )
  //"isOwner"           is DbBoolean            is 'temporary is 'client computed( _.as[Content].isOwner( T.user ) );
  //"ownerOrgTid"       is DbTid(B.Org)         is 'temporary is 'client computed( rec => B.Org.idToTid( B.User.byTid( rec.as[Content].firstOwnerTid() ).flatten( _.oid( 'org ), null ) )
  
  "v"                 is DbArray(DbTid(B.Org,B.User,Group)) as "Viewers" is SearchAuth is 'client is 'auth;
  "subV"              is DbArray(DbTid(B.Org,B.User))       ; // for showing content inside a group, subviewers

  "subscr"            is DbArray(DbTid(B.User))             as "Subscribers";

  "shown"             is DbArray(DbTid(B.User))             as "Shown To" ; // list of tids of users who have "read" this content; only maintained for some content types, like messages
  "hide"              is DbArray(DbTid(B.User))             as "Hidden From" ; // list of tids of users who have "read" this content; only maintained for some content types, like messages
 
  "complianceCode"    is DbChar(15)           is SearchToken;
  "expDate"           is DbDate               as "Expiration Date" is SearchText; 
  "approvalDate"      is DbDate               as "Approval Date" is SearchText; 
  
  "lastModified"      is DbDateTime           is 'required is SearchText is 'client;
  "lastModifiedBy"    is DbLink(B.User)       is 'required is 'client;
  "lastModifiedByOrg" is DbLink(B.Org)        is 'required;

  "lastAction"        is DbDateTime           is 'required;
  "lastActionBy"      is DbLink(B.User)       is 'required;
  "lastActionByOrg"   is DbLink(B.Org)        is 'required;

  // Messages
  "fit"               is DbBoolean            as "From in To" help <div>Indicates that the posting user is in the to list.  This will normally be false unless a user explicitly addresses a message to themselves.</div>
  "s3"                is DbBoolean            help Text( "Indicates that the file or img is stored by us in S3." );

  "logTid"            is DbChar(64)           ; // TODO:  this is really a DbTid that contains an entity instead of a record
  "logId"             is DbMongoId            ; // TODO:  this is a link to the "*_log" table for the above entity, model it better

  "file"              is DbUrl                ; // TODO: change to DbFile ... if s3 this is a S3 path, otherwise it is an absolute URL
  "fileMimeType"      is DbChar(64)           is SearchToken;

  "video"             is DbBoolean            ;


  // Attachment / File
  "link"              is DbUrl                ;
  "icon"              is DbUrl                ; // ( TODO:  should be DbImage? ) for links, this is the favicon
  "sourceUrl"         is DbUrl                ;
  "fileName"          is DbChar(128)          is 'required is 'client;
  "size"              is DbLong               ;
  "title"             is DbChar(128)          is SearchText;

  "desc"              is DbText               as "description" is 'client is SearchText;

  // Messages
  "m"                 is DbChar(1024)         as "message" is 'label is 'client is SearchText;
  "r"                 is DbArray(Comment)     as "Replies";

  "rc"                is DbInt                as "Reply Count" is 'temporary computed( _.as[Content].commentCount );

  // Image / Thumbnail
  "img"               is DbUrl                is 'client; // TODO:  change to DbImage?
  "imgH"              is DbInt                help Text( "The actual height of the image." );
  "imgW"              is DbInt                help Text( "The actual width of the image." );

  "netImg"            is DbUrl                as "Image" is 'temporary is 'client computed { _.as[Content].imageUrl( null ) }
  
  
  // RSS & Atom Feeds
  "feedOut"           is DbBoolean            help <span>If this is enabled, outgoing <b>public</b> RSS and Atom feeds are generated.</span>;
  "feed"              is DbMongoId // should be DbLink(Feed)                               ;
  "feedItemId"        is DbChar(128)          ;
  
  
  "color"             is DbChar(6)            is 'client;
  
  "locked"            is DbBoolean            is 'client;
  
  "archived"          is DbBoolean            is 'client;
  "archivedOn"        is DbDateTime           ;
  
  "dist"              is DbBoolean            is 'client; // If true, then if a group, it is a distribution group 

  "contents"          is DbArray(DbTid())     is 'temporary is 'client; // contains a sequence of the tids for client for this content
  }

  override def searchText = true

  def deleteThumbs( tid:String ) {
    val pathParts = tid.splitAt( 4 )
    val urlPath = pathParts._1 + "/" + pathParts._2 + "/"
    
    try {
      S3.delete( Content.thumbsBucket, urlPath + "l" )
      S3.delete( Content.thumbsBucket, urlPath + "m" )
      S3.delete( Content.thumbsBucket, urlPath + "s" )
      S3.delete( Content.thumbsBucket, urlPath + "t" )
    } catch {
      case nop=> ;
    }
  }
  
  override def delete( id: Any ) {
    deleteThumbs( this.idToTid( id ) )
    super.delete(id)
  }

  override def delete( rec: Record ) {
    val contentType = ContentType.getById( rec.i( 'type ) )
    
    if ( contentType != ContentType.Folder )
      deleteThumbs( rec.tid )
      
    super.delete(rec)
  }
}

object Content {
  // Thumbs are stored on S3 in the "thumbs" bucket:
  //   thumbs/[entity tid]/record tid/l
  lazy val thumbsBucket = new S3Bucket( "thumbs" )
  
  lazy val emailTag     = Tag.idFor( "email" )
  lazy val messageTag   = Tag.idFor( "message" )
  lazy val fileshareTag = Tag.idFor( "fileshare" )
  
  val defaultColors = Array( "5f89a2", "597b7c", "93278f", "e72967", "f47b20", "ef5033", "009591", "9fd5b5", "1bb7ea", "73d0f9" )
  val defaultNoteColor = "faf082"
  val noteColors = Array( "34b27d", defaultNoteColor, "e09952", "cb4d4d", "9933cc", "4d77cb" )  
}

abstract class Content( override val view:MongoView,
                        obj:DBObject = Mobj(),
                        override val parent:MongoRecord = null )
    extends MongoRecord( view, obj, parent ) with PrivateKeyRecord with HasText {

  val isBuiltin = b( 'builtin )
  
  def isLocked = b( 'locked )

  def contentType = ContentType.getById( i( 'type ) )
  def isFolder = contentType == ContentType.Folder
  def isProject = contentType == ContentType.Project

  def hasAnnotatedPages:Boolean = false
  
  def hasTag( tag:Int ) = a_?( 'tags ).exists( _ == tag )

  def copy( ownerTid:String ):Content = {
    val content = this.clone().as[Content]
    
    content( '_id ) = null
    content( 'on ) = new Date
    
    content( 'o ) = Seq( ownerTid )
    content( 'v ) = Seq( ownerTid )
    
    if ( a_?( 'subV ).length > 0 )
      content( 'v ) = Seq( ownerTid )
      
    if ( a_?( 'subscr ).length > 0 )
      content( 'v ) = Seq( ownerTid )
    
    content( 'shown ) = null
    content( 'hide ) = null

    content.stampLastModified
    content.stampLastAction
    
    //content.save
    content
  }
  
  override def text:String = ""

  override def searchText = {
    val t = text

    if ( t.notBlank ) t
    else              null
  }

  def netTags = a_?( 'tags )
  

  /*
   * * *  Label and Icons
   */
  
  override def label =
    if      ( has( 'm ) )        s( 'm )
    else if ( has( 'title ) )    s( 'title )
    else if ( has( 'name ) )     s( 'name )
    else if ( has( 'fileName ) ) s( 'fileName )
    else                         "Change Log"

  def titleInDesc( title:String, desc:String ):Boolean = {
    var t = title
    if ( t.endsWith( "..." ) )
      t = title.substring( 0, t.length - 3 )

    desc.startsWith( t ) ||
    desc.stripPrefix( "<a href=\"" ).startsWith( t ) ||
    title.startsWith( "http://" )
  }
  
  def hasImage = s( 'img ).notBlank || MimeType.isImage( fileMimeType ) //imageUrl( false ).notBlank

  def hasVideo = MimeType.isVideo( fileMimeType )

  def isPrivate = has( 'private ) && b( 'private )
  
  def imageUrl( editing:ContentEdit = null ) = s( 'img )

  def imageDimensions = {
    if ( i( 'imgW ) != 0 )
      Dimensions( height = i( 'imgH ), width = i( 'imgW ) )
    else if ( contentType == ContentType.Folder ) {
      Dimensions( 128, 128 )
    } else {
      val ext = s( 'fileName ).suffix( '.' ).denull.toLowerCase
      
      //if ( FileSystem.supportedIcons.contains( ext ) ) {
        //Dimensions( 128, 128 )
      //} else {
        Dimensions()
      //}
    }
  }    
 
  // see Thumbnail for sizes
  def imageForThumbs:File = {
    val imgUrl = imageUrl( editing = null )
    var dlUrl:String = ( imgUrl.notBlank ) ?
      ( imgUrl.toLowerCase.startsWith( "http" ) ? imgUrl | T.website + imgUrl ) | null
    
    if ( dlUrl.notBlank ) {
      dlUrl = dlUrl.prefix( '?' ) or dlUrl
      val ext = dlUrl.suffix( '.' )
      Http.GET_File( dlUrl, ext = ( "." + ext ) )
    } else {
      null
    }
  }

  def thumbClass( size:String ) =
    size match {
    case "l" => "thumbLarge"
    case "m" => "thumbMedium"
    case "s" => "thumbSmall"
    case "t" => "thumbTiny"
    }

  def thumbStyle( size:String ):String = null
  def thumbUrl( size:String ) = "/io/thumb/" + tid + "/" + size + "?cb=" + s( 'img ).denull.hashCode

  def thumbHtml( size:String, extraHtml:NodeSeq = null ) =
    <div class={ thumbClass( size ) } style={ thumbStyle( size ) }>
     <img src={ thumbUrl( size ) }/>
     { ( extraHtml != null ) |* extraHtml }
    </div>
  
  def generateThumbs:Boolean = {
    val imgFile = try {
      imageForThumbs
    } catch {
      case _ => null
    }  
    
    if ( imgFile != null ) {
      val pathParts = tid.splitAt( 4 )
      val urlPath = pathParts._1 + "/" + pathParts._2 + "/"
      
      def thumb( s:String, w:Int, h:Int ) {
        var f:File = null
        
        try {
          f = Thumbnail.generate( imgFile, w, h )
          
          if ( f != null )
            S3.write( Content.thumbsBucket, urlPath + s, f, true )
        } finally {
          if ( f != null )
            f.delete
        }
      }
      
      try {
        thumb( "l", 260, 169 )
        thumb( "m", 140, 91 )
        thumb( "s", 100, 65 )
        thumb( "t", 40, 40 )
      } finally {
        imgFile.delete
      }
      
      return true
    }
    
    false
  }


  /*
   * * *   Revisions
   */

  def displayDate = t( 'on )
  
  def stampLastModified = {
    val u = T.user
    this( 'lastModified )      = new Date
    this( 'lastModifiedBy )    = u.id
    this( 'lastModifiedByOrg ) = u.orgId
  }
  
  def stampLastAction = {
    val u = T.user
    this( 'lastModified )      = new Date
    this( 'lastModifiedBy )    = u.id
    this( 'lastModifiedByOrg ) = u.orgId
  }

  def lastModifiedByTidItem = TidItem.by( B.User.idToTid( oid( 'lastModifiedBy ) ) )

  def lastModifiedByUser = {
    val user = B.User.getById( this( 'lastModifiedBy )._oid )

    if ( user == null )
      B.systemUser
    else
      user
  }

  def lastModifiedMs:Long = {
    val lm = t( 'lastModified )
    lm != null |* lm.getTime
  }

  def wasShownTo( user:User ) = a_?( 'shown ).has( user.tid )
  def markShownTo( user:User ) = db.update( Mobj( "_id" -> id ), $addToSet( "shown", user.tid ) )

  def isHiddenFrom( user:User ) = a_?( 'hide ).has( user.tid )
  def markHiddenFrom( user:User ) = db.update( Mobj( "_id" -> id ), $addToSet( "hide", user.tid ) )


  /*
   * * *   Author
   */

  def ownerTidItem = TidItem.by( firstOwnerTid() )

  def fromUser = {
    val utid = firstOwnerTid()
    if ( utid.isBlank ) {
      B.systemUser
    } else {
      val user = B.User.getByTid( utid )

      if ( user == null )
        B.systemUser
      else
        user
    }
  }

  def fromIcon =
    if ( this.obj.has( 'feed ) ) "/images/rssLarge.png"
    else                         fromUser.icon


  /*
   * * *   Comments
   */

  def comments = Comment.asComments( a_?( 'r ) )

  def commentCount = Comment.count( comments )
  

  def canComment = !isLocked

  def commentTid( comment:Comment ) = tid + "_" + comment.id

  def commentTidToId( tid:String ) =
    if ( tid.isBlank )
      0
    else
      tid.substring( this.tid.length + 1 )._i

  def mostRecentComment = Comment.mostRecent( comments )

  def commentById( id:Int ) = Comment.find( a_?( 'r ), id )

  def comment( msg:String, user:User, replyTo:Comment = null, pageNumber:Int = 0, x:Double = 0.0, y:Double = 0.0, w:Double = -1, wi:Double = 0.0, hi:Double = 0.0, priority:Boolean = false ) = {
    val comments = a_!( 'r )

    val comment = Comment( Mobj( "_id" -> ( Comment.maxId( comments ) + 1 ), "on" -> new Date, "m" -> msg, "u" -> user.id ) )

    if ( pageNumber != 0 )
      comment( 'pn ) = pageNumber

    if ( x != 0.0 && y != 0.0 ) {
      comment( 'x ) = x
      comment( 'y ) = y
      comment( 'wi ) = wi
      comment( 'hi ) = hi
    }
    
    comment( 'w ) = w

    if ( priority )
      comment( 'pri ) = true
      
    if ( replyTo != null )
      replyTo.a_!( 'r ).add( comment )
    else
      comments.add( comment )

    save

    comment
  }

  def commentRemove( id:Int ) = {
    Comment.remove( a_!( 'r ), id )
    save
  }

  def annotatedPages = comments.map( _.i( 'pn ) ).distinct



  /*
   * * *   Security
   */

  def isJustTo( user:User ) = {
    val v = viewerTids
    v.size == 1 && v( 0 ) == user.tid
  }

  def owners = Record.getByTids( ownerTids )

  def writers = owners
  
  def viewerUsers = {
    val userTids:mutable.ArrayBuffer[String] = new mutable.ArrayBuffer[String]
    
    viewerTids foreach { vTid =>
      vTid match {
        case userTid if B.User.hasTid( userTid ) && !userTids.contains( userTid ) =>
          userTids += userTid
        case groupTid if Group.hasTid( groupTid ) =>
          val g = Group.getByTid( groupTid )

          g.viewerTids foreach { guTid =>
            if ( B.User.hasTid( guTid ) && !userTids.contains( guTid ) )
              userTids += guTid
          }
        case _ =>
          // nop
      }
    }

    val users:mutable.ArrayBuffer[Record] = new mutable.ArrayBuffer[Record]
    
    userTids.map( userTid => B.User.getByTid( userTid ) )
  }
  
  def viewers = Record.getByTids( viewerTids )

  def ownerNames = a_?( 'o ).map( tid => TidItem.by( tid.as[String] ).name ).mkString( ", " )

  def firstOwnerTid( notTids:String* ):String = {
    val owners = a_?( 'o )
    
    owners foreach { t =>
      val tid = t._s
      
      if ( !notTids.contains( tid ) )
        return tid
    }

    null
  }

  def ownerEntities = a_?( 'o ).toSeq.of[String].map( _.substring( 0, 4 ) ).distinct.map( tid => Entity.byTid( tid ).get )
  def viewerEntities = a_?( 'v ).toSeq.of[String].map( _.substring( 0, 4 ) ).distinct.map( tid => Entity.byTid( tid ).get )

  def ownerTids  = obj.a_?( 'o ).toSeq.of[String]
  def viewerTids = obj.a_?( 'v ).toSeq.of[String]

  // TODO:  make this name more generic / less Messages-ish
  def isTo( u:User ):Boolean = {
    val userTid    = u.tid
    val userOrgTid = u.orgTid

    val viewers = viewerTids

    def isSubTo = {
      val subV = obj.a_?( 'subV ).toSeq.of[String]
      subV.isEmpty || subV.exists( tid => tid == userTid || tid == userOrgTid )
    }

    for ( tid <- viewers )
      if ( tid == userTid || tid == userOrgTid )
        return isSubTo

    val from = Record.getByTid( this.a_?( 'o ).head.as[String] )
    
    if ( from == null )
      return false

    val groupTids = u.groupTids
    for ( tid <- viewers;
          if Group.hasTid( tid ) && groupTids.contains( tid );
          grp <- Group.byTid( tid );
          if grp.canSee( u, from ) )
      return isSubTo

    false
  }

  /**
   * This is like "isTo()" except that this controls whether a user can see a participant in the content (either on the "v"/to list or a "reply" from them)
   */
  def canSee( u:User, tid:String ):Boolean = {
    if (   this.a_?( 'o ).contains( u.tid )
        || u.allowProfileTids.contains( tid ) )
      return true

    val groupTids = u.groupTids
    val rec = Record.getByTid( tid )

    if ( viewerTids.exists( toTid =>
           toTid.startsWith( Group.tid ) &&
           groupTids.contains( toTid ) &&
           Group.byTid( toTid ).exists( _.canSee( u, rec ) ) ) )
      return true

    val groupPresent = viewerTids.exists( _.startsWith( Group.tid ) )

    // If no group is present, then regular network rules apply ... for now.  Is this right?
    // Note:  this u.inNetwork() is also handling the case where they are from the same org ... so if we remove u.inNetwork() will need to add in an org check
    !groupPresent && u.inNetwork( tid )
  }

  def isArchived = b( 'archived )

  def isOwner( user: org.tyranid.profile.User ): Boolean = isOwner( user.tid ) || ( user.hasOrg && isOwner( user.org.tid ) )

  def isOwner( tid: String ): Boolean = {
    if ( tid.isBlank )
      return false

    val owners = a_?( 'o )

    owners.foreach( t => {
      if ( t == tid )
        return true

      val ot = t._s

      if ( Group.hasTid( ot ) ) {
        val group = Group.getByTid( ot )

        if ( group.isOwner( tid ) )
          return true
      }
    })

    return false
  }

  def addOwner( tid:String ) = a_!( 'o ).addToSet( tid )

  def isSubViewer( user: org.tyranid.profile.User ):Boolean = isSubViewer( user.tid ) || ( user.hasOrg && isSubViewer( user.org.tid ) )
  def isSubViewer( tid: String ): Boolean = tid.isBlank ? false | ( a_?( 'subV ).exists( _._s == tid ) )
  
  def isSubscriber( user: org.tyranid.profile.User ):Boolean = isSubscriber( user.tid ) || ( user.hasOrg && isSubscriber( user.org.tid ) )
  def isSubscriber( tid: String ): Boolean = tid.isBlank ? false | ( a_?( 'subscr ).exists( _._s == tid ) )
  

  /*
   * NOTE:  canEdit() currently is the same as isOwner() but this might change in the future
   */

  
  def canEdit( user: org.tyranid.profile.User ) = isOwner( user )
  def canEdit( tid: String ):Boolean            = isOwner( tid )


  override def canView( user: org.tyranid.profile.User ):Boolean = {
    canView( user.tid ) || ( user.hasOrg && canView( user.org.tid ) )
  }

  def isMember( user:org.tyranid.profile.User ) = canEdit( user ) || canView( user )

  def canView( tid:String ):Boolean =
    tid.nonBlank &&
    a_?( 'v ).exists( t =>
      t == tid || {
        val ot = t._s
      
        Group.hasTid( ot ) &&
        Group.byTid( ot ).flatten( _.canView( tid ), false )
      }
    )


  /*
   * * *   Groups
   */

  lazy val groupTid: String = {
    val gTid:String = a_?( 'o ).map( _._s ).find( Group.hasTid ).getOrElse( null )

    if (gTid.isBlank) {
      val gOid = oid( 'parentGroup )

      if (gOid == null)
        null
      else
        Group.idToTid(gOid)
    } else
      gTid
  }

  def group = groupTid.isBlank ? null | Group.getByTid(groupTid)


  /*
   * * *   Containers & Contents  (Groups and Folders)
   */

  def container:Content = null
  def contents:Seq[Content] = Nil

  def reposition( to:Content, mode:ContentMoveMode ) = Positioning( moving = this, to = to, mode = mode )


  /*
   * * *   File Attachment
   */

  def isDocument = false
  def docId:String = isDocument ? s( 'file ).split( "/" )(1) | null 
  
  def fileType =
    contentType match {
    case ContentType.Document => s( 'fileName ).suffix( '.' ).toLowerCase
    case c                    => c.label
    }

  def hasFile = fileUrl.notBlank
  def fileUrl:String = null

  //def filePath =


  def fileMimeType =
    if ( obj.has( 'fileMimeType ) )
      s( 'fileMimeType )
    else
      org.tyranid.io.File.mimeTypeFor( s( 'fileName ) )          

  def getFileMimeType = {
    val mts = fileMimeType
    var mt = MimeType.byMimeType.getOrElse( mts, null )
    if ( mt == null )
      mt = MimeType.byExtension.getOrElse( mts, null )

    mt
  }
}

trait ContentEdit {
  def tempPath:String
}

