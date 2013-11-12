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

  val Post     = add( 1, "Post"      )
  val Card     = add( 2, "Card"      )
  val Table    = add( 3, "Table"     )
  val Grid     = add( 4, "Grid"      )
  val Kanban   = add( 5, "Kanban"    )
  val Calendar = add( 6, "Calendar"  )
  val Task     = add( 7, "Task"      )
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

  val ChangeLog    = add( 1, "ChangeLog" )
  val Message      = add( 2, "Message" )
  val Content      = add( 3, "Content" )
  val Folder       = add( 4, "Folder" )
  val Document     = add( 5, "Document" )
  val Team         = add( 6, "Team" )
  val Project      = add( 7, "Project" )
  val Organization = add( 8, "Organization" )
  val LiteProject  = add( 9, "LiteProject" )
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
//sp am( "MOVING: " + moving.label )
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
 * * * Time Track
 */

object TimeTrack extends MongoEntity( tid = "b00x", embedded = true ) {
  type RecType = TimeTrack
  override def convert( obj:DBObject, parent:MongoRecord ) = new TimeTrack( obj, parent )

  "_id"            is DbInt                  is 'id;

  "id"             is DbInt                  is 'client computed { _.i( '_id ) };

  "on"             is DbDateTime             is 'client;
  "t"              is DbDouble               as "Time Spent" is 'client is 'label;

  override def init = {
    super.init
    "u"            is DbLink(B.User)         is 'client; // user who time is applied towards
  }

  def maxId( list:BasicDBList ):Int = {
    val candidates = list.toSeq.map( obj => TimeTrack.apply( obj.as[DBObject] ).i ( '_id  ) )

    if ( candidates.size > 0 )
      candidates.max
    else
      0
  }

  def idify( list:BasicDBList ):Boolean = {

    var nextId = maxId( list ) + 1
    var didAnything = false

    def enterTime( timeTrack:TimeTrack ) {
      if ( timeTrack.i( '_id ) == 0 ) {
        timeTrack( '_id ) = nextId
        didAnything = true
        nextId += 1
      }
    }

    def enterList( list:BasicDBList ) {
      for ( tt <- asTimeTracks( list ) )
        enterTime( tt )
    }

    enterList( list )
    didAnything
  }

  def asTimeTracks( list:BasicDBList ) = list.toSeq.map( obj => TimeTrack( obj.as[DBObject] ) )

  def find( list:BasicDBList, id:Int ):TimeTrack = {

    for ( tt <- asTimeTracks( list ) ) {
      if ( tt.i( '_id ) == id )
        return tt
    }

    null
  }

  def visit( list:BasicDBList, block:TimeTrack => Unit ) {
    for ( tt <- asTimeTracks( list ) )
      tt.visit( block )
  }

  def mostRecent( list:Seq[TimeTrack] ):TimeTrack= {
    var mostRecent:TimeTrack = null

    for ( tt <- list ) {
      val mrc = tt.id._i

      if ( mostRecent == null || mrc > mostRecent.id._i )
        mostRecent = tt
    }

    mostRecent
  }

  def remove( list:BasicDBList, id:Int ) {
    for ( t <- list.toSeq.of[DBObject] )
      if ( t.i( '_id ) == id )
        list.remove( t )
  }

  def sort( list:Seq[TimeTrack], newestFirst:Boolean = false ) = {
    if ( newestFirst )
      list.sortBy( _.on ).reverse
    else
      list.sortBy( _.on )
  }

  def count( list:Seq[TimeTrack] ):Int = list.length

  def parse( tid:String ) = {
    val idx = tid.lastIndexOf( '_' )

    val contentTid = tid.substring( 0, idx )
    val commentId = tid.substring( idx + 1 )._i

    ( contentTid, commentId )
  }

  def resolve( tid:String ) = {
    val ( taskContentTid, taskCommentId ) = parse( tid )

    val taskContent = Record.getByTid( taskContentTid ).as[Content]
    val comment = taskContent.commentById( taskCommentId )
 
    ( taskContent, comment )
  }
}

class TimeTrack( obj:DBObject, parent:MongoRecord ) extends MongoRecord( TimeTrack.makeView, obj, parent ) {
  def user = {
    val uid = oid( 'u )
    val user = B.User.getById( uid )

    if ( user == null )
      B.systemUser
    else
      user
  }

  def visit( block:TimeTrack => Unit ) {
    block( this )
  }

  def on = t( 'on )
  def displayDate = t( 'on )
  
  def collectUserTids( tids:mutable.Set[String] ) {
    val fromTid = tid( 'u )

    if ( fromTid.notBlank )
      tids += fromTid
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
  "s"              is DbBoolean              as "System"      is 'client; // System generated--- not editable or removable

  "task"           is DbChar(32)             as "Task"        is 'client; // tid of the task associated with this comment

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

  def visit( comments:BasicDBList, block:Comment => Unit ) {
    for ( c <- asComments( comments ) )
      c.visit( block )
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


  def collectTaskAndUserTids( taskTids:mutable.Buffer[String], userTids:mutable.Set[String], comments:Seq[Comment] ) {
    if ( comments != null ) {
      for ( c <- comments ) {
        val taskTid = c.s( 'task )

        if ( taskTid.notBlank )
          taskTids += taskTid

        val userTid = c.tid( 'u )

        if ( userTid.notBlank )
          userTids += userTid

        collectTaskAndUserTids( taskTids, userTids, c.comments )
      }
    }
  }
  
  def collectTaskTids( taskTids:mutable.Buffer[String], comments:Seq[Comment] ) {

    if ( comments != null ) {
      for ( c <- comments ) {
        val taskTid = c.s( 'task )

        if ( taskTid.notBlank )
          taskTids += taskTid

        collectTaskTids( taskTids, c.comments )
      }
    }
  }

  def remove( comments:BasicDBList, id:Int ) {
    for ( c <- comments.toSeq.of[DBObject] )
      if ( c.i( '_id ) == id ) {
        comments.remove( c )
      } else {
        remove( c.a_?( 'r ), id )
      }
  }

  def sort( comments:Seq[Comment], newestFirst:Boolean = false, byPageXy:Boolean = false, byWhen:Boolean = false ) = {

    if ( newestFirst )
      comments.sortBy( _.on ).reverse
      //comments.sortBy( _.mostRecentOn ).reverse // This is commented out until comment.js::sort handles mostRecentOn, so the two methods are consistent
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

  def parse( tid:String ) = {
    val idx = tid.lastIndexOf( '_' )

    val contentTid = tid.substring( 0, idx )
    val commentId = tid.substring( idx + 1 )._i

    ( contentTid, commentId )
  }

  def resolve( tid:String ) = {
    val ( taskContentTid, taskCommentId ) = parse( tid )

    val taskContent = Record.getByTid( taskContentTid ).as[Content]
    val comment = taskContent.commentById( taskCommentId )
 
    ( taskContent, comment )
  }
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

  def visit( block:Comment => Unit ) {
    block( this )

    Comment.visit( a_?( 'r ), block )
  }

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

  def collectUserTids( tids:mutable.Set[String] ) {

    val fromTid = tid( 'u )

    if ( fromTid.notBlank )
      tids += fromTid

    comments foreach { _.collectUserTids( tids ) }
  }
}


/*
 * * *  Content
 */

trait ContentMeta extends PrivateKeyEntity {

  "_id"               is DbMongoId            is 'id is 'client;

  override def init = {
    super.init

  "on"                is DbDateTime           is 'required is 'client;
  "type"              is DbLink(ContentType)  is 'required is 'client;

  "tags"              is DbArray(DbLink(Tag)) is ( new SearchTextLike() { override def extract( rec:Record, va:ViewAttribute ) = rec.as[Content].netTags } ) is 'client;
  "ctags"             is DbArray(DbLink(Tag)) help Text( "Child Tags -- these tags are inherited by child content." ) is 'client;

  "name"              is DbChar(50)           is 'label is 'required is SearchText is 'client;

  "pos"               is DbInt                is 'client; // the position of this content within its parent (group, folder, board, etc.) ... see the class "Positioning"

  "c"                 is DbLink(B.User)       as "Created By" is 'client;
  "o"                 is DbArray(DbTid(B.Org,B.User,Group)) as "Owners" is 'owner is 'client;
  "ownerTid"          is DbTid(B.User)        is 'temporary is 'client computed( _.as[Content].firstOwnerTid() )
  "isOwner"           is DbBoolean            is 'temporary is 'client computed( _.as[Content].isOwner( T.user ) );
  "isViewer"          is DbBoolean            is 'temporary is 'client computed( _.canView( T.user ) );
  //"ownerOrgTid"       is DbTid(B.Org)         is 'temporary is 'client computed( rec => B.Org.idToTid( B.User.byTid( rec.as[Content].firstOwnerTid() ).flatten( _.oid( 'org ), null ) )
  
  "v"                 is DbArray(DbTid(B.Org,B.User,Group)) as "Viewers" is SearchAuth is 'client is 'auth;

  "subscr"            is DbArray(DbTid(B.User)) as "Subscribers";
  "isSubscriber"      is DbBoolean            is 'temporary is 'client computed { _.as[Content].isSubscriber( T.user ) }
  

  "shown"             is DbArray(DbTid(B.User)) as "Shown To" ; // list of tids of users who have "read" this content; only maintained for some content types, like messages
  "hide"              is DbArray(DbTid(B.User)) as "Hidden From" ; // list of tids of users who have "read" this content; only maintained for some content types, like messages
 
  "complianceCode"    is DbChar(15)           is SearchToken;
  "expDate"           is DbDate               as "Expiration Date" is SearchText; 
  "approvalDate"      is DbDate               as "Approval Date" is SearchText; 
  
  "lastModified"      is DbDateTime           is 'required is SearchText is 'client;
  "lastModifiedBy"    is DbLink(B.User)       is 'required is 'client;
  "lastModifiedByOrg" is DbLink(B.Org)        is 'required;

  // Messages
  "fit"               is DbBoolean            as "From in To" help <div>Indicates that the posting user is in the to list.  This will normally be false unless a user explicitly addresses a message to themselves.</div>
  "s3"                is DbBoolean            help Text( "Indicates that the file or img is stored by us in S3." );

  "logTid"            is DbChar(64)           ; // TODO:  this is really a DbTid that contains an entity instead of a record
  "logId"             is DbMongoId            ; // TODO:  this is a link to the "*_log" table for the above entity, model it better

  "file"              is DbUrl                ; // TODO: change to DbFile ... if s3 this is a S3 path, otherwise it is an absolute URL
  "fileMimeType"      is DbChar(64)           is SearchToken;

  "video"             is DbBoolean            ;

  "taskComment"       is DbChar(32)           is 'client; // this is a commentTid (i.e. <content tid>_<comment id>)

  "dueDate"           is DbDate               is 'client; // this is a due date for cards, a milestone due date for boards, and a due date for projects


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

  "tt"                is DbArray(TimeTrack)   as "Time Tracks";
  
  // Image / Thumbnail
  "img"               is DbUrl                is 'client; // TODO:  change to DbImage?
  "imgH"              is DbInt                help Text( "The actual height of the image." );
  "imgW"              is DbInt                help Text( "The actual width of the image." );

  "netImg"            is DbUrl                as "Image" is 'temporary is 'client computed { _.as[Content].imageUrl( null ) }
//  "cardImg"           is DbUrl                as "Card Image" is 'temporary is 'client computed { c => val content = c.as[Content]; content.imageUrl( ContentEditHolder( content ) ) }
  
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
  
  "beingDeleted"      is DbBoolean            ;            
  
  "estVal"            is DbDouble             is 'client; // Estimated amount (hrs/money,etc)  
  "actVal"            is DbDouble             is 'client; // Actual amount (hrs/money,etc)
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
      case nop:Throwable => ;
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

  override def canView( rec:Record, viewer:org.tyranid.profile.User ) = rec.as[Content].canView( viewer )
}

object Content {
  // Thumbs are stored on S3 in the "thumbs" bucket:
  //   thumbs/[entity tid]/record tid/l
  lazy val thumbsBucket = new S3Bucket( "thumbs" )
  
  lazy val emailTag     = Tag.idFor( "email" )
  lazy val messageTag   = Tag.idFor( "message" )
  lazy val fileshareTag = Tag.idFor( "fileshare" )
  
  //val defaultColors = Array( "5f89a2", "597b7c", "93278f", "e72967", "f47b20", "ef5033", "009591", "9fd5b5", "1bb7ea", "73d0f9" )
  val defaultColors = Array( "1276bc","a73c15","98bb23","0c2074","77003a","f7941e","000341","d2891e","215429","34b44a","00aeef","560047","24c0bb","68162b","fdba2c" )
  val defaultCardColor = "faf082"
  val cardColors = Array( "34b27d", defaultCardColor, "e09952", "cb4d4d", "9933cc", "4d77cb" )  
}

abstract class Content( override val view:MongoView,
                        obj:DBObject = Mobj(),
                        override val parent:MongoRecord = null )
    extends MongoRecord( view, obj, parent ) with PrivateKeyRecord with HasText {

  def isLocked = b( 'locked )

  def contentType = ContentType.getById( i( 'type ) )
  def isFolder      = contentType == ContentType.Folder
  def isProject     = contentType == ContentType.Project
  def isLiteProject = contentType == ContentType.LiteProject
  def isProjectlike = isProject || isLiteProject

  def hasAnnotatedPages:Boolean = false
  
  def hasTag( tag:Int ) = a_?( 'tags ).exists( _ == tag )

  def s3Path:String = null
  
  def copy( ownerTid:String ):Content = {
    val content = this.clone().as[Content]
    
    content( '_id ) = null
    content( 'on ) = new Date
    
    content( 'o ) = Seq( ownerTid )
    content( 'v ) = Seq( ownerTid )
    
    if ( a_?( 'subscr ).length > 0 )
      content( 'v ) = Seq( ownerTid )
    
    content( 'shown ) = null
    content( 'hide ) = null

    content.stampLastModified
    
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
  
  def isDist = b( 'dist )
  
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
      ( imgUrl.toLowerCase.startsWith( "http" ) ? imgUrl | T.website() + imgUrl ) | null
    
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
  def thumbUrl( size:String ):String =
    if ( isLiteProject )
      rec( 'lite ).as[Content].thumbUrl( size )
    else
      "/io/thumb/" + tid + "/" + size + "?cb=" + s( 'img ).denull.hashCode

  def thumbHtml( size:String, extraHtml:NodeSeq = null ) =
    <div class={ thumbClass( size ) } style={ thumbStyle( size ) }>
     <img src={ thumbUrl( size ) }/>
     { ( extraHtml != null ) |* extraHtml }
    </div>
  
  def generateThumbs:Boolean = {
    val imgFile = try {
      imageForThumbs
    } catch {
      case _:Throwable => null
    }  
    
    if ( imgFile != null && imgFile.exists && imgFile.length > 0 ) {
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
    } else {
      if ( imgFile != null && imgFile.exists )
        imgFile.delete
        
      println( "Error: No suitable thumb file for content " + this.id._s )
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
   * * *   Task Comments
   */

  def taskTid = {
    val tc = s( 'taskComment )

    if ( tc.notBlank )
      Comment.parse( tc )._1
    else
      null
  }

  def resolvedTaskComment:( Content, Comment ) = {
    val tc = s( 'taskComment )

    if ( tc.notBlank )
      Comment.resolve( tc )
    else
      ( null, null )
  }


  /*
   * * *   Time Tracking
   */

  def timeTracks = TimeTrack.asTimeTracks( a_?( 'tt ) )

  def timeTrackCount = TimeTrack.count( timeTracks )
  
  def timeTracksCollectUserTids( tids:mutable.Set[String] ) = timeTracks.foreach { _.collectUserTids( tids ) }

  def timeTrackTid( tt:TimeTrack ) = tid + "_" + tt.id

  def timeTrackTidToId( tid:String ) =
    if ( tid.isBlank )
      0
    else
      tid.substring( this.tid.length + 1 )._i

  def mostRecentTimeTrack = TimeTrack.mostRecent( timeTracks )

  def timeTrackById( id:Int ) = TimeTrack.find( a_?( 'tt ), id )

  def timeTrack( user:User, time:Double ) = {
    val timeTracks = a_!( 'tt )
    val tt = TimeTrack( Mobj( "_id" -> ( TimeTrack.maxId( timeTracks) + 1 ), "on" -> new Date, "d" -> time, "u" -> user.id ) )

    save

    tt
  }

  def timeTrackRemove( id:Int ) = {
    TimeTrack.remove( a_!( 'tt ), id )
    save
  }

  /*
   * * *   Comments
   */

  def comments = Comment.asComments( a_?( 'r ) )

  def commentCount = Comment.count( comments )
  
  def commentsCollectUserTids( tids:mutable.Set[String] ) = comments.foreach { _.collectUserTids( tids ) }

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
  

  def userTids( arrayAttName:String ) = {
    val uTids = new mutable.ArrayBuffer[String]()
    
    a_?( arrayAttName ).toSeq.of[String].foreach { tid =>
      tid match {
      case userTid if B.User.hasTid( userTid ) && !uTids.contains( userTid ) =>
        uTids += userTid
      case groupTid if Group.hasTid( groupTid ) =>
        val g = Group.db.findOne( Mobj( "_id" -> Group.tidToId( groupTid ) ), Mobj( "v" -> 1 ) )
        
        g.a_?( 'v ).toSeq.of[String].foreach { guTid =>
          if ( B.User.hasTid( guTid ) && !uTids.contains( guTid ) )
            uTids += guTid
        }
      case _ =>
        // nop
      }
    }
    
    uTids.toSeq
  }
  
  def viewerUsers = {
    val users:mutable.ArrayBuffer[Record] = new mutable.ArrayBuffer[Record]
    userTids( "v" ).map( userTid => B.User.getByTid( userTid ) )
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

  def isTo( u:User ):Boolean = canView( u )

  /**
   * This is like "isTo()" except that this controls whether a user can see a participant in the content (either on the "v"/to list or a "reply" from them)
   */
  def canSee( u:User, seenTid:String ):Boolean = (
       this.a_?( 'o ).contains( u.tid )
    || u.allowProfileTids.contains( seenTid )
    || ( canViewDirectly( u ) && canViewDirectly( seenTid ) )
  )

  def isArchived = b( 'archived )

  def isOwner( user: org.tyranid.profile.User ): Boolean = isOwner( user.tid ) || ( user.hasOrg && isOwner( user.org.tid ) )

  def isOwner( tid: String ): Boolean = {
    if ( tid.isBlank )
      return false

    a_?( 'o ) foreach { t =>
      if ( t == tid )
        return true

      val ot = t._s

      if ( Group.hasTid( ot ) ) {
        val group = Group.getByTid( ot )

        if ( group.isOwner( tid ) )
          return true
      }
    }

    return false
  }

  def addOwner( tid:String ) = a_!( 'o ).addToSet( tid )

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

  /**
   * Directly means a user is specifically mentioned either via a group or themselves -- they're not connected simply by a group like the Public Group.
   */
  def canViewDirectly( user: org.tyranid.profile.User ):Boolean = {
    canViewDirectly( user.tid ) || ( user.hasOrg && canViewDirectly( user.org.tid ) )
  }
  
  def isMember( user:org.tyranid.profile.User ) = canEdit( user ) || canView( user )

  def canView( tid:String ):Boolean =
    T.permissionCache.getOrElseUpdate(
      this.tid + '|' + tid,
      tid.nonBlank &&
      a_?( 'v ).exists( t =>
        t == tid || t == B.publicGroup.tid || { //  ( B.User.hasTid( tid ) && t == B.publicGroup.tid ) || {
          val ot = t._s
       
          Group.hasTid( ot ) &&
          Group.byTid( ot ).pluck( _.canView( tid ), false )
        }
      )
    )

  def canViewDirectly( tid:String ):Boolean =
    tid.nonBlank &&
    a_?( 'v ).exists( t =>
      t == tid || { //  ( B.User.hasTid( tid ) && t == B.publicGroup.tid ) || {
        val ot = t._s
     
        Group.hasTid( ot ) &&
        Group.byTid( ot ).pluck( _.canView( tid ), false )
      }
    )


  /*
   * * *   Groups
   */

  var _groupTid:String = null
  var groupChecked = false
  
  def groupTid: String = {
    if ( _groupTid.isBlank && !groupChecked ) {
      groupChecked = true
      
      val gTid:String = a_?( 'o ).map( _._s ).find( Group.hasTid ).getOrElse( null )
  
      if ( gTid.isBlank ) {
        val gOid = obj.has( 'parentGroup ) ? oid( 'parentGroup ) | null
  
        if ( gOid == null )
          _groupTid = null
        else
          _groupTid = Group.idToTid( gOid )
      } else
        _groupTid = gTid
    }
    
    _groupTid
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

case class ContentEditHolder( content:Content) extends ContentEdit {
   def tempPath = ""
}


