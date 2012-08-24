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

import java.util.Date

import scala.xml.Text

import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.db.{ DbArray, DbBoolean, DbChar, DbDateTime, DbInt, DbLink, DbLong, DbTid, DbText, DbUrl, Entity, Record }
import org.tyranid.db.es.{ SearchAuth, SearchText }
import org.tyranid.db.meta.TidItem
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity, MongoRecord, MongoView }
import org.tyranid.db.ram.RamEntity
import org.tyranid.db.tuple.{ Tuple, TupleView }
import org.tyranid.image.Dimensions
import org.tyranid.io.HasText
import org.tyranid.profile.{ Group, GroupType, GroupMode, Tag, User }
import org.tyranid.secure.{ PrivateKeyEntity, PrivateKeyRecord }


// TODO:  should this be in ui ?

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
}

case class ContentType( override val view:TupleView ) extends Tuple( view )



/*
 * * *  Comments
 */

object Comment extends MongoEntity( tid = "b00w", embedded = true ) {
  type RecType = Comment
  override def convert( obj:DBObject, parent:MongoRecord ) = new Comment( obj, parent )


  "on"             is DbDateTime           ;
  "m"              is DbChar(1024)         is 'label;

  override def init = {
    super.init
    "u"            is DbLink(B.User)       ;           // user who created the reply
  }

}

class Comment( obj:DBObject, parent:MongoRecord ) extends MongoRecord( Comment.makeView, obj, parent ) {

  def displayDate = t( 'on )

  def fromUser = {
    val uid = oid( 'u )
    val user = B.User.getById( uid )

    if ( user == null )
      B.systemUser
    else
      user
  }
}


/*
 * * *  Content
 */

trait ContentMeta extends MongoEntity with PrivateKeyEntity {

  "_id"               is DbMongoId            is 'id;

  override def init = {
    super.init

  "builtin"           is DbBoolean            help Text( "Builtin content is maintained by the system and is not editable by end users." );

  "on"                is DbDateTime           is 'required;
  "type"              is DbLink(ContentType)  is 'required;
  "tags"              is DbArray(DbLink(Tag)) ;
  "name"              is DbChar(50)           is 'label is 'required is SearchText;

  "o"                 is DbArray(DbTid(B.Org,B.User,Group)) as "Owners" is 'owner;
  "v"                 is DbArray(DbTid(B.Org,B.User,Group)) as "Viewers" is SearchAuth;
  "subV"              is DbArray(DbTid(B.Org,B.User))                   ; // for showing content inside a group

  "lastModified"      is DbDateTime is 'required;
  "lastModifiedBy"    is DbLink(B.User) is 'required;
  "lastModifiedByOrg" is DbLink(B.Org) is 'required;

  // Volees
  "fit"               is DbBoolean            as "From in To" help <div>Indicates that the posting user is in the to list.  This will normally be false unless a user explicitly addresses a volee to themselves.</div>
  "s3"                is DbBoolean            help Text( "Indicates that the file or img is stored by us in S3." );

  "logTid"            is DbChar(64)           ; // TODO:  this is really a DbTid that contains an entity instead of a record
  "logId"             is DbMongoId            ; // TODO:  this is a link to the "*_log" table for the above entity, model it better

  "file"              is DbUrl /* TODO:  change to DbFile */        ; // if s3 this is a S3 path, otherwise it is an absolute URL
  "fileMimeType"      is DbChar(64)           ;

  "video"             is DbBoolean            ;


  // Groups / Projects
  "groupType"         is DbLink(GroupType)    ;
  "groupMode"         is DbLink(GroupMode)    ;

  // Attachment / File
  "fileName"          is DbChar(128)          is 'required;
  "size"              is DbLong               ;
  "desc"              is DbText               is SearchText;

  // Messages
  "m"                 is DbChar(1024)         is 'label is SearchText;
  "r"                 is DbArray(Comment)     as "Replies";

  // Image / Thumbnail
  "img"               is DbUrl /* TODO:  change to DbImage? */;
  "imgH"              is DbInt                help Text( "The actual height of the image." );
  "imgW"              is DbInt                help Text( "The actual width of the image." );
  
  
  // Attachment
  "title"             is DbChar(128)          ;
  "link"              is DbUrl                ;
  "icon"              is DbUrl /* TODO:  should be DbImage ? */     ; // for links, this is the favicon
  
  // RSS & Atom Feeds
  "feedOut"           is DbBoolean            help <span>If this is enabled, outgoing <b>public</b> RSS and Atom feeds are generated.</span>;
  "feed"              is DbMongoId // should be DbLink(Feed)                               ;
  "feedItemId"        is DbChar(128)          ;
  }
}


object Content {
  lazy val emailTag     = Tag.idFor( "email" )
  lazy val messageTag   = Tag.idFor( "message" )
  lazy val fileshareTag = Tag.idFor( "fileshare" )
}

abstract class Content( override val view:MongoView,
                        obj:DBObject = Mobj(),
                        override val parent:MongoRecord = null )
    extends MongoRecord( view, obj, parent ) with PrivateKeyRecord with HasText {

  val isBuiltin = b( 'builtin )

  def contentType = ContentType.getById( i( 'type ) )

  def hasTag( tag:Int ) = a_?( 'tags ).exists( _ == tag )

  override def text:String = {
    // If I am/have a document (FileSystem):
    //  1) Do I have an externalId ?  If so, look up the document service (crocodoc) and get the text from that
    //  2) TextExtractors.extract( file )
    //
    ""
  }

  /*
   * * *  Label and Icons
   */
  
  override def label =
    if ( has( 'name ) )       s( 'name )
    else if ( has( 'title ) ) s( 'title )
    else                      "n/a"

  def titleInDesc( title:String, desc:String ):Boolean = {
    var t = title
    if ( t.endsWith( "..." ) )
      t = title.substring( 0, t.length - 3 )

    desc.startsWith( t ) ||
    desc.stripPrefix( "<a href=\"" ).startsWith( t ) ||
    title.startsWith( "http://" )
  }
  
  def hasImage = imageUrl( false ).notBlank

  def imageUrl( editing:Boolean ) = s( 'img )

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
        Dimensions( 128, 128 )
      //}
    }
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


  /*
   * * *   Author
   */

  def fromUser = {
    val utid = firstOwnerTid()
    val user = B.User.getByTid( utid )

    if ( user == null )
      B.systemUser
    else
      user
  }

  def fromIcon =
    if ( this.obj.has( 'feed ) ) "/images/rssLarge.png"
    else                         fromUser.s( 'thumbnail )


  /*
   * * *   Comments
   */

  def mostRecentComment = {
    val comments = a_?( 'r )

    if ( comments.size > 0 )
      Comment( comments.last.as[DBObject] )
    else if ( s( 'm ).notBlank )
      Comment(
        Mobj(
          "on" -> this( 'on ),
          "m"  -> this( 'm ),
          "u"  -> fromUser.id
        )
      )
    else
      null
  }


  /*
   * * *   Security
   */

  def owners = {
   for ( e <- ownerEntities;
         en = e.as[MongoEntity];
         r <- en.db.find( Mobj( "_id" -> Mobj( $in -> obj.a_?( "o" ).map( tid => en.tidToId( tid._s ) ).toSeq.toMlist ) ) );
         rec = en( r ) )
      yield rec
  }

  def writers = owners
  
  def readers =
   for ( e <- viewerEntities;
         en = e.as[MongoEntity];
         r <- en.db.find( Mobj( "_id" -> Mobj( $in -> obj.a_?( "v" ).map( tid => en.tidToId( tid._s ) ).toSeq.toMlist ) ) );
         rec = en( r ) )
     yield rec

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

  def viewerTids = obj.a_?( 'v ).toSeq.of[String]

  // TODO:  make this name more generic / less Volee-ish
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
    
    if ( from == null ) {
//spam( a_?( 'o ) )
      return false
    }

    val groupTids = u.groupTids
    for ( tid <- viewers;
          if Group.hasTid( tid ) && groupTids.contains( tid );
          grp <- Group.byTid( tid );
          if grp.canSee( u, from ) )
      return isSubTo

    false
  }

  /**
   * This is like "isTo()" except that this controls whether a user can see a participant in the volee (either on the "v"/to list or a "reply" from them)
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

  def isOwner( user: org.tyranid.profile.User ): Boolean = isOwner( user.tid ) || ( ( user.org != null ) ? isOwner( user.org.tid ) | false )

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

        if ( group.collaborative || group.isOwner( tid ) )
          return true
      }
    })

    return false
  }


  /*
   * NOTE:  isWriter() currently is the same as isOwner() but this might change in the future
   */

  def isWriter( user: org.tyranid.profile.User ) = isOwner( user )
  def isWriter( tid: String ):Boolean            = isOwner( tid )


  def isReader( user: org.tyranid.profile.User ): Boolean = isReader( user.tid ) || ( ( user.org != null ) ? isReader( user.org.tid ) | false )

  def isReader( tid: String ): Boolean = {
    if ( tid.isBlank )
      return false

    val viewers = a_?( 'v )

    viewers.foreach( t => {
      if ( t == tid )
        return true

      val ot = t._s
      
      if ( Group.hasTid( ot ) ) {
        val group = Group.getByTid( ot )

        if (group.isReader(tid))
          return true
      }
    })

    return false
  }


  /*
   * * *   Groups
   */

  lazy val groupTid: String = {
    val gTid: String = a_?('o).map(_._s).find(Group.hasTid).getOrElse(null)

    if (gTid.isBlank) {
      val gOid = oid('parentGroup)

      if (gOid == null)
        null
      else
        Group.idToTid(gOid)
    } else
      gTid
  }

  def group = groupTid.isBlank ? null | Group.getByTid(groupTid)


  /*
   * * *   Child Content (Groups and Folders)
   */

  def contents:Seq[Content] = Nil


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

  def fileMimeType = {
    if ( obj.has( 'fileMimeType ) )
      s( 'fileMimeType )
    else
      org.tyranid.io.File.mimeTypeFor( s( 'fileName ) )          
  }
}


/*
 * * *  Content Ordering
 */

object ContentOrder extends MongoEntity( tid = "a0St", embedded = true ) {
  type RecType = ContentOrder
  override def convert( obj:DBObject, parent:MongoRecord ) = new ContentOrder( obj, parent )

  override def init {
    super.init

  // indicates that a is before b

  "a"              is DbTid( B.ContentEntities:_* ) ;
  "b"              is DbTid( B.ContentEntities:_* ) ;

  }
}

class ContentOrder( obj:DBObject, parent:MongoRecord ) extends MongoRecord( Comment.makeView, obj, parent )

