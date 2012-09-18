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

import scala.xml.Text

import com.mongodb.{ BasicDBList, DBObject }

import org.tyranid.Imp._
import org.tyranid.cloud.aws.{ S3Bucket, S3 }
import org.tyranid.db.{ DbArray, DbBoolean, DbChar, DbDateTime, DbDouble, DbInt, DbLink, DbLong, DbTid, DbText, DbUrl, Entity, Record }
import org.tyranid.db.es.{ SearchAuth, SearchText }
import org.tyranid.db.meta.TidItem
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity, MongoRecord, MongoView }
import org.tyranid.db.ram.RamEntity
import org.tyranid.db.tuple.{ Tuple, TupleView }
import org.tyranid.image.{ Dimensions, Thumbnail }
import org.tyranid.http.Http
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

  "_id"            is DbInt                is 'id;

  "on"             is DbDateTime           ;
  "m"              is DbChar(1024)         is 'label;

  "pn"             is DbInt                as "Page Number";
  "x"              is DbDouble             as "X";
  "y"              is DbDouble             as "Y";

  "r"              is DbArray(Comment)     as "Replies";

  override def init = {
    super.init
    "u"            is DbLink(B.User)       ;           // user who created the reply
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

  def remove( comments:BasicDBList, id:Int ) {
    for ( c <- comments.toSeq.of[DBObject] )
      if ( c.i( '_id ) == id )
        comments.remove( c )
      else
        remove( c.a_?( 'r ), id )
  }

  def sort( comments:Seq[Comment], newestFirst:Boolean ) = {

    if ( newestFirst )
      comments.sortBy( _.mostRecentOn ).reverse
    else
      comments.sortBy( _.on )
  }
}

class Comment( obj:DBObject, parent:MongoRecord ) extends MongoRecord( Comment.makeView, obj, parent ) {

  def m = s( 'm )

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

  def comments = Comment.asComments( a_?( 'r ) )

  def hasAnnotation = has( 'pn ) || has( 'x ) || has ('y )

  def annotationType:String = has( 'pn ) ? "page" | ( ( has( 'x ) ? "xy" ) | null )

  def on = t( 'on )
  def displayDate = t( 'on )

  def mostRecentOn:Date = {
    val c = comments

    if ( c.nonEmpty )
      on max c.map( _.mostRecentOn ).max
    else
      on
  }
}


/*
 * * *  Content
 */

trait ContentMeta extends PrivateKeyEntity {

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

  "lastModified"      is DbDateTime           is 'required;
  "lastModifiedBy"    is DbLink(B.User)       is 'required;
  "lastModifiedByOrg" is DbLink(B.Org)        is 'required;

  // Volees
  "fit"               is DbBoolean            as "From in To" help <div>Indicates that the posting user is in the to list.  This will normally be false unless a user explicitly addresses a message to themselves.</div>
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
  "img"               is DbUrl                /* TODO:  change to DbImage? */;
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
  
  
  "color"             is DbChar(6)            ;
  }
  
  private def deleteThumbs( tid:String ) {
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
}

abstract class Content( override val view:MongoView,
                        obj:DBObject = Mobj(),
                        override val parent:MongoRecord = null )
    extends MongoRecord( view, obj, parent ) with PrivateKeyRecord with HasText {

  val isBuiltin = b( 'builtin )

  def contentType = ContentType.getById( i( 'type ) )

  def hasAnnotatedPages:Boolean = false
  
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
  
  // 260 x 169 (Dashboard)
  // 140 x 91 (Timeline)
  // 100 x 65 (Project header)  
  // 40 x 26 (Dashboard drop-down)
  def imageForThumbs:File = {
    val imgUrl = imageUrl( false )
    val dlUrl:String = ( imgUrl.notBlank ) ?
      ( imgUrl.toLowerCase.startsWith( "http" ) ? imgUrl | T.website + imgUrl ) | null
    
    ( dlUrl.notBlank ) ? Http.GET_File( dlUrl ) | null
  }

  def thumbClass( size:String ) =
    size match {
    case "l" => "thumbLarge"
    case "m" => "thumbMedium"
    case "s" => "thumbSmall"
    case "t" => "thumbTiny"
    }

  def thumbHtml( size:String ) =
    <div class={ thumbClass( size ) }>
     <img src={ "/io/thumb/" + tid + "/" + size }/>
    </div>
  
  def generateThumbs {
    val imgFile = imageForThumbs

    if ( imgFile != null ) {
      val pathParts = tid.splitAt( 4 )
      val urlPath = pathParts._1 + "/" + pathParts._2 + "/"
      
      def thumb( s:String, w:Int, h:Int ) {
        var f:File = null
        
        try {
          f = Thumbnail.generate( imgFile, w, h )
          if ( f != null ) {
            S3.write( Content.thumbsBucket, urlPath + s, f )
            S3.access( Content.thumbsBucket, urlPath + s, true )
          }
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

  def lastModifiedByTidItem = TidItem.by( B.User.idToTid( oid( 'lastModifiedBy ) ) )

  def lastModifiedByUser = {
    val user = B.User.getById( this( 'lastModifiedBy )._oid )

    if ( user == null )
      B.systemUser
    else
      user
  }


  /*
   * * *   Author
   */

  def ownerTidItem = TidItem.by( firstOwnerTid() )

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

  def commentTid( comment:Comment ) = tid + "_" + comment.id

  def commentTidToId( tid:String ) =
    if ( tid.isBlank )
      0
    else
      tid.substring( this.tid.length + 1 )._i

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

  def commentById( id:Int ) = Comment.find( a_?( 'r ), id )

  def comment( msg:String, user:User, replyTo:Comment = null, pageNumber:Int = 0, x:Double = 0.0, y:Double = 0.0 ) = {

    val comments = a_!( 'r )

    val comment = Comment( Mobj( "_id" -> ( Comment.maxId( comments ) + 1 ), "on" -> new Date, "m" -> msg, "u" -> user.id ) )

    if ( pageNumber != 0 )
      comment( 'pn ) = pageNumber

    if ( x != 0.0 && y != 0.0 ) {
      comment( 'x ) = x
      comment( 'y ) = y
    }
      
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

  def comments = Comment.asComments( a_?( 'r ) )

  def annotatedPages = comments.map( _.i( 'pn ) ).distinct



  /*
   * * *   Security
   */

  def isJustTo( user:User ) = {
    val v = viewerTids
    v.size == 1 && v( 0 ) == user.tid
  }

  def owners = {
   val tids = ownerTids

   for ( e <- ownerEntities;
         en = e.as[MongoEntity];
         r <- en.db.find( Mobj( "_id" -> Mobj( $in -> tids.filter( en.hasTid ).map( tid => en.tidToId( tid ) ).toSeq.toMlist ) ) );
         rec = en( r ) )
      yield rec
  }

  def writers = owners
  
  def viewers = {
   val tids = viewerTids

   for ( e <- viewerEntities;
         en = e.as[MongoEntity];
         r <- en.db.find( Mobj( "_id" -> Mobj( $in -> tids.filter( en.hasTid ).map( tid => en.tidToId( tid ) ).toSeq.toMlist ) ) );
         rec = en( r ) )
     yield rec
  }

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

