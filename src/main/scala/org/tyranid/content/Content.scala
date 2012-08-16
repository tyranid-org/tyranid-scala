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
import org.tyranid.db.{ DbArray, DbBoolean, DbChar, DbDateTime, DbInt, DbLink, DbLong, DbTid, DbText, DbUrl, EnumEntity, Record }
import org.tyranid.db.es.{ SearchAuth, SearchText }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity, MongoRecord, MongoView }
import org.tyranid.db.ram.RamEntity
import org.tyranid.db.tuple.Tuple
import org.tyranid.image.Dimensions
import org.tyranid.profile.{ Group, Tag, User }
import org.tyranid.secure.{ PrivateKeyEntity, PrivateKeyRecord }


// TODO:  should this be in ui ?

object ViewType extends RamEntity( tid = "a13v" ) with EnumEntity[ViewType] {
  "_id"    is DbInt      is 'id;
  "name"   is DbChar(64) is 'label;

  def apply( id:Int, name:String ) = {
    val t = new ViewType
    t( '_id )  = id
    t( 'name ) = name
    t
  }

  val Post  = apply( 1, "Post"  )
  val Card  = apply( 2, "Card"  )
  val Table = apply( 3, "Table" )
  val Grid  = apply( 4, "Grid"  )

  static( Post, Card, Table, Grid )
}

class ViewType extends Tuple( ViewType.makeView )


object ContentType extends RamEntity( tid = "a10v" ) with EnumEntity[ContentType] {
  "_id"    is DbInt      is 'id;
  "name"   is DbChar(64) is 'label;

  def apply( id:Int, name:String ) = {
    val t = new ContentType
    t( '_id )  = id
    t( 'name ) = name
    t
  }

  val ChangeLog          = apply( 1, "ChangeLog" )
  val Message            = apply( 2, "Message" )
  val Content            = apply( 3, "Content" )
  val Folder             = apply( 4, "Folder" )
  val Document           = apply( 5, "Document" )

  static( ChangeLog, Message, Content, Folder, Document )
}

class ContentType extends Tuple( ContentType.makeView )



//
// Replies
//

object Reply extends MongoEntity( tid = "b00w", embedded = true ) {
  "on"             is DbDateTime           ;
  "m"              is DbChar(1024)         is 'label;

  override def init = {
    super.init
    "u"            is DbLink(B.User)       ;           // user who created the reply
  }

}


trait ContentMeta extends MongoEntity with PrivateKeyEntity {

  "_id"               is DbMongoId            is 'id;

  override def init = {
    super.init

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


  // Attachment / File
  "fileName"          is DbChar(128)          is 'required;
  "size"              is DbLong               ;
  "desc"              is DbText               is SearchText;

  // Messages
  "m"                 is DbChar(1024)         is 'label is SearchText;
  "r"                 is DbArray(Reply)       as "Replies";

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
    extends MongoRecord( view, obj, parent ) with PrivateKeyRecord {

  def contentType = ContentType( i( 'type ) )

  def hasTag( tag:Int ) = a_?( 'tags ).exists( _ == tag )


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
    val utid = a_?( 'o ).head.as[String]
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
   * * *   Security
   */

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

  def isWriter( user: org.tyranid.profile.User ): Boolean = isWriter( user.tid ) || ( ( user.org != null ) ? isWriter( user.org.tid ) | false )

  def isWriter( tid: String ): Boolean = {
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

        if (group.isMember(tid))
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


