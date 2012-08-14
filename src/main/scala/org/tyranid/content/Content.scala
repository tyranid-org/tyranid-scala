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

import scala.xml.Text

import org.tyranid.Imp._
import org.tyranid.db.{ DbArray, DbBoolean, DbChar, DbDateTime, DbInt, DbLink, DbLong, DbTid, DbText, DbUrl, EnumEntity }
import org.tyranid.db.es.{ SearchAuth, SearchText }
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity }
import org.tyranid.db.ram.RamEntity
import org.tyranid.db.tuple.Tuple
import org.tyranid.profile.{ Group, Tag }
import org.tyranid.secure.PrivateKeyEntity


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

