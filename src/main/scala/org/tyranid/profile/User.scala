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

import org.apache.http.util.EntityUtils

import java.security.MessageDigest
import java.util.{ Date, TimeZone }

import scala.collection.mutable.ArrayBuffer

import org.bson.types.ObjectId
import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.content.{ Content, ContentType }
import org.tyranid.cloud.aws.S3
import org.tyranid.db.{ DbArray, DbBoolean, DbChar, DbDouble, DbEmail, DbLink, DbLong, DbPassword, Record, DbDate, DbInt, DbDateTime, DbUrl }
import org.tyranid.db.meta.TidItem
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity, MongoRecord }
import org.tyranid.db.ram.RamEntity
import org.tyranid.db.tuple.{ Tuple, TupleView }
import org.tyranid.http.{ Http, HttpResult }
import org.tyranid.image.DbThumbnail
import org.tyranid.sms.SMS
import org.tyranid.sso.SsoMapping
import org.tyranid.locale.{ Country, Language }
import org.tyranid.secure.DbReCaptcha
import org.tyranid.session.{ Session, ThreadData, WebSession }
import org.tyranid.web.{ Comet, WebContext }


object UserStatType extends RamEntity( tid = "a0Nt" ) {
  type RecType = UserStatType
  override def convert( view:TupleView ) = new UserStatType( view )

  "_id"     is DbInt      is 'id;
  "name"    is DbChar(64) is 'label;

  override val addNames = Seq( "_id", "name" )
  
  val LoginId          = 1
  val ApiId            = 2
  val CreateLiteId     = 3
  val StartConferenceId = 4
  val CreateLiteProId   = 5

  val Login      = add( LoginId,      "Login" )
  val Api        = add( ApiId,        "Api Call" )
  val CreateLite = add( CreateLiteId, "Create Lite" )
  val StartConference = add( StartConferenceId, "StartConference" )
  val CreateLitePro = add( CreateLiteProId, "CreateLitePro" )
}

case class UserStatType( override val view:TupleView ) extends Tuple( view )

object UserStat extends MongoEntity( tid = "a0Ot" ) {
  "_id"  is DbMongoId            is 'id is 'client;
  "s"    is DbLink(UserStatType) is 'required;
  "t"    is DbDateTime           is 'required;
  "u"    is DbLink(B.User)       is 'required is 'owner;
  "x"    is DbChar( 50 )         as "Extra";
  
  val index = {
    db.ensureIndex( Mobj( "s" -> 1, "u" -> 1, "t" -> 1 ) )
    db.ensureIndex( Mobj( "u" -> 1, "s" -> 1, "t" -> 1 ) )
  }
  
  private def create( userId:ObjectId, statId:Int, x:String = null ) {
    background( "Create UserStat" ) {
      val stat = UserStat.make
      stat( 's ) = statId
      stat( 't ) = new Date
      stat( 'u ) = userId
      
      if ( x.notBlank )
        stat( 'x ) = x
        
      stat.save
    }
  }
  
  def login( userId:Any ) = create( userId._oid, UserStatType.LoginId )
  def api( userId:Any, path:String ) = create( userId._oid, UserStatType.ApiId, path )
  def createLite( userId:Any ) = create( userId._oid, UserStatType.CreateLiteId )
  def startConference( userId:Any, sessId:String ) = create( userId._oid, UserStatType.StartConferenceId, sessId )
  def createLitePro( userId:Any ) = create( userId._oid, UserStatType.CreateLiteProId )
}

class UserStat( obj:DBObject, parent:MongoRecord ) extends MongoRecord( UserStat.makeView, obj, parent ) {}

object ContactInfo extends MongoEntity( tid = "a0Ov" ) {
  "_id"              is DbMongoId  is 'id;
  "name"             is DbChar(40)       ;
  "email"            is DbEmail          ;
  "createdAt"        is DbDateTime       ;
  "events"           is DbArray(DbChar(50));
  "company"          is DbChar(50)       ;
  "beta"             is DbBoolean        ;
  "inviteCode"       is DbBoolean        ;
  "lastInviteDate"   is DbDateTime       ;
  
  def ensure( email:String, name:String, company:String = null, beta:Boolean = false ) = {
    val contactInfoC = ContactInfo.db.find( Mobj( "email" -> email.toPatternI ) ).limit(1)
    val contactInfo = contactInfoC.hasNext ? contactInfoC.next | ContactInfo.make
    
    if ( contactInfo.isNew ) {
      contactInfo( 'name ) = name
      contactInfo( 'email ) =  email
      contactInfo( 'company ) = company
      
      if ( contactInfo.isNew )
        contactInfo( 'createdAt ) = new Date
            
      if ( beta )
        contactInfo( 'beta ) = true
        
      ContactInfo.db.save( contactInfo )
    }
    
    contactInfo
  }
}

class UserMeta extends MongoEntity( "a01v" ) {
  type RecType >: Null <: User
  override def convert( obj:DBObject, parent:MongoRecord ):RecType = throw new UnsupportedOperationException

  "_id"            is DbMongoId           is 'id is 'client;

  "email"          is DbEmail             is 'required is 'client is 'auth;
  "fullName"       is DbChar(64)          is 'client is 'label;
  "password"       is DbPassword          is 'required;
  "password2"      is DbPassword          is 'required is 'temporary as "Repeat Password";
  "thumbnail"      is DbThumbnail( "public" ) ;//is 'client as "Profile Image";
  "noEmail"        is DbBoolean           ; // Do not send to this user";
  "inactive"       is DbBoolean           is 'client is 'auth;

  "online"         is DbBoolean           is 'temporary is 'client is 'auth computed { rec => B.User.isOnline( rec.tid ) };

  "tz"             is DbChar(64)          ; // Olson timezone code ... i.e. "America/Chicago"
  "tzOff"          is DbDouble            ; // timezone offset in hours ... i.e. -6 ... maybe deprecated ? ... use an Olson code instead?

  "gender"         is DbLink(Gender)      ;
  "country"        is DbLink(Country)     ;
  "lang"           is DbLink(Language)    ;

  "numLogins"      is DbInt               ;
  "lastLogin"      is DbDateTime          is 'client is 'auth;
  "createdOn"      is DbDate              ;
  
  "recaptcha"      is DbReCaptcha( "white" ) is 'temporary as "Verify you are human";

  "activationCode" is DbChar(8)           as "Code";
  "resetCode"      is DbChar(8)           ;
  "loginToken"     is DbChar(10)          ;
  "monitored"      is DbBoolean           ;

  "sms"            is SMS                 ;

  "liid"           is DbChar(90)          ; // LinkedIn member id if linked
  "lit"            is DbChar(90)          ; // LinkedIn OAuth 1.0a token
  "lits"           is DbChar(90)          ; // LinkedIn OAuth 1.0a token secret

  "fbid"           is DbChar(90)          ; // Facebook id if linked
  "fbt"            is DbChar(90)          ; // Facebook OAuth 2.0 token
  "fbte"           is DbLong              ; // Facebook token expiration

  "bct"            is DbChar(90)          ; // Basecamp OAuth 2.0 token
  "bcte"           is DbLong              ; // Basecamp token expiration
  "bcid"           is DbChar(90)          ; // Basecamp refresh token if linked

  "twid"           is DbChar(90)          ; // Twitter member id if linked
  "twt"            is DbChar(90)          ; // Twitter OAuth 1.0a token
  "twts"           is DbChar(90)          ; // Twitter OAuth 1.0a token secret

  "goid"           is DbChar(90)          ; // Google id if linked
  "got"            is DbChar(90)          ; // Google OAuth 2.0 token
  "gote"           is DbLong              ; // Google token expiration
  "goft"           is DbChar(90)          ; // Google refresh token

  "eye"            is DbBoolean           ;
  "bids"           is DbArray(DbChar(10)) as "Browser IDs";
  "sso"            is DbLink(SsoMapping)  ;
  
  "n1"             is DbBoolean           ; 
  "ls"             is DbChar(30)          ; // Last Session ID 

  
  override def init = {
    super.init
    "invitedBy"    is DbLink(B.User)      ;
  }

  //def isLoggedIn = Session().isLoggedIn
  
  // TODO:  Make this more sophisticated, allow the entire user to be retrieved instead of just the name, and/or maybe something like ProfileItem
  def nameFor( userId:ObjectId ) = "TODO"

  def ensureUser( email:String, invitedBy:ObjectId = null ) = {
    val uc = db.find( Mobj( "email" -> ( "^" + email.encRegex + "$" ).toPatternI ) ).limit(1)    
    var u = uc.hasNext ? uc.next | null

    if ( u != null )
      apply( u )
    else
      createUser( email, invitedBy = invitedBy )
  }

  /* 
   * Check to see if the user has a Gravatar, and use it
   * if they do.
   */
  def setGravatar( userId:ObjectId, email:String ) {
    def md5Hex( message:String ):String = {
      def hex( array:Array[Byte]) = {
        val sb = new StringBuilder
        
        for ( a <- array )
          sb.append( Integer.toHexString( ( a & 0xFF ) | 0x100).substring( 1, 3 ) )        
        
        sb.toString()
      }
      
      try {
        return hex( MessageDigest.getInstance( "MD5" ).digest( message.getBytes( "CP1252" ) ) )
      } catch {
        case t:Throwable =>
      }
      
      return null
    }
    
    background( "Find Gravatar" ) {
      var res:HttpResult = null
      
      try {
        val iconUrl = "https://secure.gravatar.com/avatar/" + md5Hex( email ) + ".jpg?d=404"
        res = Http.GET( iconUrl )
        
        if ( res.response.getStatusLine.getStatusCode != 404 )
          B.User.db.update( Mobj( "_id" -> userId ), Mobj( $set -> Mobj( "thumbnail" -> iconUrl ) ) )
      } catch {
        case t:Throwable =>
          t.printStackTrace()
      } finally {
        if ( res != null )
          EntityUtils.consumeQuietly( res.response.getEntity )  
      }
    }
  }
  
  def createUser( email:String, possibleNames:String = null, invitedBy:ObjectId = null ) = {
    val user = make
    user( 'email ) = email
    user( 'activationCode ) = org.tyranid.math.Base62.make(8)
    user( 'createdOn ) = new Date
    
    if ( possibleNames.notBlank ) {
      val names = possibleNames.split( " " )
      
      if ( names.length > 1 ) {
        user( 'firstName ) = names(0)
        user( 'lastName ) = names(1)
      } else {
        user( 'firstName ) = possibleNames
        user( 'lastName ) = possibleNames
      }
    } else {
      user( 'firstName ) = "unknown"
      user( 'lastName ) = "unknown"
    }
    
    if ( invitedBy != null )
      user( 'invitedBy ) = invitedBy
    
    user.save
    
    setGravatar( user.id._oid, email )
    
    user
  }

  def isOnline( userTid:String ):Boolean =
    B.SessionData.db.findOne( Mobj( "u" -> B.User.tidToId( userTid ), "incognito" -> Mobj( $ne -> true ) ), Mobj( "_id" -> 1 ) ) != null
}

trait User extends MongoRecord {
  def isRegistered = s( 'activationCode ).isBlank

  def hasName = s( 'firstName ) != "unknown"

  def fullName =
    s( 'firstName ) + " " + s( 'lastName )

  def fullNameOrEmail =
    if ( hasName ) s( 'firstName ) + " " + s( 'lastName )
    else           s( 'email )

  def firstNameOrEmail =
    if ( hasName ) s( 'firstName )
    else           s( 'email )

  override def label =
    if ( hasName ) super.label
    else           s( 'email )

  def isActive:Boolean = {
    if ( ( obj.has( 'inactive ) && b( 'inactive ) ) || s( 'activationCode ).notBlank )
      return false
      
    return true
  }

  def email = s( 'email )

  /*
   *   anybody in a group that you're in ... including organizations and projects
   * + anybody in a board that you're in
   *
   *
   *
   * Option 1:  post-filtering
   *
   * 1. get all users that match the pattern
   *
   * 2. filter this list after the fact
   *
   *    cache groups and their members ...
   *
   * Option 2:  database filtering ... possibly via an extra index or "index collection"
   *
   *
   *
   *
   * Option 3:  create new database collection which by user has an array of all group and folder tids
   *
   *   query this collection where:
   *
   *      user $in [ raw list of users ],
   *      groups $in [ list of all my groups/folders ]
   *
   *
   *   ALTERNATE #1:
   *
   *   userVisibilityIndex {
   *     label: email-or-name-string,
   *     groups: array-of-groups-and-folders
   *   }
   *
   *   ALTERNATE #2:
   *
   *   store groups directly on user and then just create an index on users
   *
   *
   *  NOTE:  one large disadvantage of not doing the query entirely in mongo is that the initial query to bring back users can't have a limit() on it
   *
   */
  def canSee( seen:User ):Boolean = {
    this.id == seen.id ||
    ( this.hasOrg && this.org.group.canViewDirectly( seen ) ) ||
    this.teams.exists( _.canViewDirectly( seen ) ) ||
    this.projects.exists( _.canViewDirectly( seen ) )
  }

  // TODO:  this needs to be more sophisticated
  def canSee( seen:Org ):Boolean =
    seen.group.canView( this )

  def canSee( seenTid:String ):Boolean = {

    val rec = Record.getByTid( seenTid )

    rec match {
    case null         => false
    case seen:User    => canSee( seen )
    case seen:Org     => canSee( seen )
    case seen:Content => seen.canView( this )
    case _            => false
    }
  }

  /**
   * This is a list of tags that the user is interested in.
   */
  def allowTags:Seq[Int] = Nil

  def timeZone_=( tz:TimeZone ) =
    if ( tz != null ) obj( 'tz ) = tz.getID
    else              obj.remove( 'tz )

  def timeZone:TimeZone = {
    val tz = s( 'tz )

    if ( tz.notBlank )
      TimeZone.getTimeZone( tz )
    else
      null
  }

  def isGod = false
  
  /*
   * * *   Organizations
   */

  def hasOrg = false
  def org:Org = null // TODO:  this property is deprecated, use org.group as much as possible because eventually users will be able to be in multiple orgs

  def orgId   = if ( org != null ) org.oid else null
  def orgTid  = if ( org != null ) org.tid else null

  // note that the user must still be .save()'d
  def join( org:Org ) {
    obj( 'org ) = org.id
  }

  def toClientCommonMap( force:Boolean = false ):Map[String,Any]

  def groups:Seq[Group] = T.requestCached( tid + "groups"       ) { Group.visibleTo( this ) }

  // TODO:  cache this better somehow
  def teams:Seq[Group]  = T.requestCached( tid + "teams"        ) { groups.filter( _.contentType == ContentType.Team    ) }
  def userProjects      = T.requestCached( tid + "userProjects" ) { groups.filter( _.contentType == ContentType.Project ) }

  def projects          = T.requestCached( tid + "projects"     ) { userProjects ++ Group.publicProjects.filter( g => !userProjects.exists( _.id == g.id ) ).toSeq }

  def groupTids         = groups.map( _.tid )

  def teamIds           = teams.map( _.id )
  def teamTids          = teams.map( _.tid )

  def projectTids       = projects.map( _.tid )

  def allProjects       = T.requestCached( tid + "projects"     ) { groups.filter( g => g.contentType == ContentType.Project || g.contentType == ContentType.LiteProject ) ++
                                                                    Group.publicProjects.filter( g => !userProjects.exists( _.id == g.id ) ).toSeq }

    
  /**
   * This is a list of tids the user is authorized.  It includes their own tid, the tid of their org, and
   * the tid of all the groups they own or are members of.
   */
  def allowProfileTids =
    T.requestCached( "apt" ) {
      val tids = ArrayBuffer[String]()
      tids += tid

      val ot = orgTid
      if ( ot.notBlank )
        tids += ot

      tids ++= teamTids

      tids
    }
}
