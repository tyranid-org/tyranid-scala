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

import java.util.{ Date, TimeZone }

import scala.collection.mutable.ArrayBuffer

import org.bson.types.ObjectId
import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.content.ContentType
import org.tyranid.db.{ DbArray, DbBoolean, DbChar, DbDouble, DbEmail, DbLink, DbLong, DbPassword, Record, DbDate, DbDateTime }
import org.tyranid.db.meta.TidItem
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity, MongoRecord }
import org.tyranid.image.DbThumbnail
import org.tyranid.sms.SMS
import org.tyranid.locale.{ Country, Language }
import org.tyranid.secure.DbReCaptcha
import org.tyranid.session.{ Session, ThreadData }
import org.tyranid.ui.LnF
import org.tyranid.web.WebContext


object ContactInfo extends MongoEntity( tid = "a0Ov" ) {
  "_id"              is DbMongoId  is 'id;
  "name"             is DbChar(40)       ;
  "email"            is DbEmail          ;
  "createdAt"        is DbDateTime       ;
  "company"          is DbChar(50)       ;
  "beta"             is DbBoolean        ;
  "inviteCode"       is DbBoolean        ;
  "lastInviteDate"   is DbDateTime       ;
  
  def ensure( email:String, name:String, company:String = null, beta:Boolean = false ) = {
    val contactInfo = ContactInfo.db.findOrMake( Mobj( "email" -> email.toPatternI ) )
        
    contactInfo( 'name ) = name
    contactInfo( 'email ) =  email
    contactInfo( 'company ) = company
        
    if ( contactInfo.isNew )
      contactInfo( 'createdAt ) = new Date
          
    if ( beta )
      contactInfo( 'beta ) = true
      
    ContactInfo.db.save( contactInfo )
    
    contactInfo
  }
}

class UserMeta extends MongoEntity( "a01v" ) {
  type RecType >: Null <: User
  override def convert( obj:DBObject, parent:MongoRecord ):RecType = throw new UnsupportedOperationException

  "_id"            is DbMongoId           is 'id;
  "email"          is DbEmail             is 'required;
  "fullName"       is DbChar(64)          is 'label;
  "password"       is DbPassword          is 'required;
  "password2"      is DbPassword          is 'required is 'temporary as "Repeat Password";
  "thumbnail"      is DbThumbnail( "public" ) as "Profile Image";
  "noEmail"        is DbBoolean           ;// No sent to this user";
  "inactive"       is DbBoolean           ;

  "tzOff"          is DbDouble            ; // timezone offset in hours ... i.e. -6
  "gender"         is DbLink(Gender)      ;
  "country"        is DbLink(Country)     ;
  "lang"           is DbLink(Language)    ;

  "lastLogin"      is DbDateTime          ;
  "createdOn"      is DbDate              ;
  
  "recaptcha"      is DbReCaptcha( "white" ) is 'temporary as "Verify you are human";

  "activationCode" is DbChar(8)           as "Code";
  "resetCode"      is DbChar(8)           ;
  "loginToken"     is DbChar(10)          ;

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

  "eye"            is DbBoolean           ;
  "bids"           is DbArray(DbChar(10)) as "Browser IDs";
  
  "lnf"            is DbLink(LnF)         ;

  override def init = {
    super.init
    "invitedBy"    is DbLink(B.User)      ;
  }

  
  def isLoggedIn = Session().user.loggedIn
  
  def isAdmin    = Session().user.admin

  // TODO:  Make this more sophisticated, allow the entire user to be retrieved instead of just the name, and/or maybe something like ProfileItem
  def nameFor( userId:ObjectId ) = "TODO"

  def ensureUser( email:String, invitedBy:ObjectId ) = {

    var u = db.findOne( Mobj( "email" -> ( "^" + email.encRegex + "$" ).toPatternI ) )

    if ( u != null )
      apply( u )
    else
      createUser( email, invitedBy = invitedBy )
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
    user
  }
}

trait User extends MongoRecord {

  var loggedIn     = false
  var isLoggingOut = false
  var admin        = false


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
    if ( b( 'inactive ) )
      return false
      
    // TODO:  check for SSO here 
    if ( s( 'password ).isBlank ) // && s( 'ssoToken ).isBlank
      return false
      
    return true
  }

  /**
   * This is a list of tags that the user is interested in.
   */
  def allowTags:Seq[Int] = Nil


  @volatile var timeZone:TimeZone =
    // TODO:  persist time zone in user record, edit it in UI, etc.
    TimeZone.getDefault


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



  // TODO:  cache this better somehow
  def groups:Seq[Group] = T.requestCached( tid + "groups" ) { Group.visibleTo( this, contentType = ContentType.Group ) }
  def groupIds          = groups.map( _.id )
  def groupTids         = groups.map( _.tid )

  def nonBuiltinGroups    = T.requestCached( tid + "nonBuiltinGroups" ) { Group.visibleTo( this, contentType = ContentType.Group, allowBuiltins = false ) }
  def nonBuiltinGroupIds  = nonBuiltinGroups.map( _.id )
  def nonBuiltinGroupTids = nonBuiltinGroups.map( _.tid )

  // TODO:  cache this better somehow
  def projects = T.requestCached( tid + "projects" ) { Group.visibleTo( this, contentType = ContentType.Project ) }
  def projectIds  = projects.map( _.id )
  def projectTids = projects.map( _.tid )

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

      tids ++= groupTids

      tids
    }

  def allowProfileProjectTids =
    T.requestCached( "appt" ) {
      val tids = ArrayBuffer[String]()
      tids += tid

      val ot = orgTid
      if ( ot.notBlank )
        tids += ot

      tids ++= projectTids

      tids
    }

  def inNetwork( tid:String ):Boolean = {
    if ( tid == this.tid )
      return true

    var tidOrgId = TidItem.by( tid ).org
    org != null && tidOrgId != null && org.id == tidOrgId
  }
}
