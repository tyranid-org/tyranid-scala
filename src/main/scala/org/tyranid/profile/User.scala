/**
 * Copyright (c) 2008-2011 Tyranid <http://tyranid.org>
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


import java.util.TimeZone

import org.bson.types.ObjectId
import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.db.{ DbBoolean, DbChar, DbDouble, DbEmail, DbLink, DbLong, DbPassword, Record }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity, MongoRecord }
import org.tyranid.image.DbImage
import org.tyranid.locale.{ Country, Language }
import org.tyranid.secure.DbReCaptcha
import org.tyranid.session.{ Session, ThreadData }
import org.tyranid.web.WebContext


class UserMeta extends MongoEntity( "a01v" ) {
  "id"             is DbMongoId           is 'key;
  "email"          is DbEmail             is 'label is 'required;
  "password"       is DbPassword          is 'required;
  "password2"      is DbPassword          is 'required is 'temporary as "Repeat Password";
  "thumbnail"      is DbImage( "public" ) as "Profile Image";

  "tzOff"          is DbDouble            ; // timezone offset in hours ... i.e. -6
  "gender"         is DbLink(Gender)      ;
  "country"        is DbLink(Country)     ;
  "lang"           is DbLink(Language)    ;

  "recaptcha"      is DbReCaptcha( "white" ) is 'temporary as "Verify you are human";

  "stayLoggedIn"   is DbBoolean           as "Keep me logged in for two weeks";
  "activationCode" is DbChar(8)           ;
  "resetCode"      is DbChar(8)           ;
  "loginToken"     is DbChar(10)          ;

  "org"            is DbLink( B.Org )     ;

  "liid"           is DbChar(90)          ; // LinkedIn member id if linked
  "lit"            is DbChar(90)          ; // LinkedIn OAuth 1.0a token
  "lits"           is DbChar(90)          ; // LinkedIn OAuth 1.0a token secret

  "fbid"           is DbChar(90)          ; // Facebook id if linked
  "fbt"            is DbChar(90)          ; // Facebook OAuth 2.0 token
  "fbte"           is DbLong              ; // Facebook token expiration

  def isLoggedIn = { 
    val session = Session()

    if ( session.user.loggedIn )
      true
    else if ( B.loginCookieName == null || session.user.isLoggingOut )
      false
    else {
      LoginCookie.getUser match {
      case Some( user ) =>
        user.loggedIn = true
        session.user = user
        true

      case None =>
        false
      }
    }
  }
  
  def isAdmin    = Session().user.admin

  // TODO:  Make this more sophisticated, allow the entire user to be retrieved instead of just the name, and/or maybe something like ProfileItem
  def nameFor( userId:ObjectId ) = "TODO"

  override def apply( obj:DBObject ):User = throw new UnsupportedOperationException
}

trait User extends MongoRecord {

  var loggedIn     = false
  var isLoggingOut = false
  var admin        = false

  def fullName = s( 'firstName ) + " " + s( 'lastName )

  /**
   * This is a list of tags that the user is interested in.
   */
  def allowTags:Seq[Int] = Nil

  @volatile var timeZone:TimeZone =
    // TODO:  persist time zone in user record, edit it in UI, etc.
    TimeZone.getDefault
}

