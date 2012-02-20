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

import org.tyranid.Imp._
import org.tyranid.db.Record
import org.tyranid.session.{ Session, ThreadData }
import org.tyranid.web.{ WebContext, WebLock }


trait UserMeta {
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
}

case object UserLoginLock extends WebLock {

  def open( ctx:WebContext, td:ThreadData ):Boolean = {
    val user = td.user
    return user != null && user.loggedIn
  }

  def block( web:WebContext ) {
    web.redirect( "/log/in?l=" + web.req.uriAndQueryString.encUrl )
  }
}

object User extends UserMeta {
}

trait User extends Record {

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

