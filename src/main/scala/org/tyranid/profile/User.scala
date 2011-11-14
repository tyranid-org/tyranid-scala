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

import net.liftweb.http.{ RedirectResponse, S, SessionVar }

import org.tyranid.Bind
import org.tyranid.Imp._
import org.tyranid.db.{ Record, Scope }
import org.tyranid.session.Session

trait UserMeta {
  def isLoggedIn = { 
    if ( Session().user.loggedIn )
      true
    else {
      if ( Bind.LoginCookieName == null || Session().user.isLoggingOut )
        false
      else {
        val savedCookie = S.cookieValue( Bind.LoginCookieName ) openOr null

        var user = { 
          if ( savedCookie != null )
            Record.byTid( savedCookie, only = Bind.UserEntity ).map( _.asInstanceOf[User] ) getOrElse null
          else 
            null
        }

        if ( user == null )
          false
        else {
          user.loggedIn = true
          Session().user = user
          true
        }
      }
    }
  }
  
  def isAdmin    = Session().user.admin

  lazy val ReqLoggedIn = User._ReqLoggedIn
  lazy val ReqAdmin    = User._ReqAdmin
}

object User extends UserMeta {

  import net.liftweb.sitemap.Loc._

  private[profile] lazy val _ReqLoggedIn =
    If( isLoggedIn _,  () => {
      RedirectResponse("/user/login?l=" + S.uriAndQueryString.open_!.encUrl )
    } )
    
  private[profile] lazy val _ReqAdmin    =
    If( isAdmin _,     () => RedirectResponse("/user/login") )
}

trait User extends Record {

  var loggedIn = false
  var isLoggingOut = false
  var admin    = false

  def fullName = s( 'firstName ) + " " + s( 'lastName )

  /**
   * This is a list of tags that the user is interested in.
   */
  def allowTags:Seq[Int] = Nil
}

