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


import net.liftweb.http.{ RedirectResponse, SessionVar }

import org.tyranid.Bind
import org.tyranid.Imp.{ boolean, option, symbol }
import org.tyranid.db.{ Record, Scope }

trait UserMeta {

  def current:User           = User.currentVar.is
  def current_=( user:User ) = User.currentVar.set( user )

  def isLoggedIn = User.current.loggedIn
  def isAdmin    = User.current.admin

  lazy val ReqLoggedIn = User._ReqLoggedIn
  lazy val ReqAdmin    = User._ReqAdmin
}

object User extends UserMeta {

  private[profile] object currentVar extends SessionVar[User]( Bind.NewUser() )

  import net.liftweb.sitemap.Loc._

  private[profile] lazy val _ReqLoggedIn = If( isLoggedIn _,  () => RedirectResponse("/user/login") )
  private[profile] lazy val _ReqAdmin    = If( isAdmin _,     () => RedirectResponse("/user/login") )
}

trait User extends Record {

  var loggedIn = false
  var admin    = false
}

