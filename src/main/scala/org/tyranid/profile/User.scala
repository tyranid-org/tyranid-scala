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

object User {

  private object currentVar extends SessionVar[User]( Bind.NewUser() )

  def current:User = currentVar.is
  def current_=( user:User ) = currentVar.set( user )

  def isLoggedIn = current.loggedIn
  def isAdmin    = current.admin

  import net.liftweb.sitemap.Loc._

  lazy val ReqLoggedIn = If( isLoggedIn _,  () => RedirectResponse("/user/login") )
  lazy val ReqAdmin    = If( isAdmin _,     () => RedirectResponse("/user/login") )
}

trait User extends Record {

  var loggedIn = false
  var admin    = false
}

