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

import com.mongodb.DBObject

import net.liftweb.http.{ RedirectResponse, SessionVar }

import org.tyranid.Imp.option
import org.tyranid.db.mongo.Imp._

object User {

  private object currentVar extends SessionVar[Option[User]]( None )

  def current:Option[User] = currentVar.is

  def isLoggedIn = current.flatten( _.loggedIn, false )
  def isAdmin    = current.flatten( _.admin, false )

  import net.liftweb.sitemap.Loc._

  lazy val ReqLoggedIn = If( isLoggedIn _,  () => RedirectResponse("/user/login") )
  lazy val ReqAdmin    = If( isAdmin _,     () => RedirectResponse("/user/login") )
}

class User {

  val db:DBObject = Mongo.obj

  var loggedIn:Boolean = false
  var admin:Boolean = false

  /*

      1)  implement user/login

      2)  



      

      send an email to reset password

   */

}

