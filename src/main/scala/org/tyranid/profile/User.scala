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

import org.tyranid.Imp.{ boolean, option }
import org.tyranid.db.Record

object User {

  private object currentVar extends SessionVar[User]( null )

  def current:User = currentVar.is

  def isLoggedIn = current.loggedIn
  def isAdmin    = current.admin

  import net.liftweb.sitemap.Loc._

  lazy val ReqLoggedIn = If( isLoggedIn _,  () => RedirectResponse("/user/login") )
  lazy val ReqAdmin    = If( isAdmin _,     () => RedirectResponse("/user/login") )
}

/*

   1.  create basic model classes

       org.tyranid.db.Entity
       org.tyranid.db.Attribute
       org.tyranid.db.Domain



   Attribute support



   Entity ---* Attribute


org.tyranid.ui.UiObject

   UiObject extends DBWrapObject

     val meta = Entity

     field( "pathname" )

     render( "pathname" )


 */

// TODO:  change from MongoRecord to Record once Record has accessors/mutators
class User extends org.tyranid.db.mongo.MongoRecord {

  var loggedIn = false
  var admin    = false

  /*

      1)  implement user/login

      2)  



      

      send an email to reset password

   */

}

