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

package org.tyranid.session

import javax.servlet.http.HttpSession

import scala.collection.mutable

import org.tyranid.Imp._
import org.tyranid.db.mongo.Imp._
import org.tyranid.web.WebContext


object AccessLog {

  def log( web:WebContext, thread:ThreadData ) {
    val session = thread.session

    if ( !session.loggedUser ) {
      val user = session.user

      if ( user.loggedIn ) {
        Log.log( Log.Access, "ua" -> web.req.getHeader( "User-Agent" ) )
        session.loggedUser = true
        session.loggedEntry = true
      }
    }

    if ( !session.loggedEntry && thread.http != null ) {
      Log.log( Log.Access, "ua" -> web.req.getHeader( "User-Agent" ) )
      session.loggedEntry = true
    }
  }
}

