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

import net.liftweb.http.{ RedirectResponse, S }

import org.tyranid.Imp._
import org.tyranid.db.{ Record, Scope }
import org.tyranid.session.{ Session, ThreadData }
import org.tyranid.web.{ WebContext, WebLock }

trait UserMeta {
  def isLoggedIn = { 
    if ( Session().user.loggedIn )
      true
    else {
      if ( Tyr.loginCookieName == null || Session().user.isLoggingOut )
        false
      else {
        val savedCookie = S.cookieValue( Tyr.loginCookieName ) openOr null

        var user = { 
          if ( savedCookie != null )
            Record.byTid( savedCookie, only = Tyr.userEntity ).map( _.asInstanceOf[User] ) getOrElse null
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

  // TODO:  Make this more sophisticated, allow the entire user to be retrieved instead of just the name, and/or maybe something like ProfileItem
  def nameFor( userId:ObjectId ) = "TODO"
}

case object UserLoginLock extends WebLock {

  def open( ctx:WebContext, td:ThreadData ):Boolean = {
    val user = td.user
    return user != null && user.loggedIn
  }

  def block( ctx:WebContext ) {
    ctx.res.sendRedirect( "/sign/in?l=" + ctx.req.uriAndQueryString.encUrl )
  }
}

object User extends UserMeta {

  import net.liftweb.sitemap.Loc._

  private[profile] lazy val _ReqLoggedIn =
    If( isLoggedIn _,  () => {
spam( "HERE 1" )
      RedirectResponse( "/sign/in?l=" + S.uriAndQueryString.open_!.encUrl )
    } )
    
  private[profile] lazy val _ReqAdmin    =
    If( isAdmin _,     () => RedirectResponse( "/sign/in" ) )
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

