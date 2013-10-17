/**
 * Copyright (c) 2008-2013 Tyranid <http://tyranid.org>
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

import org.tyranid.Imp._
import org.tyranid.db.Record
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.MongoEntity
import org.tyranid.net.Uri


object LoginCookie {

  lazy val name =
    B.loginCookieName +
    ( if ( B.DEV )        "-dev"
      else if ( B.STAGE ) "-stage"
      else if ( B.BETA )  "-beta"
      else                "" )

  def getUser:Option[User] = {
    val cv = T.web.req.cookieValue( name, domain = B.domain )
    
    if ( cv != null ) {
      cv.splitFirst( '|' ) match {
      case ( tid, token ) if !tid.endsWith( "null" ) =>
        return Record.byTid( tid, only = B.User ).
          map( _.asInstanceOf[User] ).
          // loginToken is an attribute that exists on user
          filter( _.s( 'loginToken ) == token )

      case _ =>
      }
    }
    
    None
  }

  def set( user:User ) = {
    val loginToken = org.tyranid.math.Base62.make( 10 )

    val cookie = new javax.servlet.http.Cookie( name, user.tid + "|" + loginToken )
    cookie.setMaxAge(60 * 60 * 24 * 14) // two weeks
    cookie.setPath("/")
    cookie.setSecure( true )
    cookie.setDomain( B.domain )

    T.web.res.addCookie(cookie)
            
    user( "loginToken" ) = loginToken
    B.User.db.update( Mobj( "_id" -> user.id ), Mobj( $set -> Mobj( "loginToken" -> loginToken ) ) )
  }

  def remove = {
    T.web.res.deleteCookie( name, domain = B.domain )
  }

  def autoLogin = {
    val sess = T.session
    val user = sess.user

    if ( !sess.isLoggedIn && !T.http.isLoggingOut && !user.b( 'inactive ) ) {
      val user = LoginCookie.getUser.of[User].getOrElse( null )

      if ( user != null ) {
        sess.login( user )
        sess.setAutoLoggedIn
      }
    }
  }
}

