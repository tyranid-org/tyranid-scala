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

import java.util.Date

import scala.collection.mutable
import scala.xml.{ NodeSeq, Unparsed }

import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.db.Scope
import org.tyranid.db.meta.TidItem
import org.tyranid.db.mongo.Imp._
import org.tyranid.email.Email
import org.tyranid.json.{ Js, JqHtml, JsModel, JsonString, JsData, JsValue }
import org.tyranid.logic.Invalid
import org.tyranid.math.Base62
import org.tyranid.secure.DbReCaptcha
import org.tyranid.session.{ Session, EmailCookie }
import org.tyranid.sso.SsoMapping
import org.tyranid.social.Social
import org.tyranid.ui.{ Button, Grid, Row, Focus, Form }
import org.tyranid.web.{ Weblet, WebContext, WebResponse }
import org.tyranid.web.WebHandledException
import org.tyranid.web.WebIgnoreException

object Loginlet extends Weblet {
  override val requiresLogin = false
  
  def handle( web: WebContext ) {
    val sess = T.session

    rpath match {
    case "/" =>
      if ( !web.xhr )
        web.redirect( T.website( "/#login", sess.user ) )

      web.jsRes()
    case "/in" =>
      if ( !web.xhr )
        web.redirect( T.website( "/#login", sess.user ) )
        
      val saving = web.b( "xhrSbt" )

      if ( saving ) {
        val email    = web.s( 'un )
        val password = web.s( 'pw )
        //val redirect = web.s( 'l  )

        val user =
          if ( email.isBlank )
            LoginCookie.getUser.getOrElse(null)
          else
            getUserByEmailPassword( email, password )

        if ( user == null || user.s( "activationCode" ).notBlank ) {
          if ( user != null ) {
            sess.error(
                "This account has not been activated yet!  Please check your email for the activation link.",
                 <a href={ "/log/resendActivation?id=" + user.id }>Send Again</a> )
          } else if ( email.isBlank )
            sess.warn( "Please log in." )

          web.jsRes()
        } else {
          copySocialLogins( sessionUser = sess.user, existingUser = user )
          sess.login( user, setAuth = true )

          if ( web.b( 'save ) )
            LoginCookie.set(user)

          web.jsRes( JsData( user ), JsModel( user.toClientCommonMap(), "common" ),
              Js( "V.app.newLoad( '#dashboard' );" ) )
        }
      } else {
        web.jsRes( Js( "router.navigate( '#login', { trigger : true } );" ) )
      }

    case "/out" =>
      val website = T.website( "/", sess.user )
      
      sess.logout()
      
      if ( !web.xhr )
        web.redirect( website )
        
      // r means the client is handling the redirect
      web.b( 'r ) ? web.jsRes() | web.jsRes( Js( "router.navigate( '#login', { trigger : true } );" ) )
    case "/register" =>      
      if ( !web.xhr )
        web.redirect( "/#register" )
        
      register( web, sess )
    case "/log" =>      
      log( web.b( 'a ) ? Event.Alert | Event.Log, "m" -> ( "Name: " + web.s( 'n ) + ", Message: " + web.s( 'm ) ) )
      web.jsRes()
      
    case "/company" =>
      val term = web.req.s( 'term ).toLowerCase
      def icon( thumb:String ) = thumb.notBlank ? thumb | B.Org.defaultIcon

      val json = B.Org.db.find( Mobj( "name" -> ( ".*" + term + ".*" ).toPatternI ), Mobj( "name" -> 1, "thumbnail" -> 1 ) ).
            limit( 8 ).
            toSeq.
            map( o => Map( "id"    -> B.Org.idToTid( o( '_id ) ),
                           "value" -> o.s( "name" ),
                           "label" ->
                              <div>
                               <div class="tno orgIcon" data-org={ B.Org.idToTid( o( '_id ) ) }>
                                <img src={ icon( o.s( 'thumbnail ) ) }/>
                               </div>
                               <div class="lbl">
                                { o.s( "name" ) }
                               </div>
                              </div> ) )

      web.res.json( json )
    case "/resendActivation" =>
      val userId = web.s( "id" ) or ""

      if ( userId == null ) {
        sess.notice( "Your user was not found." )
      } else {
        val u = B.User.byId( userId._oid ).getOrElse( null )
        val user = if ( u == null ) null else u

        if ( user != null ) {
          sess.notice( "Your activation code has been sent to your email and should be there shortly." )
          background { B.emailTemplates.welcome( user ) }
        } else {
          sess.notice( "Your user was not found." )
        }
      }

      T.web.redirect( "/?na=1" ) // na = need activation
    case "/forgot" =>
      if ( !web.xhr )
        return web.forward()

      val forgotCode = web.req.s( 'a )

      if ( forgotCode.isBlank ) {
        val email = web.req.s( 'un ) or sess.user.s( 'email )

        if ( email.isBlank ) {
          sess.warn( "Please enter in an email address." )
          return web.jsRes()
        }

        val dbUserc = B.User.db.find( Mobj( "email" -> email.toPatternI ) ).limit(1)
        val dbUser = dbUserc.hasNext ? dbUserc.next | null

        if ( dbUser == null ) {
          sess.warn( "Sorry, it doesn't look like the email address " + email + " is on " + B.applicationName + "." )
          return web.jsRes()
        }

        val activationCode = dbUser.s( 'activationCode )

        if ( activationCode.notBlank ) {
          sess.notice( "Your account has not been activated yet.  Your activation link has been sent to " + email + "." )
          background { B.emailTemplates.welcome( B.User( dbUser ), activationCode ) }
        } else {
          val user = B.User(dbUser)
          val resetCode = Base62.make(8)
          user('resetCode) = resetCode
          user.save

          sess.notice( "Instructions for changing your password have been sent to " + email + "." )

          B.emailTemplates.forgotPassword( user )
        }

        web.jsRes()
      } else {
        val dbUserc = B.User.db.find( Mobj( "resetCode" -> forgotCode ) ).limit(1)
        val dbUser = dbUserc.hasNext ? dbUserc.next | null

        if ( dbUser == null ) {
          sess.notice( "Account access code not found!" )
          web.jsRes()
        } else {
          val user = B.User(dbUser)
          user.remove( 'resetCode )
          sess.login( user, setAuth = true )
          user.save

          sess.notice( "You can now change your password in <em>My Profile</em>.", deferred = "/#dashboard" )
          web.jsRes( JsData( sess.user ), JsModel( user.toClientCommonMap(), "common" ), Js( "router.navigate( '/#dashboard', { trigger : true } );" ) )
        }
      }

    case "/tz" =>
      T.session.setTimeZoneFromClient( web.s( 'v ) )
      web.jsRes()
      
    case "/reconnect" =>
      val userTid = web.s( 'u )
      val user = B.User.getByTid( userTid )
      
      if ( user.s( 'ls ) == web.s( 's ) ) {
        sess.user = user
        sess.login( user, setAuth = true )
        web.jsRes( JsValue( T.session.httpSessionId ) )
      } else {
        web.jsRes()
      }
    case "/checkEmail" =>
      checkEmail( web, sess, web.s( 'email ) )
      
    case "/null" =>
      log( Event.RefInt, "m" -> ( "null, Referer: " + web.req.getHeader( "referer" ) ) )
    case _ =>
      throw new WebIgnoreException()
    }
  }

  def checkEmail( web:WebContext, sess:Session, email:String ) {
    if ( !Email.isWellKnownProvider( email ) ) {
      val domain = Email.domainFor( email )

      val orgc = B.Org.db.find( Mobj( "domain" -> ( "^" + domain.encRegex + "$" ).toPatternI ) ).limit(1)
      val org = orgc.hasNext ? B.Org( orgc.next ) | null

      if ( org != null ) {
        if ( !B.canAddUser( org ) ) {
          sess.error( "Sorry, " + org.s( 'name ) + " is licensed for a specfic number of seats, and none are available." )
          return web.jsRes()
        }
        
        T.user( 'org ) = org.id
        return web.jsRes( Js( "$('#company').val( '" + org.s( 'name ) + "' ).attr( 'readonly', 'readonly' );" ) )
      } 
      
      T.user( 'org ) = null
      return web.jsRes( Js( "$('#company').val( '' ).removeAttr( 'readonly' );" ) )
    } 
    
    T.user( 'org ) = null
    return web.jsRes( Js( "$('#company').val( '' ).removeAttr( 'readonly' );" ) )    
  }
  
  def register( web:WebContext, sess:Session ) {
    val user =
      sess.user match {
      case null => B.newUser()
      case u    => if ( ( u.isNew || u.s( 'activationCode ).notBlank ) && u.s( 'firstName ) == "unknown" ) {
                     u( "firstName" ) = ""
                     u( "lastName" ) = ""
                   }

                   u
    }

    if ( user.isNew  )
      sess.user = user

    if ( web.b( 'updateFld ) ) {
      val updateFld = web.s( 'updateFld )

      updateFld match {
        case "email" =>
          val email = web.s( 'email )

          if ( Email.isValid( email ) ) {
            val exists = B.User.db.exists( Mobj( "email" -> ("^" + email.encRegex + "$").toPatternI ) )

            if ( exists ) {
              sess.error( "Email is already in use." )
              return web.jsRes()
            }

            return checkEmail( web, sess, email )
          } 
          
          return web.jsRes()
        case "company" =>
          val exists = B.Org.db.exists( Mobj( "name" -> ("^" + web.s( 'company ).encRegex + "$").toPatternI ) )

          if ( exists ) {
            sess.error( "Company name is already in use." )
            return web.jsRes()
          }
        case _ =>
           return web.jsRes()
      }

      return
    }

    val password = web.s( 'password )

    if ( password.notBlank ) {
      if ( B.requireReCaptcha && !DbReCaptcha.passed ) {
        sess.error( "Invalid captcha, please try again." )
        return web.jsRes( Js( "Recaptcha.reload();" ) )
      }
      
      val email = web.s( 'email )
      val companyName = web.s( 'company ).trim

      if ( user.oid( 'org ) == null && companyName.notBlank ) {
        val exists = B.Org.db.exists( Mobj( "name" -> ("^" + companyName.encRegex + "$").toPatternI ) )

        if ( exists ) {
          sess.error( "Company name is already in use." )
          return web.jsRes()
        }
      }

      val firstName = web.s( 'firstName )

      user( 'email ) = email
      user( 'firstName ) = firstName
      user( 'lastName ) = web.s( 'lastName )
      user( 'password ) = password.shash()
      user( 'createdOn ) = new Date

      if ( user.s( 'activationCode ).isBlank ) {
        sendActivation( user )
        B.registerUser( user, companyName )
        EmailCookie.set( email )
        sess.logout( true )
        return web.jsRes( JsModel( user.toClientCommonMap(), "common" ), JsModel( Map( "email" -> email, "firstName" -> firstName ) ) )
      }

      sess.login( user, setAuth = true )
      user.remove( 'activationCode )

      B.registerUser( user, companyName )
      B.welcomeUserEvent

      return web.jsRes( JsData( user ), JsModel( user.toClientCommonMap( true ), "common" ), JsModel( Map( "dashboard" -> true ) ) )
    } 
    
    web.forward( js = "mainLoad( function() { router.navigate( '#register', { trigger: true } ); } );" )
  }

  def sendActivation( user:User ) = {
    val activationCode = Base62.make(8)
    user( 'activationCode ) = activationCode
    background { B.emailTemplates.welcome( user, activationCode ) }
  }

  def validateEmail( email:String ) =
    if ( email.isBlank ) {
      T.session.warn( "Please enter in an email address." )
      false
    } else if ( !email.isEmail ) {
      T.session.warn( "I'm sorry, \"" + email + "\" is not a valid email address.  Please enter in a valid email address." )
      false
    } else {
      true
    }

  def validateUnusedEmail( email:String ) =
    if ( !validateEmail( email ) ) {
      false
    } else if ( B.User.db.exists( Mobj( "email" -> ("^" + email.encRegex + "$").toPatternI ) ) ) {
      T.session.warn( "That email address is already being used." )
      false
    } else {
      true
    }

  def getUserByEmailPassword( email:String, pw:String ):User = {
    val sess = T.session

    if ( email.notBlank && pw.notBlank ) {
      if ( !validateEmail( email ) )
        return null

      if ( pw.isBlank ) {
        sess.warn( "Please enter in a password." )
        return null
      }

      val users = B.User.db.find( Mobj( "email" -> ("^" + email.encRegex + "$").toPatternI,
                      $or -> Array(
                        Mobj( "inactive" -> Mobj( $exists -> false ) ),
                        Mobj( "inactive" -> false ) )
                      ) ).toSeq

      for ( u <- users; dbPw = u.s( 'password ) )
        if ( dbPw.notBlank && pw.checkShash( dbPw ) )
          return B.User( u )
    }

    sess.error( "Invalid login.  Please try again." )

    null
  }

  private def copySocialLogins( sessionUser:User, existingUser:User ) {
    for ( app <- Social.networks;
          if sessionUser.s( app.idName ).notBlank ) {
      app.copyAttributes( from = sessionUser, to = existingUser )
      app.saveAttributes( existingUser )
    }
  }

  private def findUser( firstTryNetwork:String ):User = {
    val sess = T.session
    val app = Social.appFor( firstTryNetwork )

    val networks = app +: Social.networks.filter( _ != app )

    val suser = sess.user

    for ( app <- networks;
          uid = suser.s( app.idName );
          if uid.notBlank ) {
      val userc = B.User.db.find( Mobj( app.idName -> uid ) ).limit(1)
      val user = userc.hasNext ? userc.next | null

      if ( user != null )
        return B.User( user )
    }

    null
  }

  def dumpLoginState = {
    val sess = T.session
    val user = sess.user

    println( "*** Session ID: " + sess.id )

    for ( app <- Social.networks;
          if user.s( app.idName ).notBlank )
      println( "*** " + app.networkName + " " + app.idName + " = " + user.s( app.idName ) )
  }
}

