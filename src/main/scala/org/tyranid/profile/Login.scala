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

import scala.xml.{ NodeSeq, Unparsed }

import org.tyranid.Imp._
import org.tyranid.db.Scope
import org.tyranid.db.meta.TidItem
import org.tyranid.db.mongo.Imp._
import org.tyranid.email.Email
import org.tyranid.json.{ Js, JqHtml, JsonString }
import org.tyranid.logic.Invalid
import org.tyranid.math.Base62
import org.tyranid.secure.DbReCaptcha
import org.tyranid.session.Session
import org.tyranid.social.Social
import org.tyranid.sso.SsoMapping
import org.tyranid.ui.{ Button, Grid, Row, Focus, Form }
import org.tyranid.web.{ Weblet, WebContext, WebTemplate, WebResponse }
import org.tyranid.web.WebHandledException

object Register {
  def sendActivation( user:User ) = {
    val activationCode = Base62.make(8)
    
    user( 'activationCode ) = activationCode

    background { B.emailTemplates.welcome( user, activationCode ) }
  }

  def finishPage( user:User, companyName:String = null ) = 
   <div class="container" style="background: rgb(64,64,65);background: rgba(64,64,65,0.4);margin: 0 auto;width: 740px;border-radius: 8px;padding: 16px;">
     <div style="text-align:center;background: url(https://d33lorp9dhlilu.cloudfront.net/images/volerro_logo_notag_reversed.png) no-repeat 0px 0px;height: 50px;background-position-x: center;"></div>
     <div>
      <form method="post" action="/user/register" id="f" class="register" style="margin-bottom:12px;">
       <fieldset class="registerBox">
        <div class="top-form-messages"></div>
        <div class="container-fluid" style="padding:0;">
         <div class="row-fluid">
          <h1 class="span12">Thanks, { user.s( 'firstName ) }!</h1>
         </div>
        </div>
        <div class="container-fluid" style="padding:0;padding-top:1em;">
         <div class="row-fluid">
          <div class="span12">A verification email was sent to <b>{ user.s( 'email ) }</b>.</div>
          { companyName.notBlank |* <div>Click the activation link in your email to gain access to { companyName.possessive } network and projects.</div> }
         </div>
         <hr/>
        </div> 
       </fieldset>
      </form>
     </div>
    </div>
}

object Loginlet extends Weblet {
 def box = {
    val thread = T
    val user = thread.user
    val web = thread.web
    val loggingOut = web.req.s( 'lo ).notBlank
    val noSocial = web.b( 'nosocial )

    val params = noSocial |* "?nosocial=1"

    <script>{ Unparsed( """
$( function() {
  $('#forgot').click(function(e) {
    var un = $( "#un" );
    var msg = "";
    var fldVal = un.val();
        
    if ( !fldVal ) {
      msg = "Please enter an email address";
    } else if ( !T.Validator.validEmail( fldVal ) ) {
      msg = "Please enter a valid email address";
    }
       
    var formHandler = T.FormHandler.get( un.get() );
        
    if ( msg ) {
      formHandler.topEl().empty();
      T.addTopMsg( formHandler.topEl(), "error", msg );
      return false;
    }

    formHandler.topEl().empty();
    T.navTo( '/log/forgot?xhr=1&un=' + encodeURIComponent( fldVal ) );
  });
  
  T.closeModal( ".modal" );
  $( "body" ).addClass( "front" );
  T.initFormPlaceholders( "#f" );
});
""" ) }
    </script> ++
    <form method="post" action={ wpath + "/in" } id="f" class="login" style="margin-bottom:12px;" data-val="1" data-val-top="1">
     <fieldset class="loginBox">
      <div class="container-fluid" style="padding:0;">
       <div class="row-fluid">
        <h1 class="span5">Sign-in</h1>
        <div class="span7 pull-right regLink">or <a data-sbt={ Form.attrJson( Map( "href" -> ( wpath + "/register" + params ), "top" -> 1 ) ) }>Register for { B.applicationName }!</a></div>
       </div>
      </div>
      <hr style="margin:4px 0 30px;"/>
      <div class="top-form-messages"></div>
      <div class="container-fluid" style="padding:0;">
       <div class="row-fluid">
        <input type="email" id="un" name="un" placeholder="Email" autocapitalize="off" data-val="req,email"/>
        { Focus("#un") }
       </div>
       <div class="row-fluid">
        <input type="password" name="pw" id="pw" placeholder="Password" autocapitalize="off" data-val="req"/>
       </div>
       <div class="row-fluid">
         <div class="span6">
          <div style="height:40px;display:inline-block;"><input type="checkbox" name="save" id="saveLogin" value="Y"/></div>
          <label for="saveLogin" style="vertical-align:text-top;display:inline-block;">Stay signed-in</label>
         </div> 
         <div class="span6" style="height:40px;padding-top:8px;"><button type="submit" class="btn-success btn pull-right">Sign-In <i class="icon-caret-right"></i></button></div>
       </div>
       <hr style="margin:24px 0 0;"/>
      </div>
     </fieldset>
     <!--input type="hidden" name="l" value={ web.req.s("l") }/-->
    </form> ++
    <div class="container-fluid" style="padding:0;">
     <a href="#" id="forgot" class="pull-right">Forgot your password?</a>
    </div>
  }

  def handle( web: WebContext ) {
    val sess = T.session
    val noSocial = web.b( 'nosocial )

    rpath match {
    case "/in" | "/" =>
      val saving = web.b( "xhrSbt" )
      
      if ( web.b( "xhr" ) && !saving ) {
        val jsonRes = web.jsonRes( sess )
        
        jsonRes.htmlMap = Map( 
              "html" -> B.loginPage(),
              "target" -> "#main",
              "transition" -> "fadeOutIn",
              "duration" -> 500 )

        jsonRes.extraJS = "T.initFormPlaceholders( '#f' );"
        web.json( jsonRes )              
      } else {
        val email    = web.s( 'un )
        val password = web.s( 'pw )
        val redirect = web.s( 'l  )
  
        val user =
          if ( email.isBlank )
            LoginCookie.getUser.getOrElse(null)
          else
            getUserByEmailPassword( email, password )
  
        if ( user == null || user.s( "activationCode" ).notBlank ) {
          if (user != null) {
            notActivatedYet( user )
          } else if ( email.isBlank ) 
            sess.warn( "Please log in." )
  
           val jsonRes = web.jsonRes( sess )
           
           if ( !sess.hasErrors )
             jsonRes.redirect = redirect.isBlank ? T.website() | ( T.website() + "/?l=" + redirect.encUrl )
             
           jsonRes.extraJS = "T.initFormPlaceholders( '#f' );"
           web.json( jsonRes )
        } else {
          copySocialLogins( sessionUser = sess.user, existingUser = user )
          sess.login( user )
  
          if ( web.b( 'save ) )
            LoginCookie.set(user)
  
          web.jsRes( Js( "V.common.set( " + user.toClientCommonMap().toJsonStr( client = true ) + " ); V.app.load( '" + ( redirect.isBlank ? "/#dashboard" | redirect ) + "' );" ) )
        }
      }
    case "/clear" =>
      web.html( NodeSeq.Empty )
    case "/out" =>
      val org = sess.user.org
      val hasOrg = !org.isNew
      val sso = hasOrg ? SsoMapping.db.findOne( Mobj( "org" -> org.id ) ) | null
      val ssoLoe:String = ( sso == null ) ? null | sso.s( 'loEndpoint ) 
      val website = ( ssoLoe.isBlank ) ? T.website( "/?lo=1" + ( web.b( 'xhr ) ? "&xhr=1" | "" ), sess.user ) | ssoLoe
      
      sess.logout()
      web.redirect( website )
    case s if s.startsWith( "/in" ) =>
      socialLogin( s.substring( 3 ) )
    case s if s.startsWith( "/register" ) =>
      registerRetail( web, sess )
      return
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
      import org.bson.types.ObjectId
      
      val userId = web.s( "id" ) or ""
      
      if ( userId == null ) {
        T.session.notice( "Your user was not found." )
      } else {
        val u = B.User.byId( new ObjectId( userId ) ).getOrElse( null ) 
        val user = if ( u == null ) null else u
        
        if ( user != null ) {
          T.session.notice( "Your activation code has been sent to your email and should be there shortly." )
          background { B.emailTemplates.welcome( user ) }
        } else {
          T.session.notice( "Your user was not found." )
        }
      }
      
      T.web.redirect( "/?na=1" ) // na = need activation
    case "/forgot" =>
      val forgotCode = web.req.s( 'a )

      if ( forgotCode.isBlank ) {
        val email = web.req.s( 'un ) or sess.user.s( 'email )

        if ( email.isBlank ) {
          sess.warn( "Please enter in an email address." )          
          return web.json( web.jsonRes( sess ) )
        }

        val dbUser = B.User.db.findOne( Mobj( "email" -> email ) )

        if ( dbUser == null ) {
          sess.warn( "Sorry, it doesn't look like the email address " + email + " is on " + B.applicationName + "." )
          
          return web.json( web.jsonRes( sess ) )
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
        
        return web.json( web.jsonRes( sess ) )
      } else {
        val dbUser = B.User.db.findOne(Mobj("resetCode" -> forgotCode))

        if ( dbUser == null ) {
          sess.notice( "Account access code not found!", deferred = "/#dashboard" )
          //web.redirect( "/log/in" )
          web.jsRes( Js( "V.common.clear().set(V.common.defaults); V.app.load( '/#dashboard' )" ) )
        } else {
          val user = B.User(dbUser)
          user.remove('resetCode)
          sess.login( user )
          user.save
          
          sess.notice( "You can now change your password in <em>My Profile</em>.", deferred = "/#dashboard" )
          web.jsRes( Js( "V.common.set( " + user.toClientCommonMap().toJsonStr( client = true ) + " ); V.app.load( '/#dashboard' )" ) )
        }
      }

    case "/tz" =>
      T.session.setTimeZoneFromClient( web.s( 'v ) )

    case "/null" =>
      log( Event.RefInt, "m" -> ( "null, Referer: " + web.req.getHeader( "referer" ) ) ) 
    case _ =>
    }
  }
  
 def registerRetail( web:WebContext, sess:Session ) {
    if ( web.b( 'updateFld ) ) {
      val updateFld = web.s( 'updateFld )
      
      updateFld match {
        case "activationCode" =>
          val contact = ContactInfo.db.findOne( Mobj( "inviteCode" -> web.s( 'activationCode ) ) )
              
          if ( contact != null ) {
            web.jsRes( Js( """ 
  $('#email').val( '""" + contact.s( 'email ) + """' );
  $('#firstName').val( '""" + contact.s( 'name ).split( ' ' )(0) + """' );
  $('#lastName').val( '""" + contact.s( 'name ).split( ' ' )(1) + """' );""" ) ) 
          } else {
            sess.error( "Invalid invite code" )
            web.jsRes()
          }
        case "email" =>
          val email = web.s( 'email )
          
          val exists = B.User.db.exists( Mobj( "email" -> ("^" + email.encRegex + "$").toPatternI ) )

          if ( exists ) {
            sess.error( "Email is already in use." )
            web.jsRes()
          }
          
          val domain = Email.domainFor( email )
          val org = B.Org.db.findOne( Mobj( "domain" -> domain.toPatternI ) )
          
          if ( org != null ) {
            T.user( 'org ) = org.id
            web.jsRes( Js( "$('#company').val( '" + org.s( 'name ) + "' ).attr( 'readonly', 'readonly' );" ) )
          } else {
            T.user( 'org ) = null
            web.jsRes( Js( "$('#company').val( '' ).removeAttr( 'readonly' );" ) )
          }
        case "company" =>
          val exists = B.Org.db.exists( Mobj( "name" -> ("^" + web.s( 'company ).encRegex + "$").toPatternI ) )

          if ( exists ) {
            sess.error( "Company name is already in use." )
            web.jsRes()
          }
        case _ => 
           web.jsRes()
      }
      
      return
    }
    
    val keep = web.b( 'keep )
    
    val user =
      sess.user match {
      case null => B.newUser()
      case u    => if ( u.isNew || keep ) {
                     if ( keep && u.s( 'firstName ) == "unknown" ) {
                       u( "firstName" ) = ""
                       u( "lastName" ) = ""
                     }
                     u
                   } else
                     B.newUser()
    }

    if ( user.isNew  ) {
      sess.user = user
  
      user.extraVaValidations =
        ( user.view( 'email ),
          { scope:Scope =>
            B.User.db.exists( Mobj( "email" -> user.s( 'email ) ) ) |*
              Some( Invalid( scope.at( 'email ), user.s( 'email ) + " is already in use.") )
          } ) ::
          Nil
          
      val inviteCode = web.s( 'code )
      
      if ( inviteCode.notBlank ) {
        user( 'activationCode ) = inviteCode
        
        val contact = ContactInfo.db.findOne( Mobj( "inviteCode" -> inviteCode ) )
        
        if ( contact != null ) {
          user( 'email ) = contact.s( 'email )
          user( 'firstName ) = contact.s( 'name ).split( ' ' )(0)
          user( 'lastName ) = contact.s( 'name ).split( ' ' )(1)
        }
      }
    }

    val ui = user.view.ui( "registerrb" )

    user.isAdding = true
    
    val doRecaptcha = B.requireReCaptcha

    if ( web.b( 'xhrSbt ) ) {
      val jsonRes = web.jsonRes( sess )
      val invalids = Scope( user, initialDraw = false, captcha = doRecaptcha ).submit( user, ui )

      if ( invalids.isEmpty ) {
        user( 'createdOn ) = new Date

        val email = user.s( 'email )
        val companyName = web.s( 'company ).trim 
        
        if ( user.oid( 'org ) == null && companyName.notBlank ) {
          val exists = B.Org.db.exists( Mobj( "name" -> ("^" + companyName.encRegex + "$").toPatternI ) )

          if ( exists ) {
            sess.error( "Company name is already in use." )
            web.jsRes()
            return
          }
        }
          
        if ( user.s( 'activationCode ).isBlank ) {
          Register.sendActivation( user )
          B.registerUser( user, companyName )
          web.jsRes( JqHtml( "#main", Register.finishPage( user, companyName ) ) )
          sess.logout( true )
          return
        } else {          
          sess.login( user )
          user.remove( 'activationCode )
          
          B.registerUser( user, companyName )
          B.welcomeUserEvent
            
          web.jsRes( Js( "V.common.set( " + user.toClientCommonMap( true ).toJsonStr( client = true ) + " ); V.app.load( '/#dashboard' );" ) )            
          return
        }
      } else {
        for ( i <- invalids )
          sess.error( i.message )
          
        if ( doRecaptcha )
          jsonRes.extraJS = "Recaptcha.reload();"
      }
      
      jsonRes.extraJS = "T.initFormPlaceholders( '#f' );"      
      web.json( jsonRes )
      return
    }

    val orgName:String = {
      val userEmail = user.s( 'email )
    
      if ( !user.isNew && userEmail.notBlank ) {
        val orgId = user.oid( 'org )
        
        if ( orgId != null ) {
          TidItem.by( B.Org.idToTid( orgId ) ).label
        } else {
          val domain = Email.domainFor( userEmail )
          val org = B.Org.db.findOne( Mobj( "domain" -> domain.toPatternI ) )
            
          if ( org != null ) {
            T.user( 'org ) = org.id
            org.s( 'name )
          } else {
            ""
          }
        }
      } else {
        ""
      }
    }
        
    // TODO:  When we get rid of IE8 support, then get rid of the onClick event on the agreement checkbox
    
    val inner =
   <div style="text-align:center;background: url(https://d33lorp9dhlilu.cloudfront.net/images/volerro_logo_notag_reversed.png) no-repeat 0px 0px;height: 50px;background-position-x: center;"></div> ++
   <div>
    <form method="post" action={ wpath + "/register" } id="f" class="register" style="margin-bottom:12px;" data-val="1">
     { keep |* <input type="hidden" name="keep" value="1"/> }
     <fieldset class="registerBox">
      <div class="container-fluid" style="padding:0;">
       <div class="row-fluid">
        <h1 class="span12">Register</h1>
       </div>
      </div>
      <hr style="margin:4px 0 30px;"/>
      <div class="top-form-messages"></div>
      <div class="container-fluid" style="padding:0;">
       <div class="row-fluid">
         <div class="container-fluid span12" style="padding:0;">
          <div class="row-fluid">
           <div class="span3"><input type="text" id="firstName" name="firstName" value={ user.s( 'firstName ) } placeholder="First Name" data-val="req" data-val-with="lastName"/></div>
           <div class="span3"><input type="text" id="lastName" name="lastName" value={ user.s( 'lastName ) } placeholder="Last Name" data-val="req" data-val-with="firstName"/></div>
           { Focus("#firstName") }
           <div class="span6 val-display"></div>
           <div class="span6 hints" style="position:relative;">
            <div>Hint: <b>This will be the email address we send your account activation link to</b>.  Use your company or organization email address to easier connect with co-workers.</div> 
           </div>  
          </div>
          <div class="row-fluid">
           <div class="span6">
            { user.isNew ?
              <input type="text" name="email" id="email" value={ user.s( 'email ) } placeholder="Email address" data-update="blur" data-update-url={ wpath + "/register" } data-val="req,email"/> |
              <input type="text" name="email" id="email" value={ user.s( 'email ) } readonly="readonly" placeholder="Email address"/>
            }
           </div>
           <div class="span6 val-display"></div>
          </div>
          <div class="row-fluid">
           <div class="span6">
            { orgName.notBlank ?
            <input type="text" name="company" id="company" value={ orgName } placeholder="Company Name (not required)" readonly="readonly" data-update="blur" data-update-url={ wpath + "/register" }/> |
            <input type="text" name="company" id="company" value={ orgName } placeholder="Company Name (not required)" data-update="blur" data-update-url={ wpath + "/register" }/>
            }
           </div>
           <div class="span6 val-display"></div>
          </div>
          <div class="row-fluid">
           <div class="span6">
            <input type="password" name="password" id="password" placeholder="Password" data-val="req,min=7"/>
           </div>
           <div class="span6 val-display"></div> 
          </div>
          <div class="row-fluid">
           <div class="span6">
            <input type="hidden" name="keep" value={ web.s( 'keep ) }/>
            <input type="password" name="password2" id="password2" placeholder="Re-type password" data-val="req,same=password,min=7"/>
           </div>
           <div class="span6 val-display"></div>
          </div>
          { doRecaptcha |*
          <div class="row-fluid">
           <div class="span6">
            <script>{ Unparsed( "jQuery.getScript( \"" + DbReCaptcha.scriptSrc + "\" );" + DbReCaptcha.showFunction( "white" ) ) }</script>
            { DbReCaptcha.div }
           </div>
           <div class="span6 val-display"></div> 
          </div>
        }
          <div class="row-fluid">
           <div class="span6">
            <input type="checkbox" name="agreement" id="agreement" style="height:20px;width:20px;" onClick="$( '#regBtn' ).attr( 'disabled', $( this ).is(':checked' ) ? false : true );" onChange="$( '#regBtn' ).attr( 'disabled', $( this ).is(':checked' ) ? false : true );"/> <label style="display:inline;padding-left:0;vertical-align:text-bottom;" class="extra" for="agreement">I agree to Volerro <a href="http://www.volerro.com/site/policies.html" style="text-decoration:none;border-bottom:1px dotted;" target="_terms">terms of use</a> policy.</label>
           </div>
          </div>
         </div>
       </div>
       <hr style="margin:10px 0 12px;"/>
       <div class="row-fluid">
         <div class="span6">
          <div style="height:40px;line-height:40px;position:relative;top:10px;">Already registered? <a tabindex="-1" data-sbt={ Form.attrJson( Map( "href" -> ( wpath + "/in" ), "top" -> 1 ) ) }>Sign in here</a></div>
         </div> 
         <div class="span6" style="height:40px;padding-top:8px;"><button id='regBtn' disabled='disabled' type="submit" class="btn-success btn pull-right">Register <i class="icon-caret-right"></i></button></div>
       </div>
      </div>
     </fieldset>
    </form>
   </div>
        
    val jsonRes = web.jsonRes( sess )
    
    jsonRes.htmlMap = Map( 
        "html" -> <div class="container" style="background: rgb(64,64,65);background: rgba(64,64,65,0.4);margin: 0 auto;width: 740px;border-radius: 8px;padding: 16px;">{ inner }</div>,
        "transition" -> "fadeOutIn",
        "duration" -> 500 )
    
    jsonRes.extraJS = "T.callWhenHtmlDone( function() { T.initFormPlaceholders( '#f' ); }, 600 );" + ( doRecaptcha ?
                        ( "T.callWhen( function() { return window.Recaptcha !== undefined && window.showRecapcha !== null; }, function() {" + DbReCaptcha.callShowFunction + "}, 100 );" ) | "" )
      
    web.json( jsonRes )
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
      val user = B.User.db.findOne( Mobj( app.idName -> uid ) ) 

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

  def socialLogin( network:String ) = {
    val sess = T.session

    if ( !Social.appFor( network ).exchangeToken )
      T.web.redirect( T.website() )

    val user = findUser( network )

    if ( user == null ) {
      T.web.redirect( wpath + "/register" + network )
    } else if ( user.s( 'activationCode ).notBlank ) {
      notActivatedYet( user )
    } else {
      copySocialLogins( sess.user, user )
      sess.login( user )
      LoginCookie.set( user )
      T.web.redirect( T.website() )
    }
  }

  def socialRegister( network:String ) = {
    val t = T
    val web = t.web
    val sess = t.session
    val app = Social.appFor( network )

    val user =
      sess.user match {
        case null => B.newUser()
        case u    => if ( u.isNew ) u else B.newUser()
      }

    val uid = user.s( app.idName )
    if ( uid.isBlank )
      web.redirect( "/" )

    if ( web.req.s( 'create ).notBlank ) {
      val email = web.req.s( 'un )

      if ( !validateUnusedEmail( email ) )
        web.redirect( web.path )

      app.importUser( user, uid )

      user( 'email )     = email
      user( 'createdOn ) = new Date

      Register.sendActivation( user )

      user.save
      
      Social.ensureSecureThumbnail( user )

      web.redirect("/")

    } else if ( web.req.s( 'link ).notBlank ) {

      val existing = getUserByEmailPassword( web.req.s( 'un ), web.req.s( 'pw ) )

      if ( existing != null ) {
        copySocialLogins( sessionUser = user, existingUser = existing )
        sess.login( existing )
        LoginCookie.set(user)

        sess.notice( "Your " + B.applicationName + " and " + app.networkName + " accounts are now linked." )
        web.redirect( "/" )
      }
    }

    web.template(
      <tyr:shell>
       <div class="plainBox">
        <div class="title">Are You a New { B.applicationName } Member?</div>
        <div class="contents">
         <div>
          If you are not a current { B.applicationName } user, then please select this option.
          <p>Please enter in the email address that we should use with { B.applicationName }.</p>
         </div>
         <form method="post" action={ web.path } id="f">
          <table>
           <tr>
            <td style="width:75px;">
             <label for="un">Email:</label>
            </td>
            <td>
             <input type="text" id="un" name="un" style="width:240px;" value={ user.s( 'email ) }/>
             { Focus("#un") }
            </td>
           </tr>
          </table>
          <div class="btns" style="padding-top:16px;">
           <input name="create" type="submit" class="btn-success btn" value={ "Create a New " + B.applicationName + " Account" }/>
          </div>
         </form>
        </div>
       </div>
       <div class="plainBox">
        <div class="title">... Or Do You Already Have a { B.applicationName } Account?</div>
        <div class="contents">
         <div>
          If you already have an existing { B.applicationName } account, please log in with it below.
          <p>This will link your existing { B.applicationName } account with your { app.networkName } account.</p>
         </div>
         <form method="post" action={ web.path } id="f">
          <table>
           <tr>
            <td style="width:75px;">
             <label for="un">Email:</label>
            </td>
            <td>
             <input type="text" id="un" name="un" style="width:240px;" value={ user.s( 'email ) }/>
             { Focus("#un") }
            </td>
           </tr>
           <tr>
            <td>
             <label for="pw" style="padding-right:8px;">Password:</label>
            </td>
            <td>
             <input type="password" name="pw" style="width:240px;"/>
            </td>
           </tr>
          </table>
          <div class="btns" style="padding-top:16px;">
           <input name="link" type="submit" class="btn-success btn" value={ "Link your Existing " + B.applicationName + " Account to " + app.networkName }/>
          </div>
          {
            val otherNetworks = Social.networks.filter( !_.isActive )
            otherNetworks.nonEmpty |*
          <hr style="margin:24px 0 0; border-color:#ccc;"/> ++
          <div class="plainBox">
           <div class="title">If you used a different Social Network to log in with { B.applicationName } in the past, you can log in with it here.</div>
           <div class="contents">
            <div>{
             otherNetworks.flatMap { network =>
              <div>{ network.loginButton( this ) }</div>
             }
            }</div>
           </div>
          </div> }
         </form>
        </div>
       </div>
      </tyr:shell> )
  }

  def notActivatedYet( user:User ) = {
    T.session.error( 
        "This account has not been activated yet!  Please check your email for the activation link.",
        <a href={ "/log/resendActivation?id=" + user.id }>Send Again</a> )
        
    T.web.jsRes()
    throw new WebHandledException
  }
}

