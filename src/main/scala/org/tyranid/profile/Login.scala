/**
 * Copyright (c) 2008-2012 Tyranid <http://tyranid.org>
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
import org.tyranid.db.mongo.Imp._
import org.tyranid.email.Email
import org.tyranid.json.Js
import org.tyranid.logic.Invalid
import org.tyranid.math.Base62
import org.tyranid.secure.DbReCaptcha
import org.tyranid.session.Session
import org.tyranid.social.Social
import org.tyranid.ui.{ Button, Grid, Row, Focus, LnF, Form }
import org.tyranid.web.{ Weblet, WebContext, WebTemplate, WebResponse }
import org.tyranid.web.WebHandledException

/*
     new Form( "/user/register", "register" )
       .style( "margin-bottom:12px;" )
       .data( "val", "1" )
       .fieldsetClass( "registerBox" )
       .title( "Thanks, " + user.s( 'firstName ) + "!" )
       .hidden( "regStep", "2" )
 */

object Register {
  def sendActivation( user:User ) = {
    val lnf = T.LnF
    val activationCode = Base62.make(8)
    
    user( 'activationCode ) = activationCode
    user( 'lnf ) = lnf.id

    if ( T.LnF == LnF.SupplyChain ) 
      T.session.notice( "Thank you!  You should be receiving an email shortly to verify your account." )        
    
    background { B.emailTemplates.welcome( user, activationCode ) }
  }

  def page( user:User, org:Org, jsonRes:WebResponse ) = {
    val inner:NodeSeq =
      if ( org != null ) {
        // if the org domain is the same as MY domain then add them.
        if ( org.s( 'domain ).toLowerCase == Email.domainFor( user.s( 'email ) ).toLowerCase ) {
          user.join( org )
        } else {
          // Must be approved, so send a join request AFTER they activate the account.
        }
        
        sendActivation( user )
        user.save
        
        <div class="container-fluid" style="padding:0;padding-top:1em;">
         <div class="row-fluid">
          <div class="span12">A verification email was sent to <b>{ user.s( 'email ) }</b>.</div>
          <div>Click the activation link in your email to gain access to { org.s( 'name ).possessive } network and projects.</div>
         </div>
         <hr/>
         <div class="row-fluid">
          <!--div class="span12" style="height:40px;padding-top:8px;text-align:right;">Or activate later and <button type="submit" class="btn-success btn">Continue To Volerro <i class="icon-caret-right"></i></button></div-->
         </div>
        </div>
      } else {
        //jsonRes.extraJS = "$(function(){ tyr.callWhenHtmlDone( function() { tyr.autocomplete( 'companyName', '/log/company' ); $('#companyName').focus(); }, 1000 ) } );"
        
        <div class="container-fluid" style="padding:0;">
         <input type="hidden" name="regStep" value="2"/>
         { T.web.b( 'keep ) |* <input type="hidden" name="keep" value="1"/> }
         <div class="row-fluid">
          <div style="padding-top:1em;">One last step.  Please enter the name of <b style='color:#cc4418;'>your</b> company below.  This helps other identify you within Volerro.</div>
         </div>
         <hr/>
         <div class="row-fluid">
          <div class="span6"><input type="text" id="companyName" name="companyName" placeholder="Your Company Name" data-update="blur" data-val="req"/></div>
          { Focus( "#companyName" ) }
          <div class="span6 hints">
           <div class="fldHint">
            Hint: Use your own name (ex: BobSmith Co) if you are not in a company.
           </div>
          </div> 
          <div class="span6 val-display"/>
         </div> 
         <div class="row-fluid">
          <div class="span12" style="height:40px;padding-top:8px;text-align:right;"><button type="submit" class="btn-success btn">Next <i class="icon-caret-right"></i></button></div>
         </div>
        </div>
      }       
        
    <div class="container">
     <div class="offset3 span6" style="margin-top:100px;text-align:center;">
      <a href="/"><img src="/volerro_logo.png"/></a>
     </div>
     <div class="offset2 span8">
      <form method="post" action="/user/register" id="f" class="register" style="margin-bottom:12px;" data-val="1">
       <fieldset class="registerBox">
        <div class="top-form-messages"/>
        <div class="container-fluid" style="padding:0;">
         <div class="row-fluid">
          <h1 class="span12">Thanks, { user.s( 'firstName ) }!</h1>
         </div>
        </div>
        { inner }
       </fieldset>
      </form>
     </div>
    </div>
  }
  
  def beta( user:User, jsonRes:WebResponse ) = {
    <div class="container">
     <div class="offset3 span6" style="margin-top:100px;text-align:center;">
      <a href="/"><img src="/volerro_logo.png"/></a>
     </div>
     <div class="offset2 span8">
      <form method="post" action="/user/register" id="f" class="register" style="margin-bottom:12px;" data-val="1">
       <fieldset class="registerBox">
        <div class="top-form-messages"/>
        <div class="container-fluid" style="padding:0;">
         <div class="row-fluid">
          <h1 class="span12">Thanks, { user.s( 'firstName ) }!</h1>
         </div>
        </div>
        <div class="container-fluid" style="padding:0;padding-top:1em;">
         <div class="row-fluid">
          <div class="span12">We will be sending you an invitation to our private beta very soon!</div>
         </div>
        </div>
       </fieldset>
      </form>
     </div>
    </div>
  }
}

object Loginlet extends Weblet {
 def box = {
    val thread = T
    val user = thread.user
    val web = thread.web
    val loggingOut = web.req.s( 'lo ).notBlank
    val noSocial = web.b( 'nosocial )

    val params = noSocial |* "?nosocial=1"

    // TODO:  make this more template-based
     { T.LnF match {
       case LnF.RetailBrand =>
    <script>{ Unparsed( """
$( function() {
  $('#forgot').click(function(e) {
    var un = $( "#un" );
    var msg = "";
    var fldVal = un.val();
        
    if ( !fldVal ) {
      msg = "Please enter an email address";
    } else if ( !tyr.Validator.validEmail( fldVal ) ) {
      msg = "Please enter a valid email address";
    }
       
    var formHandler = tyr.FormHandler.get( un.get() );
        
    if ( msg ) {
      formHandler.topEl().empty();
      formHandler.addTopMessage( "error", msg );
      return false;
    }

    formHandler.topEl().empty();
    tyr.navTo( '""" + wpath + """/forgot?xhr=1&un=' + encodeURIComponent( fldVal ) );
  });
    
  tyr.initFormPlaceholders( "f" );
});
""" ) }
    </script> ++
    <form method="post" action={ wpath + "/in" } id="f" class="login" style="margin-bottom:12px;" data-val="1" data-val-top="1">
     { /*web.s( 'l ).notBlank |* <input name="l" value={ web.s( 'l ) }/> */ }
     <fieldset class="loginBox">
      <div class="container-fluid" style="padding:0;">
       <div class="row-fluid">
        <h1 class="span5">Sign-in</h1>
        <div class="span7 pull-right regLink">or <a data-sbt={ Form.attrJson( Map( "href" -> ( wpath + "/register" + params ), "top" -> 1 ) ) }>Register for { B.applicationName }!</a></div>
       </div>
      </div>
      <hr style="margin:4px 0 30px;"/>
      <div class="top-form-messages"/>
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
     <input type="hidden" name="l" value={ web.req.s("l") }/>
    </form> ++
    <div class="container-fluid" style="padding:0;">
     <a href="#" id="forgot" class="pull-right">Forgot your password?</a>
    </div>
 case _ =>
    <script>{ Unparsed( """
$( function() {
  $('#forgot').click(function(e) {
    window.location.assign( '""" + wpath + """/forgot?un=' + encodeURIComponent( fldVal ) );
  });
});
""" ) }
    </script> ++
    <form method="post" action={ wpath + "/in" } id="f" class="form-horizontal">
     <fieldset class="loginBox">
      <legend><span>Log In</span></legend>
      <div class="control-group" style="padding: 0px 8px;">
       <label class="control-label" for="un">Email:</label>
       <div class="controls">
        <input type="text" id="un" name="un" value={ user.s('email) }/>
        { Focus("#un") }
       </div>
       <label class="control-label" for="pw">Password:</label>
       <div class="controls">
        <input type="password" name="pw"/>
       </div>
       <div class="row-fluid">
         <label class="checkbox span7">
          <input type="checkbox" name="save" id="saveLogin" value="Y"/> Stay Logged In
         </label>
         <div class="span5"><input type="submit" value="Login" class="btn-success btn pull-right"/></div>
       </div>
       { !noSocial && web.req.s( 'na ).isBlank |*
           Social.networks.flatMap { network =>
             <hr style="margin:4px 0 8px;"/> ++
             network.loginButton( this ) }
       }
      </div>
     </fieldset>
     <input type="hidden" name="l" value={ web.req.s("l") }/>
     <div class="container-fluid" style="padding:0;margin-top:4px;">
      <a href={ wpath + "/register" + params } class="pull-left">Join { B.applicationName }!</a>
      <a href="#" id="forgot" class="pull-right">Forgot password ?</a>
     </div>
    </form>
     } }
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

        jsonRes.extraJS = "tyr.initFormPlaceholders( 'f' );"
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
  
           // check look and feel, and if rb, do json, otherwise this.
          T.LnF match {
             case LnF.RetailBrand =>
               val jsonRes = web.jsonRes( sess )
               
               if ( !sess.hasErrors )
                 jsonRes.redirect = redirect.isBlank ? T.website | ( T.website + "/?l=" + redirect.encUrl )
                 
               jsonRes.extraJS = "tyr.initFormPlaceholders( 'f' );"
               web.json( jsonRes )
             case _ =>
               web.redirect( redirect.isBlank ? T.website | ( T.website + "/?l=" + redirect.encUrl ) )
          }
        } else {
          copySocialLogins( sessionUser = sess.user, existingUser = user )
          sess.login( user )
  
          sess.put( "remoteHost", web.req.getRemoteHost() )
          sess.put( "remoteAddr", web.req.getRemoteAddr() )
          
          if ( web.b( 'save ) )
            LoginCookie.set(user)
  
          T.LnF match {
             case LnF.RetailBrand =>
               val jsonRes = web.jsonRes( sess )
               jsonRes.extraJS = "tyr.app.loadMenubar( '/user/menubar' ); tyr.app.loadMain( '" + ( redirect.isBlank ? "/dashboard" | redirect ) + "' );"
               web.json( jsonRes )
             case _ =>
               web.redirect(redirect.isBlank ? T.website | redirect)
          }
        }
      }
    case "/clear" =>
      sess.clearAllEditing
      web.html( NodeSeq.Empty )
    case "/out" =>
      val website = T.website
      sess.logout
      web.redirect( website + "/?lo=1" + ( web.b( 'xhr ) ? "&xhr=1" | "" ) )
    case s if s.startsWith( "/in" ) =>
      socialLogin( s.substring( 3 ) )
    case "/register2" =>
      registerRetail( web, sess )
    case s if s.startsWith( "/register" ) =>
      if ( T.LnF == LnF.RetailBrand ) {
        registerRetail( web, sess )
        return
      }

      val network = s.substring( 9 )

      if ( network.notBlank )
        return socialRegister( network )

      val user =
        sess.user match {
        case null => B.newUser()
        case u    => if ( u.isNew ) u else B.newUser()
        }

      sess.user = user

      user.extraVaValidations =
        ( user.view( 'email ),
          { scope:Scope =>
            B.User.db.exists( Mobj( "email" -> ("^" + user.s( 'email ).encRegex + "$").toPatternI ) ) |*
              Some( Invalid( scope.at( 'email ), "'" + user.s( 'email ) + "' email address is already taken.") )
          } ) ::
          Nil

      val ui = user.view.ui( "register" )

      user.isAdding = true
      
      if ( web.b( 'saving ) ) {
        val invalids = Scope( user, initialDraw = false, captcha = true ).submit( user, ui )

        if ( invalids.isEmpty ) {
          user( 'createdOn ) = new Date

          Register.sendActivation( user )
          
          val entryAppVal = sess.get( "entryApp" )
          
          if ( entryAppVal != null )
            user( "entryApp" ) = entryAppVal._i
            
          user.save
          
          web.redirect( "/" )
        }
      }

      val entryApp = web.i( 'app ) or 0
      sess.put( "entryApp", new java.lang.Integer( entryApp._i ) )

      val inner = 
       <div class="container">
        { ( entryApp == 0 ) |* <div style="margin-top:16px; font-size:24px;">Creating an account with Volerro is Free!</div> }
        { !noSocial |*
         <div class="plainBox">
          <div class="title">Use Social Login to Automatically Register</div>
          <div class="contents">
           Sign in using a Social Network to quickly create an account automatically:
           <div>{
            Social.networks.flatMap { network =>
             <hr style="margin:4px 0 8px;"/>++ 
             network.loginButton( this ) }
           }</div>
          </div>
         </div> }
         <div class="plainBox">
          <div class="title">{ noSocial ? "Register" | "Manually Register" }</div>
          <div class="contents" style="height:390px;">
           <form method="post" action={ web.path } id="f" style="float:left">
            <table>
              { Scope(user, saving = true, captcha = true).draw(ui) }
            </table>
            <div class="btns">
              <input type="submit" class="btn-success btn" value="Save &amp; Register" name="saving"/>
              <a href="/" class="btn">Cancel</a>
            </div>
           </form>
           <div style="float:right; padding-top: 10px;">
           { Unparsed( """
            <video id="vid_sign_up" class="video-js vjs-default-skin" controls preload="auto" width="643" height="276" data-setup="{}">
             <source src="https://d33lorp9dhlilu.cloudfront.net/videos/Volerro_Sign_Up.mp4" type="video/mp4"/>
            </video>
            """ ) }
            <div class="title">Volerro Sign Up</div>
           </div>
          </div>
         </div>
        </div>
           
      web.template( ( entryApp == 0 ) ? <tyr:shell>{ inner }</tyr:shell> | <tyr:shellApp>{ inner }</tyr:shellApp> )    
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
          
          if ( T.LnF == LnF.RetailBrand )
            return web.json( web.jsonRes( sess ) )
            
          web.redirect( "/" )
        }

        val dbUser = B.User.db.findOne( Mobj( "email" -> email ) )

        if ( dbUser == null ) {
          sess.warn( "Sorry, it doesn't look like the email address " + email + " is on " + B.applicationName + "." )
          
          if ( T.LnF == LnF.RetailBrand )
            return web.json( web.jsonRes( sess ) )
            
          web.redirect("/")
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
        
        if ( T.LnF == LnF.RetailBrand )
          return web.json( web.jsonRes( sess ) )
            
        web.redirect("/")
      } else {
        val dbUser = B.User.db.findOne(Mobj("resetCode" -> forgotCode))

        if (dbUser == null) {
          if ( T.LnF == LnF.RetailBrand ) {
          sess.notice( "Account access code not found!", deferred = "/dashboard" )
          web.jsRes( Js( "tyr.app.loadMenubar( '/user/menubar' ); tyr.app.loadMain( '/dashboard' )" ) )
          } else {
            web.template(
            <tyr:shell>
              <p>Account access code not found!</p>
            </tyr:shell>)
          }
        } else {
          val user = B.User(dbUser)
          user.remove('resetCode)
          sess.login( user )
          user.save
          
          if ( T.LnF == LnF.RetailBrand ) {
            sess.notice( "You can now change your password in <em>My Profile</em>.", deferred = "/dashboard" )
            web.jsRes( Js( "tyr.app.loadMenubar( '/user/menubar' ); tyr.app.loadMain( '/dashboard' )" ) )
          } else {
            sess.notice( "You can now change your password." )
            web.redirect( "/user/edit?id=" + user.tid )
          }
        }
      }
      
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
            web.jsRes( org.tyranid.json.Js( """ 
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
        case _ => 
           web.jsRes()
      }
      
      return
    }
    
    val betaSignup = web.b( 'beta )
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

    val registerView = "register" + ( betaSignup ? "beta" | ( "rb" + ( B.BETA ? "beta" | "" ) ) )
    
    val ui = user.view.ui( registerView )

    user.isAdding = true
    
    if ( web.b( 'xhrSbt ) ) {
      val jsonRes = web.jsonRes( sess )
      val invalids = Scope( user, initialDraw = false, captcha = !betaSignup ).submit( user, ui )

      if ( invalids.isEmpty ) {
        user( 'createdOn ) = new Date

        val entryAppVal = sess.get( "entryApp" )
        
        if ( entryAppVal != null )
          user( "entryApp" ) = entryAppVal._i
          
        //user.save
        
        val email = user.s( 'email )
        
        if ( !betaSignup ) {
          val org = Email.isWellKnownProvider( email ) ? null | {
            val emailDomain = Email.domainFor( email )
            B.Org( B.Org.db.findOne( Mobj( "domain" -> emailDomain.toPatternI ), Mobj( "name" -> 1, "domain" -> 1 ) ) )
          }
          
          if ( org != null )
            user.save
            
          if ( keep && org != null ) {
            user.join( org )
            sess.login( user )
            user.remove( 'activationCode )
            user.save
            
            B.welcomeUserEvent
            val jsonRes = web.jsonRes( sess )
            jsonRes.extraJS = "tyr.app.loadMenubar( '/user/menubar' ); tyr.app.loadMain( '/dashboard' );"
            web.json( jsonRes )
            Group.ensureInOrgGroup( user )
            
            return
          } else {
            jsonRes.htmlMap = Map( 
                "html" -> WebTemplate( Register.page( user, org, jsonRes ) ),
                "transition" -> "slideLeft",
                "duration" -> 500 )
          }
        } else {
          ContactInfo.ensure( user.s( 'email ), user.fullName, beta = true )
          
          jsonRes.htmlMap = Map( 
              "html" -> WebTemplate( Register.beta( user, jsonRes ) ),
              "transition" -> "slideLeft",
              "duration" -> 500 )
        }
      } else {
        for ( i <- invalids )
          sess.error( i.message )
          
        if ( B.requireReCaptcha && !betaSignup )
          jsonRes.extraJS = "Recaptcha.reload();"
      }
      
      jsonRes.extraJS = "tyr.initFormPlaceholders( 'f' );"      
      web.json( jsonRes )
      return
    }

    val entryApp = web.i( 'app ) or 0
    sess.put( "entryApp", new java.lang.Integer( entryApp._i ) )

    val doRecaptcha = B.requireReCaptcha && !betaSignup
    
    val inner =
   <div class="offset3 span6" style={ "margin-top:" + ( doRecaptcha ? ( B.BETA ? "25" | "50" )  | "100" ) + "px;text-align:center;" }>
    <a href="/"><img src="/volerro_logo.png"/></a>
   </div> ++
   <div class={ "offset2 span8" + ( betaSignup |* " beta" ) }>
    <form method="post" action={ wpath + "/register2" } id="f" class="register" style="margin-bottom:12px;" data-val="1">
     { keep |* <input type="hidden" name="keep" value="1"/> }
     { betaSignup |* <input type="hidden" name="beta" value="1"/> }
     <fieldset class="registerBox">
      <div class="container-fluid" style="padding:0;">
       <div class="row-fluid">
        <h1 class="span12">{ betaSignup ? "Join our private beta!" | ( "Register" + ( B.BETA ? " for Beta access." | "" ) ) }</h1>
       </div>
      </div>
      <hr style="margin:4px 0 30px;"/>
      <div class="top-form-messages"/>
      <div class="container-fluid" style="padding:0;">
       <div class="row-fluid">
         <div class="container-fluid span12" style="padding:0;">
          { !keep && ( B.BETA && !betaSignup ) |*
            { user.s( 'activationCode ).isBlank ?
          <div class="row-fluid">
           <div class="span6">
            <input type="text" name="activationCode" id="activationCode" value={ user.s( 'activationCode ) } placeholder="Invite Code" data-update="blur" data-val="req"/>
           </div>
           <div class="span6 val-display"/>
          </div> |
          <input type="hidden" name="activationCode" id="activationCode" value={ user.s( 'activationCode ) }/>
            }
          }
          { ( !keep && ( B.BETA && !betaSignup ) ) ?
          <div class="row-fluid">
           <div class="span3"><input type="text" id="firstName" name="firstName" readonly="readonly" placeholder="First Name" value={ user.s( 'firstName ) }/></div>
           <div class="span3"><input type="text" id="lastName" name="lastName" readonly="readonly" placeholder="Last Name" value={ user.s( 'lastName ) }/></div>
           { Focus("#activationCode") }
           <div class="span6 val-display"/>
          </div> |
          <div class="row-fluid">
           <div class="span3"><input type="text" id="firstName" name="firstName" value={ user.s( 'firstName ) } placeholder="First Name" data-val="req" data-val-with="lastName"/></div>
           <div class="span3"><input type="text" id="lastName" name="lastName" value={ user.s( 'lastName ) } placeholder="Last Name" data-val="req" data-val-with="firstName"/></div>
           { Focus("#firstName") }
           <div class="span6 val-display"/>
           <div class="span6 hints" style="position:relative;">
            <div>Hint: <b>This will be the email address we send your account activation link to</b>.  Use your company or organization email address to easier connect with co-workers.</div> 
           </div>  
          </div>
          }
          <div class="row-fluid">
           <div class="span6">
            { ( ( B.BETA && !betaSignup ) || !user.isNew ) ?
              <input type="text" name="email" id="email" value={ user.s( 'email ) } readonly="readonly" placeholder="Email address"/> |
              <input type="text" name="email" id="email" value={ user.s( 'email ) } placeholder="Email address" data-update="blur" data-update-url={ wpath + "/register" } data-val="req,email"/>
            }
           </div>
           <div class="span6 val-display"/>
          </div>
          { !betaSignup |*
        <div class="row-fluid">
           <div class="span6">
            <input type="password" name="password" id="password" placeholder="Password" data-val="req,min=7"/>
           </div>
           <div class="span6 val-display"/> 
          </div>
        <div class="row-fluid">
           <div class="span6">
            <input type="hidden" name="keep" value={ web.s( 'keep ) }/>
            <input type="password" name="password2" id="password2" placeholder="Re-type password" data-val="req,same=password,min=7"/>
           </div>
           <div class="span6 val-display"/>
          </div>
          }
          { doRecaptcha |*
        <div class="row-fluid">
           <div class="span6">
            <script>{ Unparsed( "jQuery.getScript( \"" + DbReCaptcha.scriptSrc + "\" );" + DbReCaptcha.showFunction( "white" ) ) }</script>
            { DbReCaptcha.div }
           </div>
           <div class="span6 val-display"/> 
          </div>
        }
          <div class="row-fluid">
           <div class="span6">
            <input type="checkbox" name="agreement" id="agreement" style="height:20px;width:20px;" onChange="$( '#regBtn' ).attr( 'disabled', $( this ).is(':checked' ) ? false : true );"/> <label style="display:inline;padding-left:0;vertical-align:text-bottom;" class="extra" for="agreement">I agree to Volerro <a href="http://www.volerro.com/site/policies.html" style="text-decoration:none;border-bottom:1px dotted;" target="_terms">terms of use</a> policy.</label>
           </div>
          </div>
         </div>
       </div>
       <hr style="margin:10px 0 12px;"/>
       <div class="row-fluid">
         { !betaSignup |* 
         <div class="span6">
          <div style="height:40px;line-height:40px;position:relative;top:10px;">Already registered? <a tabindex="-1" data-sbt={ Form.attrJson( Map( "href" -> ( wpath + "/in" ), "top" -> 1 ) ) }>Sign in here</a></div>
         </div> 
         } 
         <div class={ "span6" + ( betaSignup |* " offset6" ) } style="height:40px;padding-top:8px;"><button id='regBtn' disabled='disabled' type="submit" class="btn-success btn pull-right">{ betaSignup ? "Sign-up" | "Register" } <i class="icon-caret-right"></i></button></div>
       </div>
      </div>
     </fieldset>
    </form>
   </div>
        
    val jsonRes = web.jsonRes( sess )
    
    jsonRes.htmlMap = Map( 
        "html" -> <div class="container">{ inner }</div>,
        "transition" -> "fadeOutIn",
        "duration" -> 500 )
    
    jsonRes.extraJS = "tyr.callWhenHtmlDone( function() { tyr.initFormPlaceholders( 'f' ); }, 600 );" + ( doRecaptcha ?
                        ( "tyr.callWhen( function() { return window.Recaptcha !== undefined && window.showRecapcha !== null; }, function() {" + DbReCaptcha.callShowFunction + "}, 100 );" ) | "" )
      
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
  
      val users = B.User.db.find( Mobj( "email" -> ("^" + email.encRegex + "$").toPatternI ) ).toSeq
      
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
      T.web.redirect( T.website )

    val user = findUser( network )

    if ( user == null ) {
      T.web.redirect( wpath + "/register" + network )
    } else if ( user.s( 'activationCode ).notBlank ) {
      notActivatedYet( user )
    } else {
      copySocialLogins( sess.user, user )
      sess.login( user )
      LoginCookie.set( user )
      T.web.redirect( T.website )
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
        
    if ( T.LnF == LnF.RetailBrand ) {
      T.web.jsRes()
      throw new WebHandledException
    } else
      T.web.redirect( "/?na=1" ) // na = need activation
  }
}

