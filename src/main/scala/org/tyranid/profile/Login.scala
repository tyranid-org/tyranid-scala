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
import org.tyranid.email.AWSEmail
import org.tyranid.logic.Invalid
import org.tyranid.math.Base62
import org.tyranid.session.Session
import org.tyranid.social.Social
import org.tyranid.ui.{ Button, Grid, Row, Focus, LnF, Form }
import org.tyranid.web.{ Weblet, WebContext, WebTemplate, WebResponse }


object Loginlet extends Weblet {

  def box = {
    val thread = T
    val user = thread.user
    val web = thread.web
    val loggingOut = web.req.s( 'lo ).notBlank
    val noSocial = web.b( 'nosocial )

    val params = noSocial |* "?nosocial=1"

    // TODO:  make this more template-based
     <head>
      <script>{ Unparsed( """
$( function() {
  $('#forgot').click(function(e) {
    window.location.assign( '""" + wpath + """/forgot?un=' + encodeURIComponent( $("#un").val() ) );
  });
});
""" ) }</script>
     </head> ++
     { T.LnF match {
       case LnF.RetailBrand =>
    <form method="post" action={ wpath + "/in" } id="f" class="login" style="margin-bottom:12px;" data-val="1" data-val-top="1">
     <fieldset class="loginBox">
      <div class="container-fluid" style="padding:0;">
       <div class="row-fluid">
        <h1 class="span5">Sign-in</h1>
        <div class="span7 pull-right regLink">or <a href="javascript:void(0);" data-sbt={ Form.attrJson( Map( "href" -> ( wpath + "/register" + params ) ) ) }>Register for { B.applicationName }!</a></div>
       </div>
      </div>
      <hr style="margin:4px 0 30px;"/>
      <div class="top-form-messages"/>
      <div class="container-fluid" style="padding:0;">
       <div class="row-fluid">
        <input type="text" id="un" name="un" placeholder="Email" value={ user.s('email) } data-val="req,email"/>
        { Focus("#un") }
       </div>
       <div class="row-fluid">
        <input type="password" name="pw" id="pw" placeholder="Password" data-val="req"/>
       </div>
       <div class="row-fluid">
         <div class="span6">
          <div style="height:40px;display:inline-block;"><input type="checkbox" name="save" id="saveLogin" value="Y"/></div>
          <label for="saveLogin" style="vertical-align:text-top;display:inline-block;">Stay signed-in</label>
         </div> 
         <div class="span6" style="height:40px;padding-top:8px;"><button type="submit" class="btn-success btn pull-right">Sign-In <i class="icon-play"></i></button></div>
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
              
        web.json( jsonRes )              
      } else {
        val email    = web.req.s( 'un )
        val password = web.req.s( 'pw )
        val redirect = web.req.s( 'l  )
  
        val user =
          if ( email.isBlank )
            LoginCookie.getUser.getOrElse(null)
          else
            getUserByEmailPassword( email, password )
  
        if ( user == null || user.s( "activationCode" ).notBlank) {
          if (user != null)
            notActivatedYet( user )
          else if ( email.isBlank ) 
            sess.warn( "Please log in." )
  
          // check look and feel, and if rb, do json, otherwise this.
          T.LnF match {
             case LnF.RetailBrand =>
               val jsonRes = web.jsonRes( sess )
               
               if ( !sess.hasErrors )
                 jsonRes.redirect = redirect.isBlank ? T.website | ( T.website + "/?l=" + redirect.encUrl )
                 
               web.json( jsonRes )
             case _ =>
               web.redirect( redirect.isBlank ? T.website | ( T.website + "/?l=" + redirect.encUrl ) )
          }
        } else {
          copySocialLogins( sessionUser = sess.user, existingUser = user )
          sess.login( user )
  
          sess.put( "remoteHost", web.req.getRemoteAddr() )
          sess.put( "remoteAddr", web.req.getRemoteAddr() )
          
          if ( web.b( 'save ) )
            LoginCookie.set(user)
  
          T.LnF match {
             case LnF.RetailBrand =>
               val jsonRes = web.jsonRes( sess )
               jsonRes.redirect = redirect.isBlank ? T.website | redirect
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
      web.redirect( website + "/?lo=1" )
    case s if s.startsWith( "/in" ) =>
      socialLogin( s.substring( 3 ) )

    case s if s.startsWith( "/register" ) =>
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
            B.User.db.exists( Mobj( "email" -> user.s( 'email ) ) ) |*
              Some( Invalid( scope.at( 'email ), "The \"" + user.s( 'email ) + "\" email address is already taken.") )
          } ) ::
          Nil

      val ui = user.view.ui( "register" )

      user.isAdding = true
      
      if ( web.b( 'saving ) || web.b( 'xhrSbt ) ) {
        val invalids = Scope( user, initialDraw = false, captcha = true ).submit( user, ui )

        if ( invalids.isEmpty ) {
          user( 'createdOn ) = new Date

          sendActivation( user )
          
          val entryAppVal = sess.get( "entryApp" )
          
          if ( entryAppVal != null )
            user( "entryApp" ) = entryAppVal._i
            
          user.save
          web.redirect( "/" )
        }
      }

      val entryApp = web.i( 'app ) or 0
      sess.put( "entryApp", new java.lang.Integer( entryApp._i ) )

      T.LnF match {
        case LnF.RetailBrand =>
          
          //if ( !invalids.is)
          val inner =
   <div class="offset3 span6" style="margin-top:100px;text-align:center;">
    <img src="/volerro_logo.png" style="width:197px;height:60px;"/>
   </div> ++
   <div class="offset2 span8">
    <form method="post" action={ wpath + "/register" } id="f" class="register" style="margin-bottom:12px;" data-val="1">
     <fieldset class="registerBox">
      <div class="container-fluid" style="padding:0;">
       <div class="row-fluid">
        <h1 class="span5">Register</h1>
       </div>
      </div>
      <hr style="margin:4px 0 30px;"/>
      <div class="top-form-messages"/>
      <div class="container-fluid" style="padding:0;">
       <div class="row-fluid">
         <div class="container-fluid span12" style="padding:0;">
          <div class="row-fluid">
           <div class="span3"><input type="text" id="firstName" name="firstName" placeholder="First Name" data-val="req" data-val-with="lastName"/></div>
           <div class="span3"><input type="text" id="lastName" name="lastName" placeholder="Last Name" data-val="req" data-val-with="firstName"/></div>
           { Focus("#firstName") }
           <div class="span6 val-display"/>
           <div class="span6 hints" style="position:relative;">
            <div>
            Hint: Use your company or organization email address to easier connect with co-workers.
            </div> 
           </div>  
          </div>
          <div class="row-fluid">
           <div class="span6">
            <input type="text" name="email" id="email" value={ user.s( 'email ) } placeholder="Email address" data-val="req,email"/>
           </div>
           <div class="span6 val-display"/>
          </div>
          <div class="row-fluid">
           <div class="span6">
            <input type="password" name="password" id="password" placeholder="Password" data-val="req"/>
           </div>
           <div class="span6 val-display"/> 
          </div>
          <div class="row-fluid">
           <div class="span6">
            <input type="password" name="password2" id="password2" placeholder="Re-type password" data-val="req,same=password"/>
           </div>
           <div class="span6 val-display"/>
          </div>
         </div>
       </div>
       <hr style="margin:20px 0 12px;"/>
       <div class="row-fluid">
         <div class="span6">
          <div style="height:40px;line-height:40px;position:relative;top:10px;">Already registered? <a href="javascript:void(0);" data-sbt={ Form.attrJson( Map( "href" -> ( wpath + "/in" ) ) ) }>Sign in here</a></div>
         </div> 
         <div class="span6" style="height:40px;padding-top:8px;"><button type="submit" class="btn-success btn pull-right">Register <i class="icon-play"></i></button></div>
       </div>
      </div>
     </fieldset>
    </form>
   </div>
          val jsonRes = web.jsonRes( sess )
          jsonRes.htmlMap = Map( 
              "html" -> inner,
              "transition" -> "fadeOutIn",
              "duration" -> 500 )
              
          web.json( jsonRes )
        case _ =>
          val inner = 
         { ( entryApp == 0 ) |* <div style="margin-top:16px; font-size:24px;">Creating an account with Volerro is Free!</div> } ++
         { !noSocial |*
         <div class="plainBox">
          <div class="title">Use Social Login to Automatically Register</div>
          <div class="contents">
           Sign in using a Social Network to quickly create an account automatically:
           <div>{
            Social.networks.flatMap { network =>
             <hr style="margin:4px 0 8px;"/> ++
             network.loginButton( this ) }
           }</div>
          </div>
         </div> } ++
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

          web.template( ( entryApp == 0 ) ? <tyr:shell>{ inner }</tyr:shell> | <tyr:shellApp>{ inner }</tyr:shellApp> )
      }

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
      val forgotCode = web.req.s("a")

      if ( forgotCode.isBlank ) {
        val email = web.req.s( "un" ) or sess.user.s( 'email )

        if ( email.isBlank ) {
          sess.warn( "Please enter in an email address." )
          web.redirect("/")
        }

        val dbUser = B.User.db.findOne( Mobj( "email" -> email ) )

        if (dbUser == null) {
          sess.warn("Sorry, it doesn't look like the email address " + email + " is on " + B.applicationName + ".")
          web.redirect("/")
        }
        
        val user = B.User(dbUser)
        val resetCode = Base62.make(8)
        user('resetCode) = resetCode
        user.save

        sess.notice( "Instructions for changing your password have been sent to " + email + "." )

        // TODO:  move this to email templates
        AWSEmail(subject = B.applicationName + " account access", text = """
Hello """ + dbUser.s( 'firstName ) + """,

Forgot your password? No problem. You can access your """ + B.applicationName + """ account with the link below:

""" + T.website + """/user/forgot?a=""" + resetCode + """

Once you have accessed your account, please update it with a new password.

Thank you!

The """ + B.applicationName + """ Team
""" + T.website )
          .addTo( user s 'email )
          .from( "volerro@" + B.domain )
          .replyTo( "volerro@" + B.domain )
          .send

        web.redirect("/")
      } else {
        val dbUser = B.User.db.findOne(Mobj("resetCode" -> forgotCode))

        if (dbUser == null) {
          web.template(
            <tyr:shell>
              <p>Account access code not found!</p>
            </tyr:shell>)
        } else {
          val user = B.User(dbUser)
          user.remove('resetCode)
          sess.login( user )
          user.save
          sess.notice( "You can now change your password." )
          web.redirect( "/user/edit?id=" + user.tid )
        }
      }
      
    case "/null" =>
      log( Event.RefInt, "m" -> ( "null, Referer: " + web.req.getHeader( "referer" ) ) ) 
    }
  }

  def sendActivation( user:User ) = {
    val activationCode = Base62.make(8)
    user('activationCode) = activationCode

    T.session.notice( "Thank you!  You should be receiving an email shortly to verify your account." )

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

      sendActivation( user )

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
    T.session.warn( 
        "This account has not been activated yet!  Please check your email for the activation link.",
        <a href={ "/log/resendActivation?id=" + user.id }>Send Again</a> )
    T.web.redirect( "/?na=1" ) // na = need activation
  }
}

