
package org.tyranid.profile

import java.util.Date

import scala.xml.Unparsed

import org.tyranid.Imp._
import org.tyranid.db.Scope
import org.tyranid.db.mongo.Imp._
import org.tyranid.email.Email
import org.tyranid.logic.Invalid
import org.tyranid.math.Base62
import org.tyranid.social.Social
import org.tyranid.ui.{ Button, Grid, Row, Field, Focus }
import org.tyranid.web.{ Weblet, WebContext, WebTemplate }


object Loginlet extends Weblet {

  def box = {
    val thread = T
    val user = thread.user
    val web = thread.web
    val loggingOut = web.req.s( 'lo ).notBlank

    // TODO:  make this more template-based
    <form method="post" action={ wpath + "/in" } id="f">
     <div class="loginBox">
      <div class="title">
       Log In
      </div>
      <div class="contents">
       <table>
        <tr>
         <td>
          <label for="un">Email:</label>
         </td>
         <td>
          <input type="text" id="un" name="un" style="width:180px;" value={ user.s('email) }/>
          { Focus("#un") }
         </td>
        </tr>
        <tr>
         <td>
          <label for="pw" style="padding-right:8px;">Password:</label>
         </td>
         <td>
          <input type="password" name="pw" style="width:180px;"/>
         </td>
        </tr>
        <tr>
         <td colspan="2" style="padding-top:8px;">
          <input type="checkbox" name="save" value="Y"/>
          <label for="save" style="line-height:18px;">Stay Logged In</label>
          <input type="hidden" name="l" value={ web.req.s("l") }/>
          <input type="submit" value="Login" class="greenBtn" style="padding:2px 16px; display:inline-block; float:right;"/>
         </td>
        </tr>
       </table>
       { web.req.s( 'na ).isBlank |*
         <hr style="margin-top:12px;"/> ++
         { B.linkedIn != null |* <div>{ B.linkedIn.loginButton( this ) }</div> } ++
         { B.linkedIn != null && B.facebook != null |* <hr style="margin:4px 0 8px;"/> } ++
         { B.facebook != null |* <div>{ B.facebook.loginButton( this ) }</div> }
       }
      </div>
     </div>
     <a href={ wpath + "/register" } style="float:left; margin-top:4px;">Join { B.applicationName }!</a>
     <a href="#" id="forgot" style="float:right; margin-top:4px;">Forgot password ?</a>
    </form>
  }

  def handle(web: WebContext) {
    val sess = T.session

    rpath match {
    case "/in" | "/" =>
      val email    = web.req.s( 'un )
      val password = web.req.s( 'pw )
      val redirect = web.req.s( 'l  )

      val user =
        if ( email.isBlank )
          LoginCookie.getUser.getOrElse(null)
        else
          getUserByEmailPassword( email, password )

      if (user == null || user.s("activationCode").notBlank) {
        if (user != null)
          notActivatedYet
        else if ( email.isBlank )
          sess.warn( "Please log in." )

        web.redirect(redirect.isBlank ? "/" | ("/?l=" + redirect.encUrl))
      } else {
        sess.login(user)

        if ( web.req.b( 'save ) )
          LoginCookie.set(user)

        web.redirect(redirect.isBlank ? "/" | redirect)
      }

    case "/out" =>
      sess.logout
      web.redirect( "/?lo=1" )

    case "/facebook" =>
      socialLogin( "fb" )

    case "/linkedin" =>
      socialLogin( "li" )

    case "/registerfb" =>
      socialRegister( "fb" )

    case "/registerli" =>
      socialRegister( "li" )

    case "/register" =>

      val user =
        sess.user match {
        case null => B.newUser()
        case u    => if ( u.isNew ) u else B.newUser()
        }

      sess.user = user

      user.extraVaValidations =
        (user.view('email),
          (scope: Scope) => {
            B.User.db.exists(Mobj("email" -> user.s('email))) |*
              Some(Invalid(scope.at('email), "The \"" + user.s( 'email ) + "\" email address is already taken."))
          }) ::
          Nil

      val ui = user.view.ui( "register" )

      user.isAdding = true
      
      if ( web.req.b( 'saving ) ) {

        val invalids = Scope(user, initialDraw = false, captcha = true).submit( user, ui )

        if (invalids.isEmpty) {
          user('createdOn) = new Date
          user('thumbnail) = "/icon_individual.png"

          sendActivation( user )
          user.save
          web.redirect("/")
        }
      }

      web.template(
        <tyr:shell>
          <div class="fieldhc">Create a New Account with { B.applicationName }</div>
          <form method="post" action={ web.path } id="f">
            <table>
              { Scope(user, saving = true, captcha = true).draw(ui) }
            </table>
            <div class="btns">
              <input type="submit" class="greenBtn" value="Save &amp; Register" name="saving"/>
              <a href="/" class="greyBtn">Cancel</a>
            </div>
          </form>
        </tyr:shell> )

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
        Email(subject = B.applicationName + " account access", text = """
Hello """ + dbUser.s( 'firstName ) + """,

You can access your """ + B.applicationName + """ account with the link below:

""" + B.website + """/user/forgot?a=""" + resetCode + """

Once you have access to your account, please update it with a new password.

Thank you,

The """ + B.applicationName + """ Team
""")
          .addTo(user s 'email)
          .from(B.systemEmail)
          .replyTo("no-reply@" + B.domain)
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

    if ( !validateEmail( email ) )
      return null

    if ( pw.isBlank ) {
      sess.warn( "Please enter in a password." )
      return null
    }

    val user = B.User( B.User.db.findOne(Mobj("email" -> ("^" + email.encRegex + "$").toPatternI, "password" -> pw ) ) )
    if ( user == null )
      sess.warn( "Invalid login.  Please try again." )

    user
  }

  def socialLogin( network:String ) = {
    val app = Social.appFor( network )

    if ( app.exchangeToken ) {
      val sess = T.session
      val socialIdName = network + "id"
      val socialId = sess.user.s( socialIdName )
      val user = B.User( B.User.db.findOne( Mobj( socialIdName -> socialId ) ) )

      if ( user == null )
        T.web.redirect( wpath + "/register" + network )
      else if ( user.s( 'activationCode ).notBlank )
        notActivatedYet
      else
        sess.login( user )
    }

    T.web.redirect("/")
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

    val liid = user.s( app.idName )
    if ( liid.isBlank )
      web.redirect( "/" )

    if ( web.req.s( 'create ).notBlank ) {

      val email = web.req.s( 'un )

      if ( !validateUnusedEmail( email ) )
        web.redirect( web.path )

      app.importUser( user, liid )

      user( 'email )     = email
      user( 'createdOn ) = new Date

      sendActivation( user )

      user.save

      web.redirect("/")

    } else if ( web.req.s( 'link ).notBlank ) {

      val existing = getUserByEmailPassword( web.req.s( 'un ), web.req.s( 'pw ) )

      if ( existing != null ) {
        app.copyAttributes( from = user, to = existing )
        app.saveAttributes( existing )

        sess.login( existing )

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
             <input type="text" id="un" name="un" style="width:240px;" value={ user.s('email) }/>
             { Focus("#un") }
            </td>
           </tr>
          </table>
          <div class="btns" style="padding-top:16px;">
           <input name="create" type="submit" class="greenBtn" value={ "Create a New " + B.applicationName + " Account" }/>
          </div>
         </form>
        </div>
       </div>
       <div class="plainBox">
        <div class="title">... Or Do You Already Have a { B.applicationName } Account?</div>
        <div class="contents">
         <div>
          If you already have an existing { B.applicationName } account, please log in with it below.
          <p>This will link your existing { B.applicationName } account with your " + app.networkName + " account.</p>
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
           <input name="link" type="submit" class="greenBtn" value={ "Link your Existing " + B.applicationName + " Account to " + app.networkName }/>
          </div>
         </form>
        </div>
       </div>
      </tyr:shell> )
  }

  def notActivatedYet = {
    T.session.warn( "This account has not been activated yet!  Please check your email for the activation link." )
    T.web.redirect( "/?na=1" ) // na = need activation
  }
}

