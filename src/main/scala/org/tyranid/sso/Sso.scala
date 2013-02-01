package org.tyranid.sso

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

import com.mongodb.DBObject

import java.net.URLEncoder
import java.util.Date

import org.apache.http.auth.{ AuthScope }

import org.tyranid.Imp._
import org.tyranid.db.{ DbChar, DbLink }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity }
import org.tyranid.http.Http
import org.tyranid.json.{ Js, Json }
import org.tyranid.profile.{ Org, User, Group }
import org.tyranid.ui.Focus
import org.tyranid.web.{ Weblet, WebContext, WebTemplate }

object SsoMapping extends MongoEntity( tid = "a0Ut" ) {
  "_id"             is DbChar(8)     is 'id;
  "org"             is DbLink(B.Org) is 'required;
  "idpId"           is DbChar(40)    is 'required;
  "emailAttrib"     is DbChar(20)    is 'required;
  "firstNameAttrib" is DbChar(20);
  "lastNameAttrib"  is DbChar(20);
  
  lazy val testMapping = {
    val ts = SsoMapping.make
    ts( 'idpId ) = "testidp.connect.pingidentity.com"
    ts( 'emailAttrib ) = "pingone.subject"
    ts
  }
}

object Ssolet extends Weblet {
  lazy val SAAS_ID = URLEncoder.encode( B.saasId, "UTF-8" )
  lazy val TOKEN_URL = URLEncoder.encode( T.website + "/sso/token", "UTF-8" ) 
  lazy val ERROR_URL = URLEncoder.encode( T.website + "/sso/error", "UTF-8" ) 

  // PingOne Documentation for this
  // https://connect.pingidentity.com/web-portal/appintegration?x=tyGMaRMgiMSYAHNoa21b84ce4ZKmtJ88
  
  def handle(web: WebContext) {
    val sess = T.session
    
    rpath match {
    case "/" =>
      val sbt = web.b( 'xhrSbt )
      if ( web.b( "xhr" ) && !sbt ) {
        val jsonRes = web.jsonRes( sess )
        
        jsonRes.htmlMap = Map( 
              "html" -> 
               <div class="container">
                <div class="offset3 span6" style="margin-top:100px;text-align:center;">
                 <a href="/"><img src="/volerro_logo.png"/></a>
                </div>
                <div class="offset3 span6">
                 { signupBox }
                </div>
               </div> ,
              "target" -> "#main",
              "transition" -> "fadeOutIn",
              "duration" -> 500 )

        jsonRes.extraJS = "tyr.initFormPlaceholders( 'f' );"
        web.json( jsonRes )
      } else {
        val code = web.s( 'code )
        
        if ( code.notBlank ) {
          val mapping = SsoMapping.getById( code )
          
          if ( mapping == null ) {
            sess.error( "Unable to find this code.  Please contact " + B.applicationName + " support." )
            web.jsRes()
          } else if ( mapping.s( 'idpId ).notBlank ) {
            //sess.error( "That code has been registered.  Please contact " + B.applicationName + " support if this information needs to be changed." )
            web.jsRes( Js( "tyr.app.loadMain( '/sso/auth/'" + code + " );" ) )
            //web.jsRes()
          } else {
            val orgId = mapping.oid( 'org )
            val org = B.Org.getById( orgId )
            sess.notice( "Welcome " + org.name + "!  Please complete the form below to complete your SSO registration." )
            sess.put( "sso", mapping )
            web.jsRes( Js( """
$( '.step2' ).fadeIn(); 
$( '#code' ).attr('disabled','disabled'); 
$( '#ssoBtn span' ).text( 'Finish' );
$( $('#idp').focus() );
""" ) ) 
          }
        } else {
          val mapping = sess.get( "sso" ).as[DBObject]
          mapping( "idpId" ) = web.s( 'idp )
          mapping( "email" ) = web.s( 'email )
          mapping( "fname" ) = web.s( 'fname )
          mapping( "lname" ) = web.s( 'lname )
         // SsoMapping.save( mapping )
        }
      }
    case s if s.startsWith( "/auth/" ) =>
      val id = s.split( "/" )(2)
      val mapping = ( id == "test" ) ? SsoMapping.testMapping | SsoMapping.getById( id )
      
      if ( mapping == null ) {
        sess.error( "SSO Mapping for code " + id + " not found." )
        web.jsRes( Js( "tyr.app.loadMain( '/sso' );" ) )
      } else {
        sess.put( "sso", mapping )
        val idpId = URLEncoder.encode( mapping.s( 'idpId ), "UTF-8" ) 
        web.res.sendRedirect( "https://sso.connect.pingidentity.com/sso/sp/initsso?saasid=" + SAAS_ID + "&idpid=" + idpId + "&appurl=" + TOKEN_URL + "&appurl=" + ERROR_URL )
      }
    case "/token" =>
      val token = web.s( 'tokenid )
      //println( "Token: " + token )
      
      val str = Http.GET( "https://sso.connect.pingidentity.com/sso/TXS/2.0/1/" + token, authScope = AuthScope.ANY, username = B.pingIdentityUsername, password = B.pingIdentityPassword, preemptive = true ).s
      //println( str )
 
/*      
{
   "pingone.nameid.format":"urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress",
   "pingone.idp.id":"testidp.connect.pingidentity.com",
   "pingone.authn.context":"urn:oasis:names:tc:SAML:2.0:ac:classes:Password",
   "subject":"testuser1@testidp.connect.pingidentity.com",
   "pingone.nameid.qualifier":"",
   "pingone.saas.id":"4c25fda7-844b-42ad-adc3-f8baccbab88e",
   "pingone.subject":"testuser1@testidp.connect.pingidentity.com",
   "pingone.assertion.id":"onqw710l.IVmpl3NbjODYIe8G5g",
   "pingone.authninstant":"1359661588000"
}      
*/
      val json = Json.parse( str ) // .get(0)
      
      val mapping = sess.get( "sso" ).as[DBObject]
      val email = json.s( mapping.s( 'emailAttrib ) )
      
      val user = B.User.db.findOne( Mobj( "email" -> ("^" + email.encRegex + "$").toPatternI ) )
      
      if ( user == null ) {
        // Create a new one
        val orgId = mapping.oid( 'org )
        
        if ( orgId != null ) {
          val org = B.Org.getById( orgId )
          val newUser = B.newUser()          
          //newUser( 'firstName ) = contact.s( 'name ).split( ' ' )(0)
          //newUser( 'lastName ) = contact.s( 'name ).split( ' ' )(1)
          newUser( 'email ) = email
          newUser( 'createdOn ) = new Date
          newUser.join( org )
          sess.login( newUser )
          newUser.save
          B.welcomeUserEvent
          Group.ensureInOrgGroup( newUser )        
        } else {
          
        }
      } else if ( !user.b( 'inactive ) ) {
        sess.login( B.User( user ) )
      } else {
        sess.error( "Sorry, this account has been deactivated" )
        web.jsRes()
        return
      }
   
      web.jsRes( Js( "tyr.app.loadMenubar( '/user/menubar' ); tyr.app.loadMain( '/dashboard' );" ) )
    case "/error" =>
    }  
  }
    
  def signupBox = {
    val thread = T
    val user = thread.user
    val web = thread.web
    val loggingOut = web.req.s( 'lo ).notBlank
    val noSocial = web.b( 'nosocial )

    val params = noSocial |* "?nosocial=1"

    <form method="post" action={ wpath } id="f" class="sso" style="margin-bottom:12px;" data-val="1" data-val-top="1">
     <fieldset class="ssoBox">
      <div class="container-fluid" style="padding:0;">
       <div class="row-fluid">
        <h1 class="span12" style="text-align:center">Single Sign-On Registration</h1>
       </div>
      </div>
      <hr style="margin:4px 0 30px;"/>
      <div class="top-form-messages"/>
      <div class="container-fluid" style="padding:0;">
       <div class="row-fluid">
        <label for="code" class="span4">Assigned Code:</label>
        <input type="text" class="span8" id="code" name="code" autocapitalize="on" data-val="req" data-val-label="Assigned Code"/>
        { Focus("#code") }
       </div>
       <div class="row-fluid step2">
        <label for="idp" class="span4">Identity Provider Id:</label>
        <input type="text" class="span8" name="idp" id="idp" placeholder="company.connect.pingidentity.com" data-val="req" data-val-label="Identity Provider Id"/>
       </div>
       <div class="row-fluid step2">
        <label for="email" class="span4">Email Attribute:</label>
        <input type="text" class="span8" name="email" id="email" placeholder="subject" data-val="req" data-val-label="Email Attribute Name"/>
       </div>
       <div class="row-fluid step2">
        <label for="fname" class="span4">First Name Attribute:</label>
        <input type="text" class="span8" name="fname" id="fname"/>
       </div>
       <div class="row-fluid step2">
        <label for="lname" class="span4">Last Name Attribute:</label>
        <input type="text" class="span8" name="lname" id="lname"/>
       </div>
       <div class="row-fluid">
         <div class="span12" style="height:40px;padding-top:8px;"><button type="submit" id='ssoBtn' class="btn-success btn pull-right"><span>Lookup</span> <i class="icon-caret-right"></i></button></div>
       </div>
      </div>
     </fieldset>
    </form>
    }
}
