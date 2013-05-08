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

import java.net.URLEncoder
import java.util.Date

import scala.xml.NodeSeq

import org.apache.http.auth.{ AuthScope }

import org.tyranid.Imp._
import org.tyranid.content.ContentType
import org.tyranid.db.{ DbChar, DbLink, DbBoolean, Record, DbText }
import org.tyranid.db.meta.TidItem
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity }
import org.tyranid.http.Http
import org.tyranid.json.{ Js, Json, JqHtml }
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
  "groupsAttrib"    is DbChar(20);
  "groupsAttribReq" is DbBoolean;
  "errorMessage"    is DbText;
  "actOptOut"       is DbBoolean;
  "featOptOut"      is DbBoolean;
  "naOptOut"        is DbBoolean;
  "newProjects"     is DbText;
  
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
  
  def pageWrapper( inner:NodeSeq ) =
   <div class="container">
    <div class="offset3 span6" style="margin-top:100px;text-align:center;">
     <a href="/"><img src="/volerro_logo.png"/></a>
    </div>
    <div class="offset3 span6">{ inner }</div>
   </div>
        
  def handle(web: WebContext) {
    val sess = T.session

    rpath match {
    case "/" =>      
      val sbt = web.b( 'xhrSbt )
      
      if ( !sbt ) {
        if ( web.b( 'xhr ) ) 
          web.jsRes( JqHtml( "#main", pageWrapper( signupBox ), transition="fadeOutIn", duration = 500 ), Js( "T.initFormPlaceholders( '#f' );" ) )
        else
          web.template( <tyr:shell>{ pageWrapper( signupBox ) }</tyr:shell> )
      } else {
        val code = web.s( 'code )
        
        if ( code.notBlank ) {
          val mapping = SsoMapping.getById( code )
          
          if ( mapping == null ) {
            sess.error( "Unable to find this code.  Please contact " + B.applicationName + " support." )
            web.jsRes()
          } else if ( mapping.s( 'idpId ).notBlank ) {
            //sess.error( "That code has been registered.  Please contact " + B.applicationName + " support if this information needs to be changed." )
            web.jsRes( Js( "V.app.load( '/sso/auth/" + code + "' );" ) )
            //web.jsRes()
          } else {
            val orgId = mapping.oid( 'org )
            val org = TidItem.by( B.Org.idToTid( orgId ) )
            sess.notice( "Welcome " + org.name + "!  Please complete the form below to complete your SSO setup." )
            sess.put( "sso", mapping )
            web.jsRes( Js( """
$( '.step2' ).fadeIn(); 
$( '#code' ).attr('disabled','disabled'); 
$( '#ssoBtn span' ).text( 'Finish' );
$( $('#idp').focus() );
""" ) ) 
          }
        } else {
          val mapping = sess.get( "sso" ).as[Record]
          mapping( "idpId" ) = web.s( 'idp )
          mapping( "emailAttrib" ) = web.s( 'email )
          mapping( "firstNameAttrib" ) = web.s( 'fname )
          mapping( "lastNameAttrib" ) = web.s( 'lname )
          mapping.save
          
          web.jsRes( JqHtml( "#main", pageWrapper(
            <div class="container-fluid" style="padding:0;">
             <div class="row-fluid">
              <h1 class="span12" style="text-align:center">Single Sign-On Setup Complete!</h1>
             </div>
             <div class="row-fluid">
              <h3 class="span12" style="text-align:center">Your single-sign URL for { B.applicationName } is:</h3>
              <h3 class="span12" style="text-align:center"><a href={ T.website + "/sso/auth/" + mapping.id }>{ T.website + "/sso/auth/" + mapping.id }</a></h3>
             </div>
            </div> ) ) )
        }
      }
    case s if s.startsWith( "/errTest/" ) =>
      val id = s.split( "/" )(2)
      val mapping = ( id == "test" ) ? SsoMapping.testMapping | SsoMapping.getById( id )
      
      val errorMessage = mapping.s( 'errorMessage )
      
      if ( errorMessage.isBlank ) {
        sess.error( "Sorry, this account has been deactivated." )
        web.jsRes()
      } else {
        web.template( <tyr:shell>{ errorMessageBox( errorMessage ) }</tyr:shell> )
      }
      
      return
    
    case s if s.startsWith( "/auth/" ) =>
      val id = s.split( "/" )(2)
      val mapping = ( id == "test" ) ? SsoMapping.testMapping | SsoMapping.getById( id )
      
      if ( mapping == null || mapping.s( 'idpId ).isBlank ) {
        sess.error( "SSO Mapping for code " + id + " not found." )
        
        if ( web.b( 'xhr ) ) 
          web.jsRes( JqHtml( "#main", pageWrapper( signupBox ), transition="fadeOutIn", duration = 500 ), Js( "T.initFormPlaceholders( '#f' );" ) )
        else
          web.template( <tyr:shell>{ pageWrapper( signupBox ) }</tyr:shell> )
      } else {
        sess.put( "sso", mapping )
        val idpId = URLEncoder.encode( mapping.s( 'idpId ), "UTF-8" ) 
        web.res.sendRedirect( "https://sso.connect.pingidentity.com/sso/sp/initsso?saasid=" + SAAS_ID + "&idpid=" + idpId + "&appurl=" + TOKEN_URL + "&appurl=" + ERROR_URL )
      }
    case "/token" =>
      val token = web.s( 'tokenid )
      val str = Http.GET( "https://sso.connect.pingidentity.com/sso/TXS/2.0/1/" + token, authScope = AuthScope.ANY, username = B.pingIdentityUsername, password = B.pingIdentityPassword, preemptive = true ).s
 
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
      val json = Json.parse( str )
      val mapping = sess.get( "sso" ).as[Record]
      val email = json.s( mapping.s( 'emailAttrib ) )
      
      if ( email.isBlank ) {
        sess.error( "There is no email field with the attribute name " + mapping.s( 'emailAttrib ) + " in the response:" + str )
        web.jsRes()
        return
      }
      
      val mustHaveGroups = mapping.b( 'groupsAttribReq )
      val groupNames = ( json.s( mapping.s( 'groupsAttrib ) ) or "" ).split( "," )
      
      if ( mustHaveGroups && groupNames.length == 0 ) {
        sess.error( "Sorry, but you must be a member of a group to access " + B.applicationName + "." )
        web.jsRes()
        return
      }
      
      val orgId = mapping.oid( 'org )
      val user = B.User.db.findOne( Mobj( "email" -> ("^" + email.encRegex + "$").toPatternI ) )
      
      if ( user == null ) {
        // Create a new one
        val org = B.Org.getById( orgId )
        val newUser = B.newUser()       
        
        val fnameAttrib = mapping.s( 'firstNameAttrib )
        
        if ( fnameAttrib.notBlank )
          newUser( 'firstName ) = json.s( fnameAttrib )
          
        val lnameAttrib = mapping.s( 'lastNameAttrib )
        
        if ( lnameAttrib.notBlank )
          newUser( 'lastName ) = json.s( lnameAttrib )
          
        newUser( 'email ) = email
        newUser( 'createdOn ) = new Date
        newUser( 'password ) = ""
        
        if ( mapping.b( 'actOptOut ) )
          newUser( 'actOptOut ) = true
          
        if ( mapping.b( 'featOptOut ) )
          newUser( 'featOptOut ) = true
        
        if ( mapping.b( 'naOptOut ) )
          newUser( 'naOptOut ) = true
          
        newUser.join( org )
        sess.login( newUser )
        newUser.save
        B.welcomeUserEvent
        Group.ensureInOrgGroup( newUser )        
        
        // Add them to any groups specified
        if ( mustHaveGroups && groupNames.size > 0 ) {
          val groups = Group.db.find( Mobj( "org" -> orgId, "ssoSynced" -> true ), Mobj( "name" -> 1 ) ).toSeq
          
          for ( group <- groups if ( groupNames.find( g => g.toLowerCase == group.s( 'name ).toLowerCase ) != None ) )
            Group.db.update( Mobj( "_id" -> group.id ), $addToSet( "v", newUser.tid ) )          
        }
        
        val newProjectNames = mapping.s( 'newProjects )
        
        if ( newProjectNames.notBlank ) {
          val now = new Date
          val userList = Mlist( newUser.tid )
          
          newProjectNames.split( "," ).foreach( name => {
            val project = Group.make
            project( 'name    ) = name
            project( 'on      ) = now
            project( 'o       ) = userList
            project( 'type    ) = ContentType.Project.id
            project( 'color   ) = project.getRandomColor
            project( 'v       ) = userList
            project( 'members ) = userList
            project.save
            
            val newBoard = B.DocEntity.make
            newBoard( 'name        ) = "Files"
            newBoard( 'on          ) = now
            newBoard( 'o           ) = userList
            newBoard( 'v           ) = userList
            newBoard( 'type        ) = ContentType.Folder.id
            newBoard( 'parentGroup ) = project.id
            newBoard( 'lastModified )      = now
            newBoard( 'lastModifiedBy )    = newUser.id
            newBoard( 'lastModifiedByOrg ) = newUser.orgId
            newBoard( 'lastAction ) = now
            newBoard( 'lastActionBy ) = newUser.id
            newBoard( 'lastActionByOrg ) = org.id
            newBoard.save
            
          } )
        }
        
      } else if ( user.b( 'inactive ) ) {
        val errorMessage = mapping.s( 'errorMessage )
        
        if ( errorMessage.isBlank ) {
          sess.error( "Sorry, this account has been deactivated." )
          web.jsRes()
        } else {
          web.template( <tyr:shell>{ errorMessageBox( errorMessage ) }</tyr:shell> )
        }
        
        return
      } else {
        //println( "Log in via SSO:" + email )
        val u = B.User( user )
        var save = false
        
        if ( u.s( 'firstName ).isBlank ) {
          val fnameAttrib = mapping.s( 'firstNameAttrib )
          u( 'firstName ) = json.s( fnameAttrib )
          save = true
        }
        
        if ( u.s( 'lastName ).isBlank ) {
          val lnameAttrib = mapping.s( 'lastNameAttrib )
          u( 'lastName ) = json.s( lnameAttrib )
          save = true
        }

        if ( save )
          u.save
          
        // Add or remove them them to any groups specified
        if ( mustHaveGroups && groupNames.size > 0 ) {
          val groups = Group.db.find( Mobj( "org" -> orgId, "ssoSynced" -> true ), Mobj( "name" -> 1 ) ).toSeq
          
          for ( group <- groups ) {
            if ( groupNames.find( g => g.toLowerCase == group.s( 'name ).toLowerCase ) == None ) {
              Group.db.update( Mobj( "_id" -> group.id ), $pull( "v", u.tid ) )
            } else {
              Group.db.update( Mobj( "_id" -> group.id ), $addToSet( "v", u.tid ) )
            }
          }
        }
        
        sess.login( u )
      }
   
      web.redirect( "/#dashboard" )
    case "/error" =>
      web.redirect( "/#dashboard" )
    case _ =>
      web.redirect( "/#dashboard" )
    }  
  }
    
  def errorMessageBox( errorMessage:String ) = {
   <div class="container">
    <form method="post" id="f" class="sso offset2 span8" style="margin-bottom:12px;">
     <fieldset class="ssoBox">
      <div class="container-fluid" style="padding:0;">
       <div class="row-fluid">
        <h1 class="span12" style="text-align:center">Deactivated account</h1>
       </div>
      </div>
      <hr style="margin:4px 0 30px;"/>
      <div class="top-form-messages"/>
      <div class="container-fluid" style="padding:0;">
       <div class="row-fluid">
        <div class="errorMessage" style="white-space:pre-wrap;font-family:'Courier New',monospace;">{ errorMessage }</div>
       </div>
      </div>
     </fieldset>
    </form>    
   </div>
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
        <h1 class="span12" style="text-align:center">Single Sign-On Setup</h1>
        <h4 class="span12" style="text-align:center">SSO Services for <a href="https://wwww.pingidentity.com" target="_blank">PingIdentity &reg;</a></h4>
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
        <input type="text" class="span8" name="idp" id="idp" placeholder="company.connect.pingidentity.com" data-val="req" data-val-label="PingIdentity Provider Id"/>
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
