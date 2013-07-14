package org.tyranid.sso

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

import java.net.URLEncoder
import java.util.Date

import scala.xml.NodeSeq

import org.apache.http.auth.{ AuthScope }

import org.tyranid.Imp._
import org.tyranid.content.ContentType
import org.tyranid.db.{ DbChar, DbLink, DbBoolean, Record, DbText, DbUrl }
import org.tyranid.db.meta.TidItem
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity }
import org.tyranid.http.Http
import org.tyranid.json.{ Js, Json, JqHtml }
import org.tyranid.profile.{ Org, User, Group }
import org.tyranid.ui.Focus
import org.tyranid.web.{ Weblet, WebContext, WebTemplate }

// US BANK ERROR MESSAGE:
//   "errorMessage" : "Your Reserve Marketplace account has been deactivated for security reasons after inactivity over 90 days.\n\nTo reactivate your account, please contact The Resolution Center.\n\nEmail Address: resolution.center@usbank.com\nInternal Employee Phone: 651-466-7103\nStandard Hours:  Mon-Fri 1:00 am CT - 7:00 pm CT\n"

object SsoMapping extends MongoEntity( tid = "a0Ut" ) {
  "_id"             is DbChar(8)     is 'id;
  "org"             is DbLink(B.Org) is 'required;
  "idpId"           is DbChar(40)    is 'required;
  "emailAttrib"     is DbChar(20)    is 'required;
  "firstNameAttrib" is DbChar(20);
  "lastNameAttrib"  is DbChar(20);
  "titleAttrib"     is DbChar(20);
  "iconUrlAttrib"   is DbChar(30);
  "groupsAttrib"    is DbChar(20);
  "groupsAttribReq" is DbBoolean;
  "errorMessage"    is DbText;
  "actOptOut"       is DbBoolean;
  "featOptOut"      is DbBoolean;
  "naOptOut"        is DbBoolean;
  "inviteOptOut"    is DbBoolean;
  "newProjects"     is DbText;
  "loEndpoint"      is DbUrl; // log out endpoint
  
  lazy val testMapping = {
    val ts = SsoMapping.make
    ts( 'idpId ) = "testidp.connect.pingidentity.com"
    ts( 'emailAttrib ) = "pingone.subject"
    ts
  }  
}

object Ssolet extends Weblet {
  lazy val SAAS_ID = URLEncoder.encode( B.saasId, "UTF-8" )
  lazy val TOKEN_URL = URLEncoder.encode( T.baseWebsite + "/sso/token/", "UTF-8" ) 
  lazy val ERROR_URL = URLEncoder.encode( T.baseWebsite + "/sso/error", "UTF-8" ) 

  def tokenUrl( id:String, startUrl:String ) = startUrl.isBlank ? ( TOKEN_URL + id ) | URLEncoder.encode( T.baseWebsite + "/sso/token/" + id + "?startUrl=" + URLEncoder.encode( startUrl, "UTF-8" ), "UTF-8" )
  
  // PingOne Documentation for this
  // https://connect.pingidentity.com/web-portal/appintegration?x=tyGMaRMgiMSYAHNoa21b84ce4ZKmtJ88
  
  def pageWrapper( inner:NodeSeq ) =
   <div class="container">
    <div class="offset3 span6" style="margin-top:100px;text-align:center;">
     <a href="/"><img src="/volerro_logo.png"/></a>
    </div>
    <div class="offset3 span6">{ inner }</div>
   </div>

  def loginUser( user:User, web:WebContext ) {
    val thread = T
    thread.http = web.req.getSession( true )
    thread.web = web
       
    thread.session.login( user )
  }
  
  def handle(web: WebContext) {
    var sess = T.session

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
          mapping( "titleAttrib" ) = web.s( 'title )
          mapping( "iconUrlAttrib" ) = web.s( 'iconUrl )
          
          mapping.save
          
          web.jsRes( JqHtml( "#main", pageWrapper(
            <div class="container-fluid" style="padding:0;">
             <div class="row-fluid">
              <h1 class="span12" style="text-align:center">Single Sign-On Setup Complete!</h1>
             </div>
             <div class="row-fluid">
              <h3 class="span12" style="text-align:center">Your single-sign URL for { B.applicationName } is:</h3>
              <h3 class="span12" style="text-align:center"><a href={ T.baseWebsite + "/sso/auth/" + mapping.id }>{ T.baseWebsite + "/sso/auth/" + mapping.id }</a></h3>
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
    
    case r if r.startsWith( "/redir/" ) =>
      val url = T.website( "/#messages", T.user )
      println( url )
      web.jsRes()
    case s if s.startsWith( "/auth/" ) =>
      val id = s.split( "/" )(2)
      val mapping = ( id == "test" ) ? SsoMapping.testMapping | SsoMapping.getById( id )
      
      if ( mapping == null || mapping.s( 'idpId ).isBlank ) {
        if ( B.debugSso )
          println( "DEBUG: Incoming /auth call for: " + id + ", mapping not found" )
      
        sess.error( "SSO Mapping for code " + id + " not found." )
        
        if ( web.b( 'xhr ) ) 
          web.jsRes( JqHtml( "#main", pageWrapper( signupBox ), transition="fadeOutIn", duration = 500 ), Js( "T.initFormPlaceholders( '#f' );" ) )
        else
          web.template( <tyr:shell>{ pageWrapper( signupBox ) }</tyr:shell> )
      } else {
        if ( B.debugSso )
          println( "DEBUG: Incoming /auth call for: " + id + ", mapping found" )
      
        val idpId = URLEncoder.encode( mapping.s( 'idpId ), "UTF-8" )
        
        if ( B.debugSso )
          println( "DEBUG: Mapping found, trying ping identity for ipdid " + idpId )
          
        val appUrl = tokenUrl( id, web.s( 'startUrl ) )
        
        web.res.sendRedirect( "https://sso.connect.pingidentity.com/sso/sp/initsso?saasid=" + SAAS_ID + "&idpid=" + idpId + "&appurl=" + appUrl + "/" + id + "&errorurl=" + ERROR_URL )
      }
    case t if t.startsWith( "/token/" ) =>
      val id = t.split( "/" )(2)
      val token = web.s( 'tokenid )
      val startUrl = web.s( 'startUrl )
      
      if ( token.isBlank ) {
        sess.error( "No token sent!" )
        web.jsRes()
        return
      }
      
      if ( B.debugSso )
        println( "DEBUG: Trying to get info for token: " + token )
        
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
      if ( str.isBlank ) {
        sess.error( "No response when trying to retrieve sso token: " + token )
        web.jsRes()
        return
      }
      
      if ( B.debugSso )
        println( "DEBUG: Raw string response: " + str )
        
      val json = Json.parse( str )
      
      if ( B.debugSso )
        println( "Json Object: " + json )
      
      var mapping = SsoMapping.getById( id )

      if ( mapping == null ) {
        println( "DEBUG: Mapping not found!" )
        sess.error( "Unable to determine SSO profile." )
        web.jsRes()
        return
      }
      
      val email = json.s( mapping.s( 'emailAttrib ) )
      
      if ( email.isBlank ) {
        if ( B.debugSso )
          println( "DEBUG: There is no email field with the attribute name " + mapping.s( 'emailAttrib ) + "." )
        
        sess.error( "There is no email field with the attribute name " + mapping.s( 'emailAttrib ) + " in the response:" + str )
        web.jsRes()
        return
      }
      
      val mustHaveGroups = mapping.b( 'groupsAttribReq )
      val groupNames = ( json.s( mapping.s( 'groupsAttrib ) ) or "" ).split( "," )
      
      if ( mustHaveGroups && groupNames.length == 0 ) {
        if ( B.debugSso ) {
          println( "DEBUG: Sorry, but you must be a member of a group to access " + B.applicationName + "." )
          sess.error( "Your SSO profile does not list you as a member of group and it is required." )
        } else {
          sess.error( "Sorry, but you must be a member of a group to access " + B.applicationName + "." )
          web.jsRes()
        }
      }
      
      val orgId = mapping.oid( 'org )
      val user = B.User.db.findOne( Mobj( "email" -> ("^" + email.encRegex + "$").toPatternI ) )
      
      if ( user == null ) {
        if ( B.debugSso )
          println( "DEBUG: User not found for email " + email + ", creating one." )
          
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
        
        val titleAttrib = mapping.s( 'titleAttrib )
        
        if ( titleAttrib.notBlank )
          newUser( 'title ) = json.s( titleAttrib )
          
        val iconUrlAttrib = mapping.s( 'iconUrlAttrib )
        
        //if ( iconUrlAttrib.notBlank )
        //  newUser( 'firstName ) = json.s( iconUrlAttrib )
          
        if ( mapping.b( 'actOptOut ) )
          newUser( 'actOptOut ) = true
          
        if ( mapping.b( 'featOptOut ) )
          newUser( 'featOptOut ) = true
        
        if ( mapping.b( 'naOptOut ) )
          newUser( 'naOptOut ) = true
          
        if ( mapping.b( 'inviteOptOut ) )
          newUser( 'inviteOptOut ) = true
          
        newUser.join( org )
        newUser.save
        
        loginUser( newUser, web )
        sess = T.session
        
        if ( B.debugSso )
          println( "DEBUG: New user created and saved." )
          
        B.welcomeUserEvent
        Group.ensureInOrgGroup( newUser )        
        
        // Add them to any groups specified
        if ( mustHaveGroups && groupNames.size > 0 ) {
          
          val groups = Group.db.find( Mobj( "lastModifiedByOrg" -> orgId, "ssoSynced" -> true ), Mobj( "name" -> 1 ) ).toSeq

          if ( groups.size == 0 ) {
            if ( B.debugSso  )
              println( "DEBUG: No SSO managed groups found when they must have them!" )
          } else {
            for ( group <- groups if ( groupNames.find( g => g.toLowerCase == group.s( 'name ).toLowerCase ) != None ) ) {
              Group.db.update( Mobj( "_id" -> group.id ), $addToSet( "v", newUser.tid ) )
              
              if ( B.debugSso )
                println( "DEBUG: Adding user to group " + group.s( 'name ) )
            }
          }
        }
        
        val newProjectNames = mapping.s( 'newProjects )

        if ( B.debugSso )
          println( "DEBUG: New project names: " + newProjectNames )
        
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
            
            if ( B.debugSso )
              println( "DEBUG: Created new group for user: " + name )
            
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
        if ( B.debugSso )
          println( "DEBUG: User in inactive." )
              
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
        
        val fName = u.s( 'firstName )
        
        if ( fName.isBlank || fName == "unknown" ) {
          val fnameAttrib = mapping.s( 'firstNameAttrib )
          u( 'firstName ) = json.s( fnameAttrib )
          save = true
        }
        
        val lName = u.s( 'lastName )
        
        if ( lName.isBlank || lName == "unknown" ) {
          val lnameAttrib = mapping.s( 'lastNameAttrib )
          u( 'lastName ) = json.s( lnameAttrib )
          save = true
        }

        val titleAttrib = mapping.s( 'titleAttrib )
        
        if ( titleAttrib.nonBlank ) {
          val title = json.s( titleAttrib )
          
          if ( title.notBlank ) {
            u( 'title ) = json.s(title )
            save = true
          }
        }
        
//        if ( u.s( 'thumbnail ).isBlank ) {
//           user( 'thumbnail ) = publicBucket.url( path )
//
//          val titleAttrib = mapping.s( 'titleAttrib )Ω
//          u( 'title ) = json.s( titleAttrib )
//          save = true
//        }
        
        if ( save )
          u.save
          
        // Add or remove them them to any groups specified
        if ( mustHaveGroups && groupNames.size > 0 ) {            
          val groups = Group.db.find( Mobj( "lastModifiedByOrg" -> orgId, "ssoSynced" -> true ), Mobj( "name" -> 1 ) ).toSeq
          
          if ( B.debugSso && groups.size == 0 )
            println( "DEBUG: No SSO managed groups found when they must have them!" )
          
          for ( group <- groups ) {
            if ( groupNames.find( g => g.toLowerCase == group.s( 'name ).toLowerCase ) == None ) {
              
              if ( B.debugSso )
                println( "DEBUG: Removing user from group " + group.s( 'name ) )
                
              Group.db.update( Mobj( "_id" -> group.id ), $pull( "v", u.tid ) )
            } else {
              if ( B.debugSso )
                println( "DEBUG: Ensuring user is in group " + group.s( 'name ) )
                
              Group.db.update( Mobj( "_id" -> group.id ), $addToSet( "v", u.tid ) )
            }
          }
        }
        
        loginUser( u, web )
        sess = T.session
        
        if ( B.debugSso )
          println( "DEBUG: User is logged in." )
      }
   
      web.redirect( startUrl.isBlank ? "/#dashboard" | startUrl )
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
      <div class="top-form-messages"></div>
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
      <div class="top-form-messages"></div>
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
       <div class="row-fluid step2">
        <label for="title" class="span4">Title Attribute:</label>
        <input type="text" class="span8" name="title" id="title"/>
       </div>
       <!--div class="row-fluid step2">
        <label for="iconUrl" class="span4">Icon URL Attribute:</label>
        <input type="text" class="span8" name="iconUrl" id="iconUrl"/>
       </div-->
       <div class="row-fluid">
         <div class="span12" style="height:40px;padding-top:8px;"><button type="submit" id='ssoBtn' class="btn-success btn pull-right"><span>Lookup</span> <i class="icon-caret-right"></i></button></div>
       </div>
      </div>
     </fieldset>
    </form>
    }
}
