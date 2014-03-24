/**
 * Copyright (c) 2008-2014 Tyranid <http://tyranid.org>
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

package org.tyranid.boot

import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.db.{ DbBoolean, DbInt, DbChar }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity, MongoRecord }
import org.tyranid.json.JsModel
import org.tyranid.sms.SMS
import org.tyranid.email.Email
import org.tyranid.web.{ Weblet, WebContext }

object TyranidConfig extends MongoEntity( tid = "a03t" ) {
  type RecType = TyranidConfig
  override def convert( obj:DBObject, parent:MongoRecord ) = new TyranidConfig( obj, parent )


  "_id"            is DbMongoId         is 'id;
  "recaptcha"      is DbBoolean         as "Require ReCaptchas";
  "lite"           is DbBoolean         as "Enable " + B.liteAppName;
  "accessLogs"     is DbBoolean         as "Enable Access Logs";
  "onePagePdf"     is DbBoolean         as "One Page PDF";
  "debugSso"       is DbBoolean         as "Debug SSO";
  "debugChat"      is DbBoolean         as "Debug Chat";
  "syncWebDav"     is DbBoolean         as "Sync WebDavs";
  "hideUpgradeBtn" is DbBoolean         as "Hide Upgrade Button";
  "allTmpl"        is DbBoolean         as "Templates for All";
  "confAlertLimit" is DbInt             as "Send Alert when this conference limit is hit";     
  "convIndd"       is DbInt             as "Convert InDesign Docs";     
  "profile"        is DbBoolean         as "Profiling";     
  "actOn"          is DbBoolean         as "Act On";     

  def apply():TyranidConfig = singleton

  private var _singleton:TyranidConfig = null
  
  def clear {
    _singleton = null
  }
  
  def singleton = {
    if ( _singleton == null ) {
      this.synchronized {
        if ( _singleton == null ) {
          val recc = db.find( Mobj() ).limit(1)
          var rec = recc.hasNext ? apply( recc.next ) | null
      
          if ( rec == null ) {
            rec = make
            rec.save
          }
    
          _singleton = rec
        }
      }
    }
    
    _singleton
  }
}

class TyranidConfig( obj:DBObject, parent:MongoRecord ) extends MongoRecord( TyranidConfig.makeView, obj, parent )


object TyranidConfiglet extends Weblet {
  def handle( web:WebContext ) = {
    val t = T
    val user = T.user

    if ( !user.isGod )
      _404

    val sess = t.session
    var common:JsModel = null

    rpath match {
    case "/" =>
      val cmd = web.s( 'cmd )
      
      cmd match {
      case "remap" =>
        org.tyranid.db.es.Es.mapAll
        sess.warn( "Re-Map initiated." )
  
      case "reindex" =>
        org.tyranid.db.es.Es.indexAll
        sess.warn( "Re-Index initiated." )
  
      case "eye" =>
        user( 'eye ) = !user.b( 'eye )
        user.save
        
        // This should change if T eye icons are seen in the current session
        common = JsModel( sess.user.toClientCommonMap(), "common" )
        sess.notice( "Debug has been turned " + ( user.b( 'eye ) ? "ON" | "OFF" ) + "." )
      
      case "sms" =>
        SMS.enabled = !SMS.enabled
        sess.notice( "SMS has been turned " + ( SMS.enabled ? "ON" | "OFF" ) + "." )
      
      case "email" =>
        Email.enabled = !Email.enabled
        sess.notice( "Email has been turned " + ( Email.enabled ? "ON" | "OFF" ) + "." )
      
      case "pdf" =>
        val obj = TyranidConfig()
        obj( 'onePagePdf ) = !B.onePagePdf
        TyranidConfig.db.update( Mobj( "_id" -> obj.id ), Mobj( $set -> Mobj( "onePagePdf" -> obj.b( 'onePagePdf ) ) ) )
        sess.notice( "One Page PDF has been turned " + ( B.onePagePdf ? "ON" | "OFF" ) + "." )
  
      case "indd" =>
        val obj = TyranidConfig()
        obj( 'convIndd ) = !B.convIndd
        TyranidConfig.db.update( Mobj( "_id" -> obj.id ), Mobj( $set -> Mobj( "convIndd" -> obj.b( 'convIndd ) ) ) )
        sess.notice( "InDesign Conversion has been turned " + ( B.convIndd ? "ON" | "OFF" ) + "." )
  
      case "tmpl" =>
        val obj = TyranidConfig()
        obj( 'allTmpl ) = !B.allTmpl
        TyranidConfig.db.update( Mobj( "_id" -> obj.id ), Mobj( $set -> Mobj( "allTmpl" -> obj.b( 'allTmpl ) ) ) )
        sess.notice( "Templates for ALL has been turned " + ( B.allTmpl ? "ON" | "OFF" ) + "." )
  
      case "sso" =>
        val obj = TyranidConfig()
        obj( 'debugSso ) = !B.debugSso
        TyranidConfig.db.update( Mobj( "_id" -> obj.id ), Mobj( $set -> Mobj( "debugSso" -> obj.b( 'debugSso ) ) ) )
        sess.notice( "SSO Debug has been turned " + ( B.debugSso ? "ON" | "OFF" ) + "." )
  
      case "actOn" =>
        val obj = TyranidConfig()
        obj( 'actOn ) = !B.actOn
        TyranidConfig.db.update( Mobj( "_id" -> obj.id ), Mobj( $set -> Mobj( "actOn" -> obj.b( 'actOn ) ) ) )
        sess.notice( "Act On has been turned " + ( B.actOn ? "ON" | "OFF" ) + "." )
  
      case "chat" =>
        val obj = TyranidConfig()
        obj( 'debugChat ) = !B.debugChat
        TyranidConfig.db.update( Mobj( "_id" -> obj.id ), Mobj( $set -> Mobj( "debugChat" -> obj.b( 'debugChat ) ) ) )
        sess.notice( "Chat Debug has been turned " + ( B.debugChat ? "ON" | "OFF" ) + "." )
  
      case "profile" =>
        val obj = TyranidConfig()
        obj( 'profile ) = !B.profile
        TyranidConfig.db.update( Mobj( "_id" -> obj.id ), Mobj( $set -> Mobj( "profile" -> obj.b( 'profile ) ) ) )
        sess.notice( "Profiling has been turned " + ( B.profile ? "ON" | "OFF" ) + "." )
  
      case "syncWebDav" =>
        val obj = TyranidConfig()
        obj( 'syncWebDav ) = !B.syncWebDav
        TyranidConfig.db.update( Mobj( "_id" -> obj.id ), Mobj( $set -> Mobj( "syncWebDav" -> obj.b( 'syncWebDav ) ) ) )
        sess.notice( "WebDav syncing has been turned " + ( B.syncWebDav ? "ON" | "OFF" ) + "." )
        
      case "hideUpgradeBtn" =>
        val obj = TyranidConfig()
        obj( 'hideUpgradeBtn ) = !B.hideUpgradeBtn
        TyranidConfig.db.update( Mobj( "_id" -> obj.id ), Mobj( $set -> Mobj( "hideUpgradeBtn" -> obj.b( 'hideUpgradeBtn ) ) ) )
        sess.notice( "Hide Upgrade Btn has been turned " + ( B.hideUpgradeBtn ? "ON" | "OFF" ) + "." )
        
      case "maint" =>
        B.maintenanceMode = true
 
        web.redirect( "/maintenance.html" )
      case "recaptcha" =>
        val obj = TyranidConfig()
        obj( 'recaptcha ) = !B.requireReCaptcha
        TyranidConfig.db.update( Mobj( "_id" -> obj.id ), Mobj( $set -> Mobj( "recaptcha" -> obj.b( 'recaptcha ) ) ) )
        sess.notice( "Recaptcha has been turned " + ( B.requireReCaptcha ? "ON" | "OFF" ) + "." )
  
      case "lite" =>
        val obj = TyranidConfig()
        obj( 'lite ) = !B.enableLite
        TyranidConfig.db.update( Mobj( "_id" -> obj.id ), Mobj( $set -> Mobj( "lite" -> obj.b( 'lite ) ) ) )
        sess.notice( B.liteAppName + " has been turned " + ( B.enableLite ? "ON" | "OFF" ) + "." )
  
      case "logs" =>
        val obj = TyranidConfig()
        obj( 'accessLogs ) = !B.accessLogs
        TyranidConfig.db.update( Mobj( "_id" -> obj.id ), Mobj( $set -> Mobj( "accessLogs" -> obj.b( 'accessLogs ) ) ) )
        sess.notice( "Access Logs have been turned " + ( B.accessLogs ? "ON" | "OFF" ) + "." )
  
      case _ =>
        //_404
      }
    }
    
    web.jsRes( 
      common, 
      JsModel(
         Map(
           "config" -> 
             Map(
              "eye"            -> user.b( 'eye ),
              "sms"            -> SMS.enabled,
              "onePagePdf"     -> B.onePagePdf,
              "allTmpl"        -> B.allTmpl,
              "debugSso"       -> B.debugSso,
              "debugChat"      -> B.debugChat,
              "syncWebDav"     -> B.syncWebDav,
              "hideUpgradeBtn" -> B.hideUpgradeBtn,
              "maint"          -> false,
              "email"          -> Email.enabled,
              "recaptcha"      -> B.requireReCaptcha,
              "lite"           -> B.enableLite,
              "accessLogs"     -> B.accessLogs,
              "profile"        -> B.profile,
              "convIndd"       -> B.convIndd,
              "actOn"          -> B.actOn
            )
         ),
         name = "main"
     ) )
  }
}
