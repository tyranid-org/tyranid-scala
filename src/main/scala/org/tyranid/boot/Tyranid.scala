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

package org.tyranid.boot

import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.db.DbBoolean
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity, MongoRecord }
import org.tyranid.sms.SMS
import org.tyranid.web.{ Weblet, WebContext }



object TyranidConfig extends MongoEntity( tid = "a03t" ) {
  type RecType = TyranidConfig
  override def convert( obj:DBObject, parent:MongoRecord ) = new TyranidConfig( obj, parent )


  "_id"       is DbMongoId         is 'id;
  "recaptcha" is DbBoolean         as "Enable ReCaptchas";


  def apply():TyranidConfig = singleton

  lazy val singleton = {
    var rec = apply( db.findOne( Mobj() ) )

    if ( rec == null ) {
      rec = make
      rec.save
    }

    rec
  }
}

class TyranidConfig( obj:DBObject, parent:MongoRecord ) extends MongoRecord( TyranidConfig.makeView, obj, parent )


object TyranidConfiglet extends Weblet {

  def ui = {
    val user = T.user

    <a href={ wpath + "/reindex" } class="stop btn">Re-Index Search</a>
    <a href={ wpath + "/eye" } class={ user.b( 'eye ) ? "go btn" | "stop btn" }>Debug: { user.b( 'eye ) ? "ON" | "OFF" }</a>
    <a href={ wpath + "/sms" } class={ SMS.enabled ? "go btn" | "stop btn" }>SMS: { SMS.enabled ? "ON" | "OFF" }</a>
    <a href={ wpath + "/recaptcha" } class={ B.requireReCaptcha ? "go btn" | "stop btn" }>Recaptcha: { B.requireReCaptcha ? "ON" | "OFF" }</a>;
  }

  def handle( web:WebContext ) = {
    val t = T
    val user = T.user

    if ( !user.isGod )
      _404

    val sess = t.session

    rpath match {
    case "/reindex" =>
      org.tyranid.db.es.Es.indexAll
      sess.warn( "Re-Index initiated." )
      web.redirect( parent.wpath )

    case "/eye" =>
      user( 'eye ) = !user.b( 'eye )
      user.save
      sess.notice( "Debug has been turned " + ( user.b( 'eye ) ? "ON" | "OFF" ) + "." )
      web.redirect( parent.wpath )
    
    case "/sms" =>
      SMS.enabled = !SMS.enabled
      sess.notice( "SMS has been turned " + ( SMS.enabled ? "ON" | "OFF" ) + "." )
      web.redirect( parent.wpath )
    
    case "/recaptcha" =>
      val obj = TyranidConfig()
      obj( 'recaptcha ) = !B.requireReCaptcha
      TyranidConfig.db.update( Mobj( "_id" -> obj.id ), Mobj( $set -> Mobj( "recaptcha" -> obj.b( 'recaptcha ) ) ) )
      sess.notice( "Recaptcha has been turned " + ( B.requireReCaptcha ? "ON" | "OFF" ) + "." )
      web.redirect( parent.wpath )

    case _ =>
      _404
    }
  }
}


