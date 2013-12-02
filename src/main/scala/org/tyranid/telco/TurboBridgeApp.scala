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

package org.tyranid.telco

import scala.collection.JavaConversions._

import java.util.HashMap

import org.apache.http.NameValuePair
import org.apache.http.message.BasicNameValuePair

import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.content.Content
import org.tyranid.db.{ DbArray, DbChar, DbLink, DbDateTime, DbTid }
import org.tyranid.db.mongo._
import org.tyranid.json.{ Json, JsModel }
import org.tyranid.session.Session
import org.tyranid.web.{ WebContext, Weblet }


object Conference extends MongoEntity( "a0O5" ) {
  type RecType = Conference
  override def convert( obj:DBObject, parent:MongoRecord ) = new Conference( obj, parent )

  "_id"       is DbMongoId         is 'id is 'client;
  "number"    is DbChar(15)        is 'client;
  "on"        is DbDateTime        ; // Date/Time it was purchased
//  "c"         is DbLink(Content)    as "Content";
  "m"         is DbArray(DbTid(B.User)) as "Members" is 'client;
}

class Conference( obj:DBObject, parent:MongoRecord ) extends MongoRecord( Conference.makeView, obj, parent ) {
}

object TurboBridge {
  val bridgeUrl = "https://api.turbobridge.com/Bridge"
}

case class TurboBridgeApp( account:String, email:String, password:String ) {
  def setBridgeUserParams( confId: String, userId:String ) = {
    Map(
      "outputFormat" -> "json",
      "authAccountEmail" -> email,
      "authAccountPassword" -> password,
      "authAccountPartnerID" -> "turbobridge",
      "authAccountAccountID" -> account,
      "requestType" -> "setBridgeUserID",
      "conferenceID" -> confId,
      "userID" -> userId      
      )
  }
  
  def setBridgeParams( confId: String ) = {
    Map(
      "outputFormat" -> "json",
      "authAccountEmail" -> email,
      "authAccountPassword" -> password,
      "authAccountPartnerID" -> "turbobridge",
      "authAccountAccountID" -> account,
      "requestType" -> "setBridge",
      "conferenceID" -> confId,
      "userIDPrompt" -> "1",
      "confEventsUrl" -> ( "https://" + ( B.PRODUCTION ? "rb.volerro.com" | "stage-s.volerro.com:8443" ) + "/turbobridge/event" ),
      "confMode" -> "qa"
      )
  }
                            
  def createConference( confId:String ) = {
    val json = Json.parse( "https://api.turbobridge.com/Bridge".POST( setBridgeParams( confId ), null, "text/json" ).s ).get(0)
    
    val authToken = json.get( "authToken" )
    val reqItem = json.get( "requestItem" )
    //val bridge = reqItem.
    
  }
}

object TurboBridgelet extends Weblet {

  def handle( web:WebContext ) {
    val s = Session()
    val u = s.user

    rpath match {

    // Events coming in from TurboBridge-- join conference, leave conference, etc
    case "/event" =>
      
    case "/start" =>

    case _ => _404
    }
  }
}

