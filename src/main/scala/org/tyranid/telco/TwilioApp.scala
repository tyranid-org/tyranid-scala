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
/*
import com.twilio.sdk.{ TwilioRestClient, TwilioRestException }
import com.twilio.sdk.resource.factory.SmsFactory
import com.twilio.sdk.resource.instance.Sms
import com.twilio.sdk.resource.list.SmsList
import com.twilio.sdk.client.TwilioCapability
*/

import org.tyranid.Imp._
import org.tyranid.db.{ DbChar, DbDateTime }
import org.tyranid.db.mongo._
import org.tyranid.json.JsModel
import org.tyranid.session.Session
import org.tyranid.web.{ WebContext, Weblet }


object ConferenceNumber extends MongoEntity( "a0O5" ) {
  type RecType = ConferenceNumber
  override def convert( obj:DBObject, parent:MongoRecord ) = new ConferenceNumber( obj, parent )

  "_id"       is DbMongoId         is 'id is 'client;
  "number"    is DbChar(15)        is 'client;
  "on"        is DbDateTime        ; // Date/Time it was purchased
}

class ConferenceNumber( obj:DBObject, parent:MongoRecord ) extends MongoRecord( ConferenceNumber.makeView, obj, parent ) {
}

object Twilio {
  val baseUrl = "https://api.twilio.com/2010-04-01"
}

case class TwilioApp( sid:String, auth:String ) {
  /*
  val client = new TwilioRestClient( sid, auth )

  def createConference( name:String ) = {
    val capability = new TwilioCapability( sid, auth )
    capability.allowClientIncoming( name )
    capability.generateToken()
  }

  /*
   * https://www.twilio.com/docs/api/rest/available-phone-numbers
   */
  def provisionNumber:String = {
    // Build a filter for the AvailablePhoneNumberList
    val numbers = client.getAccount().getAvailablePhoneNumbers( new HashMap[String,String](), "US", "Local" )
    //val numbers = client.getAccount().getAvailablePhoneNumbers( new HashMap[String,String](), "US", "TollFree" )
    val list = numbers.getPageData()

    if ( list.size > 0 ) {
      // Purchase the first number in the list.
      val purchaseParams = new java.util.ArrayList[org.apache.http.NameValuePair]()
      val number = list.get(0).getPhoneNumber()
      purchaseParams.add( new BasicNameValuePair( "PhoneNumber", number ) )
      client.getAccount().getIncomingPhoneNumberFactory().create( purchaseParams )
      number
    } else {
      provisionNumber
    }
  }
  */
}

object Twiliolet extends Weblet {

  def handle( web:WebContext ) {
    val s = Session()
    val u = s.user

    rpath match {
    case "/start" =>
      //val token = B.twilio.createConference( web.s( 'name ) )

      //web.jsRes( JsModel( Map( "token" -> token ) ) )

    case "/app" =>
      // Direct from twilio

    case _ => _404
    }
  }
}

