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
import com.twilio.sdk.{ TwilioRestClient, TwilioRestException}
import com.twilio.sdk.resource.factory.SmsFactory
import com.twilio.sdk.resource.instance.Sms
import com.twilio.sdk.resource.list.SmsList
import com.twilio.sdk.client.TwilioCapability
//import java.util.HashMap;
//import java.util.Map;

object Twilio {
  val baseUrl = "https://api.twilio.com/2010-04-01"
  
}

case class TwilioApp( sid:String, auth:String ) {
  //val client = new TwilioRestClient( sid, auth )
  
  def createConference = {
    val capability = new TwilioCapability( sid, auth )
    capability.allowClientIncoming( "someone" )
    capability.generateToken()
  }
}