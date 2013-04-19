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

package org.tyranid.share

import java.util.regex.Pattern

import scala.xml.{ Unparsed, NodeSeq }
import java.io.{ File, FileOutputStream }
import com.mongodb.DBObject
import org.tyranid.Imp._
import org.tyranid.db.mongo.Imp._
import org.tyranid.json.Json
import org.tyranid.http.Http
import org.tyranid.math.Base64
import org.tyranid.session.Session
import org.tyranid.document.DocApp
import org.tyranid.time.Time
import org.tyranid.ui.Form
import java.io.FileInputStream


object JoinMeApi {
  val ApiUrl = "https://secure.join.me/API/"
/*
OK
Log in was successful.
ERROR: 1; Missing arguments
An email address or password was not provided.
ERROR: 2; Internal error
An unexpected error occurred. Try again later.
ERROR: 3; Account locked out
The user account has been locked out. An email is sent to the account holder with the instructions to unlock the account.
ERROR: 4; Invalid email or password
An authentication code could not be generated due to at least one invalid input value.
ERROR: 5; Invalid authcode
An incorrect authentication code was provided.
ERROR: 6; Too often called
The method was called more than three times in three minutes. 
*/  
  def login( email:String, password:String ) = {
    Http.GET( ApiUrl + "login?email=" + email + "&password=" + password ).s match {
      case err if err.startsWith( "ERROR" ) =>
        err
      case ok =>
        ok
    }
  }
/*
OK
An authentication code was successfully generated.
ERROR: 2; Internal error
An unexpected error occurred. Try again later.
ERROR: 3; Account locked out
The join.me account has been locked out. An email is sent to the account holder with the instructions to unlock the account.
ERROR: 4; Invalid email or password
An authentication code could not be generated due to at least one invalid input value.
ERROR: 5; Invalid authcode
An incorrect authentication code was provided.
ERROR: 6; Too often called
The method was called more than three times in three minutes.
*/
  def authCode( email:String, password:String ) = {
    Http.GET( ApiUrl + "requestAuthCode?email=" + email + "&password=" + password ).s match {
      case err if err.startsWith( "ERROR" ) =>
        err
      case s =>
        val authIdx = s.indexOf( "AUTHCODE:" )
       
        if ( authIdx > -1 )
          s.substring( authIdx + 10 )
        else 
          s
    }
  }
  
/*
OK
Generating an authentication code succeeded.
ERROR: 1; Missing arguments
An email address or password was not provided.
ERROR: 2; Internal error
An unexpected error occurred. Try again later.
ERROR: 3; Account locked out
The join.me account has been locked out. An email is sent to the account holder with the instructions to unlock the account.
ERROR: 4; Invalid email or password
Requesting a meeting code failed due to at least one of the input values was incorrect.
ERROR: 5; Invalid authcode
An incorrect authentication code was provided.
ERROR: 6; Too often called
The method was called more than three times in three minutes.
*/
  
  val patternStr = """.*?code\: (\d+)\s+ticket\: (\d+)"""
  val pattern = Pattern.compile( patternStr, Pattern.CASE_INSENSITIVE | Pattern.DOTALL | Pattern.MULTILINE);

  def meetingCode( authCode:String ) = {
    Http.GET( ApiUrl + "requestCode?authCode=" + authCode ).s match {
      case err if err.startsWith( "ERROR" ) =>
        (err,err)

      case s =>
        val matcher = pattern.matcher( s )
        if ( matcher.matches() ) {
          val code   = matcher.group( 1 )
          val ticket = matcher.group( 2 )

          (code,ticket)
        } else {
          ( "code not found", null )
        }
    }
  }
  
  def downloadUrl( meetingCode:String, ticket:String, webdownload:Boolean = true ) = {
    "https://secure.join.me/download.aspx?code=" + meetingCode + "&ticket=" + ticket + ( webdownload ? "&webdownload=true" | "" )
  }
}
