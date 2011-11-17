/**
 * Copyright (c) 2008-2011 Tyranid <http://tyranid.org>
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

package org.tyranid.time

import java.util.{ Date }

object Time {

  val OneMinuteMs   =           60 * 1000
  val FiveMinutesMs =       5 * 60 * 1000
  val HalfHourMs    =      30 * 60 * 1000
  val OneDayMs      = 24 * 60 * 60 * 1000
  
  val datep1 = """(\d\d)/(\d\d?)/(\d\d\d\d)""".r.pattern
  val DateFormat = new java.text.SimpleDateFormat( "MM/dd/yyyy" )

  val datep2 = """(\d\d)-(\d\d?)-(\d\d\d\d)""".r.pattern
  val DateFormat2 = new java.text.SimpleDateFormat( "MM-dd-yyyy" )

  val datep3 = """(\w\w\w)/(\d\d?)/(\d\d\d\d)""".r.pattern
  val DateFormat3 = new java.text.SimpleDateFormat( "MMM/dd/yyyy" )

  val datep4 = """(\w\w\w)-(\d\d?)-(\d\d\d\d)""".r.pattern
  val DateFormat4 = new java.text.SimpleDateFormat( "MMM-dd-yyyy" )

  val datep5 = """(\w\w\w\w+)-(\d\d?)-(\d\d\d\d)""".r.pattern
  val DateFormat5 = new java.text.SimpleDateFormat( "MMMM-dd-yyyy" )

  val DateTimeFormat = new java.text.SimpleDateFormat( "MM/dd/yyyy HH:mm:ss" )


  
  def isDate( s:String ) =
    datep1.matcher( s ).matches || datep2.matcher( s ).matches ||
    datep3.matcher( s ).matches || datep4.matcher( s ).matches ||
    datep5.matcher( s ).matches

  def parse( s:String ):Date = {
    try {
      if ( datep1.matcher( s ).matches )
        DateFormat.parse( s )
      else if ( datep2.matcher( s ).matches )
        DateFormat2.parse( s )
      else if ( datep3.matcher( s ).matches )
        DateFormat3.parse( s )
      else if ( datep4.matcher( s ).matches )
        DateFormat4.parse( s )
      else if ( datep5.matcher( s ).matches )
        DateFormat5.parse( s )
      else
        null
        
      // TODO:  use time parser

    } catch {
    case e =>
      println( "Couldn't parse " + s + " as a date." )
      e.printStackTrace
      null
    }
  }

  def toDateStr( d:Date ) = {
    if ( d == null )
      null
    else
      DateFormat.format( d );
  }
  
  def toDateTimeStr( d:Date ) = {
    if ( d == null )
      null
    else
      DateTimeFormat.format( d );
  }
}

