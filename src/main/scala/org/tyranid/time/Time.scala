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

import java.util.{ Calendar, Date, TimeZone }

import org.tyranid.Imp._


class CalendarImp( c:Calendar ) {

  def copyDateFrom( other:Calendar ) {
    c.set( Calendar.YEAR,         other.get( Calendar.YEAR ) )
    c.set( Calendar.MONTH,        other.get( Calendar.MONTH ) )
    c.set( Calendar.DAY_OF_MONTH, other.get( Calendar.DAY_OF_MONTH ) )
  }

  def copyDateTimeFrom( other:Calendar ) {
    copyDateFrom( other )
    c.set( Calendar.HOUR,         other.get( Calendar.HOUR ) )
    c.set( Calendar.MINUTE,       other.get( Calendar.MINUTE ) )
    c.set( Calendar.SECOND,       other.get( Calendar.SECOND ) )
    c.set( Calendar.MILLISECOND,  other.get( Calendar.MILLISECOND ) )
    c.setTimeZone(                other.getTimeZone )
  }

  def rollToDayOfWeek( dayOfWeek:Int, direction:Int ) {
    assert( direction == -1 || direction == 1 )

    do {
      c.add( Calendar.DAY_OF_MONTH, direction )
    } while ( c.get( Calendar.DAY_OF_WEEK ) != dayOfWeek )
  }

  def format( iso:Boolean = false ) = {
    val sb = new StringBuilder
    sb ++= "%04d-%02d-%02d%s%02d:%02d".format(
      c.get( Calendar.YEAR ), c.get( Calendar.MONTH ) + 1, c.get( Calendar.DAY_OF_MONTH ),
      if ( iso ) "Z" else " ",
      c.get( Calendar.HOUR_OF_DAY ), c.get( Calendar.MINUTE ) )
    val seconds = c.get( Calendar.SECOND )
    val milli   = c.get( Calendar.MILLISECOND )
    if ( seconds != 0 || milli != 0 ) {
      sb ++= ":%02d".format( seconds )

      if ( milli != 0 )
        sb ++= ".%03d".format( milli )
    }

    val tz = c.getTimeZone
    sb += ' ' ++= tz.getDisplayName( tz.inDaylightTime( c.getTime ), TimeZone.SHORT )
    sb.toString
  }

  def toDisplay = format( iso = false )
  def toIso8601 = format( iso = true )
}

class DateImp( d:Date ) {
}

object Time {

  def fourDigitYear( year:Int, len:Int ) =
    if ( len == 4 || year >= 100 ) year
    else if ( year < 50 )          2000 + year
    else                           1900 + year

  def createNullCalendar = {
    val c = Calendar.getInstance
    c.set( 0, 0, 0, 0, 0, 0 )
    c.setTimeZone( Utc )
    c
  }

  val Utc = TimeZone.getTimeZone( "UTC" )

  def toLaxTimeZone( s:String ) = {
    val su = s.toUpperCase
    TimeZone.getTimeZone(
      // see http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4135882
      if ( "AKST".equals( s ) ) "AST"
      else                      su )
  }

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

