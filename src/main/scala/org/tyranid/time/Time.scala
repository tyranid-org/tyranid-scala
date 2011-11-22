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
import org.tyranid.session.Session


class CalendarImp( c:Calendar ) {

  def dayOfWeekName = Time.WeekDayNames( c.get( Calendar.DAY_OF_WEEK ) - 1 ).capitalize
  def monthName     = Time.MonthNames( c.get( Calendar.MONTH ) ).capitalize

  def isSameYearAs( other:Calendar ) =
    c.get( Calendar.YEAR )         == other.get( Calendar.YEAR )

  def isSameWeekAs( other:Calendar ) =
    c.get( Calendar.WEEK_OF_YEAR ) == other.get( Calendar.WEEK_OF_YEAR )

  def isSameDayAs( other:Calendar ) =
    c.get( Calendar.YEAR )         == other.get( Calendar.YEAR ) &&
    c.get( Calendar.MONTH )        == other.get( Calendar.MONTH ) &&
    c.get( Calendar.DAY_OF_MONTH ) == other.get( Calendar.DAY_OF_MONTH )

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

  def format( iso8601:Boolean = false ) = {
    val sb = new StringBuilder
    sb ++= "%04d-%02d-%02d%s%02d:%02d".format(
      c.get( Calendar.YEAR ), c.get( Calendar.MONTH ) + 1, c.get( Calendar.DAY_OF_MONTH ),
      if ( iso8601 ) "T" else " ",
      c.get( Calendar.HOUR_OF_DAY ), c.get( Calendar.MINUTE ) )
    val seconds = c.get( Calendar.SECOND )
    val milli   = c.get( Calendar.MILLISECOND )
    if ( seconds != 0 || milli != 0 ) {
      sb ++= ":%02d".format( seconds )

      if ( milli != 0 )
        sb ++= ".%03d".format( milli )
    }

    val tz = c.getTimeZone
    if ( iso8601 ) {
      var offset = tz.getOffset( c.getTime.getTime )
      if ( offset == 0 ) {
        sb += 'Z'
      } else {
        offset /= 60000
        val minutes = offset % 60
        val hours   = offset / 60
        sb ++= "%+03d%02d".format( hours, minutes )
      }
    } else {
      sb += ' ' ++= tz.getDisplayName( tz.inDaylightTime( c.getTime ), TimeZone.SHORT )
    }

    sb.toString
  }

  def toDisplay = format( iso8601 = false )
  def toIso8601 = format( iso8601 = true )
}

class DateImp( d:Date ) {

  def toCalendar( tz:TimeZone ) = {
    val c = Calendar.getInstance( tz )
    c.setTime( d )
    c
  }

  def toUserCalendar = toCalendar( Session().user.timeZone )
  def toUtcCalendar  = toCalendar( Time.Utc )

  def add( field:Int, amount:Int ) = {
    val c = toUtcCalendar
    c.add( field, amount )
    c.getTime
  }
}

object Time {

  val OneMinuteMs    =               60 * 1000
  val FiveMinutesMs  =           5 * 60 * 1000
  val HalfHourMs     =          30 * 60 * 1000
  val OneHourMs      =          60 * 60 * 1000
  val OneDayMs       =     24 * 60 * 60 * 1000
  val OneWeekMs      = 7 * 24 * 60 * 60 * 1000
  
	val MonthNames     = Array( "january", "february", "march", "april", "may", "june",
		                          "july", "august", "september", "october", "november", "december" )
	val WeekDayNames   = Array( "sunday", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday" )
	val AmPmNames      = Array( "am", "pm" )
	val FillerWords    = Array( "on", "at" )
	val RelativeWords  = Array( "tomorrow", "yesterday", "today", "next", "this", "last", "now" )

  def fourDigitYear( year:Int, len:Int ) =
    if ( len == 4 || year >= 100 ) year
    else if ( year < 50 )          2000 + year
    else                           1900 + year

  def createNullCalendar = {
    val c = Calendar.getInstance( Utc )
    c.set( 0, 0, 0, 0, 0, 0 )
    c
  }

  def createUserNowCalendar:Calendar = Calendar.getInstance( Session().user.timeZone )

  val Utc = TimeZone.getTimeZone( "UTC" )

  def toLaxTimeZone( s:String ) = {
    val su = s.toUpperCase
    TimeZone.getTimeZone(
      // see http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4135882
      if ( "AKST".equals( s ) ) "AST"
      else                      su )
  }

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

  def duration( now:Date, date:Date ):String = {

    val rawSince = now.getTime - date.getTime

    val tz = Session().user.timeZone
    val nc = now.toCalendar( tz )
    val c = date.toCalendar( tz ) 

    val ( since, future ) =
      if ( rawSince < 0 ) ( rawSince * -1, true  )
      else                ( rawSince,      false )

    if ( since < 1 * Time.OneMinuteMs ) {
      "now"
    } else if ( since < 5 * Time.OneMinuteMs ) {
      if ( future ) "in a few minutes"
      else          "a few minutes ago"
    } else if ( since < 50 * Time.OneMinuteMs ) {
      if ( future ) "in %d minutes".format( since / Time.OneMinuteMs )
      else          "%d minutes ago".format( since / Time.OneMinuteMs )
    } else if ( since < 70 * Time.OneMinuteMs ) {
      if ( future ) "in an hour"
      else          "an hour ago"
    } else if ( since < 12 * Time.OneHourMs ) {
      if ( future ) "in %d hours".format( ( since.toDouble / Time.OneHourMs ).round )
      else          "%d hours ago".format( ( since.toDouble / Time.OneHourMs ).round )
    } else if ( since < Time.OneWeekMs ) {
      var days = 0
      val sameWeek = c.isSameWeekAs( nc )

      while ( !c.isSameDayAs( nc ) ) {
        nc.add( Calendar.DATE, if ( future ) 1 else -1 )
        days += 1
      }

      "%s at %d:%02d%s".format(
        if ( days == 0 ) {
          ""
        } else if ( days == 1 ) {
          future ? "tomorrow" | "yesterday"
        } else if ( sameWeek ) {
          c.dayOfWeekName
        } else {
          ( future ? "next " | "last " ) + c.dayOfWeekName
        },
        c.get( Calendar.HOUR ) match { case 0 => 12 case i => i },
        c.get( Calendar.MINUTE ),
        c.get( Calendar.AM_PM ) match { case 0 => "am" case 1 => "pm" } )
    } else {
      val sb = new StringBuilder
      sb ++= "%s, %s %d%s%s%d:%02d%s".format(
        c.dayOfWeekName,
        c.monthName, c.get( Calendar.DAY_OF_MONTH ),
        if ( c.isSameYearAs( nc ) ) ""
        else                        ", " + c.get( Calendar.YEAR ),
        " at ",
        c.get( Calendar.HOUR ) match { case 0 => 12 case i => i },
        c.get( Calendar.MINUTE ),
        c.get( Calendar.AM_PM ) match { case 0 => "am" case 1 => "pm" } )

      val seconds = c.get( Calendar.SECOND )
      val milli   = c.get( Calendar.MILLISECOND )
      if ( seconds != 0 || milli != 0 ) {
        sb ++= ":%02d".format( seconds )

        if ( milli != 0 )
          sb ++= ".%03d".format( milli )
      }

      sb.toString
    }
  }
}

