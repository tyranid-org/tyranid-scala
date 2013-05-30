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

package org.tyranid.time

import java.text.{ DateFormat, DateFormatSymbols, SimpleDateFormat }
import java.util.{ Calendar, Date, TimeZone }

import org.tyranid.Imp._
import org.tyranid.session.Session


class DateFormatImp( df:DateFormat ) {

  def inUser = in( T.session.netTimeZone )
  def inUtc  = in( Time.Utc )

  def in( tz:TimeZone ) = {
    // maybe set up some simple DateFormat cache here ...

    val tzdf = df.clone.as[DateFormat]
    tzdf.setTimeZone( tz )
    tzdf
  }
}


class CalendarImp( c:Calendar ) {
  lazy val weekdays = new DateFormatSymbols().getWeekdays()
  
  def weekDayName   = Time.WeekDayNames( c.get( Calendar.DAY_OF_WEEK ) - 1 ).capitalize
  def monthName     = Time.MonthNames( c.get( Calendar.MONTH ) ).capitalize

  def isSunday    = c.get( Calendar.DAY_OF_WEEK ) == Calendar.SUNDAY
  def isMonday    = c.get( Calendar.DAY_OF_WEEK ) == Calendar.MONDAY
  def isTuesday   = c.get( Calendar.DAY_OF_WEEK ) == Calendar.TUESDAY
  def isWednesday = c.get( Calendar.DAY_OF_WEEK ) == Calendar.WEDNESDAY
  def isThursday  = c.get( Calendar.DAY_OF_WEEK ) == Calendar.THURSDAY
  def isFriday    = c.get( Calendar.DAY_OF_WEEK ) == Calendar.FRIDAY
  def isSaturday  = c.get( Calendar.DAY_OF_WEEK ) == Calendar.SATURDAY

  def year        = c.get( Calendar.YEAR )
  def month       = c.get( Calendar.MONTH )
  def dayOfMonth  = c.get( Calendar.DAY_OF_MONTH )
  def hour12      = c.get( Calendar.HOUR )
  def hour24      = c.get( Calendar.HOUR_OF_DAY )
  def ampm        = c.get( Calendar.AM_PM ) match {
                    case Calendar.AM => "am"
                    case Calendar.PM => "pm"
                    }
  def minute      = c.get( Calendar.MINUTE )
  def second      = c.get( Calendar.SECOND )


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
    c.set( Calendar.HOUR_OF_DAY,  other.get( Calendar.HOUR_OF_DAY ) )
    c.set( Calendar.MINUTE,       other.get( Calendar.MINUTE ) )
    c.set( Calendar.SECOND,       other.get( Calendar.SECOND ) )
    c.set( Calendar.MILLISECOND,  other.get( Calendar.MILLISECOND ) )
    c.setTimeZone(                other.getTimeZone )
  }

  def setBeginningOfMonth = {
    c.set( Calendar.DAY_OF_MONTH, 1 )
    setMidnight
    c
  }

  def setBeginningOfYear = {
    c.set( Calendar.MONTH,        0 )
    setBeginningOfMonth
    c
  }

  def setMidnight = {
    c.set( Calendar.HOUR_OF_DAY, 0 )
    c.set( Calendar.MINUTE,      0 )
    c.set( Calendar.SECOND,      0 )
    c.set( Calendar.MILLISECOND, 0 )
    c
  }

  def rollToNearestDayOfWeek( c:Calendar, dayOfWeek:Int, direction:Int ) {
    while ( c.get( Calendar.DAY_OF_WEEK ) != dayOfWeek )
      c.add( Calendar.DAY_OF_MONTH, direction )
  }

  def rollToNextDayOfWeek( dayOfWeek:Int, direction:Int ) {
    assert( direction == -1 || direction == 1 )

    do {
      c.add( Calendar.DAY_OF_MONTH, direction )
    } while ( c.get( Calendar.DAY_OF_WEEK ) != dayOfWeek )
  }

  def weekday = { weekdays( c.get( Calendar.DAY_OF_WEEK ) ) }
  
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

  def max( o:Date ) =
    if ( o.getTime > d.getTime )
      o
    else
      d

  def min( o:Date ) =
    if ( o.getTime < d.getTime )
      o
    else
      d

  def toCalendar( tz:TimeZone = null ) = {
    val c = ( tz == null ) ? Calendar.getInstance | Calendar.getInstance( tz )
    c.setTime( d )
    c
  }

  def toServerCalendar = toCalendar( B.serverTimeZone )

  def toUserCalendar = toCalendar( T.session.netTimeZone )
  def toUtcCalendar  = toCalendar( Time.Utc )

  def weekday = toUserCalendar.weekday

  def toDay = {
    val c = toUserCalendar
    c.set( Calendar.HOUR_OF_DAY, 0 )
    c.set( Calendar.MINUTE, 0 )
    c.set( Calendar.SECOND, 0 )
    c.set( Calendar.MILLISECOND, 0 )
    c.getTime()
  }
  
  def add( field:Int, amount:Int ) = {
    val c = toUtcCalendar
    c.add( field, amount )
    c.getTime
  }

  def localLastWeekRange = {
    val firstOfWeek = Calendar.getInstance
    firstOfWeek.setTime( d )
    firstOfWeek.setMidnight
    firstOfWeek.add( Calendar.DATE, -( firstOfWeek.get( Calendar.DAY_OF_WEEK ) - 1 ) )

    val firstOfLastWeek = Calendar.getInstance
    firstOfLastWeek.copyDateTimeFrom( firstOfWeek )
    firstOfLastWeek.add( Calendar.DATE, -7 )

    ( firstOfLastWeek.getTime, firstOfWeek.getTime )
  }

  def toServerDateStr =
    if ( d == null ) null
    else             Time.DateFormat.format( d )
  
  def toUtcDateStr =
    if ( d == null ) null
    else             Time.DateFormat.inUtc.format( d )
  
  def toUserDateStr =
    if ( d == null ) null
    else             Time.DateFormat.inUser.format( d )
  
  def toServerDateTimeStr =
    if ( d == null ) null
    else             Time.DateTimeFormat.format( d )

  def toUserDateTimeStr =
    if ( d == null ) null
    else             Time.DateTimeFormat.inUser.format( d )

  def toUserDisplay = {

    if ( d == null ) ""
    else {
      // TODO:  this should be configurable by Bootable based on the application's user interface and the user's local time conventions

      val cal = toUserCalendar
      val now = Time.createUserNowCalendar

      val sb = new StringBuilder

      sb ++= toUserMonthDayStr

      if ( now.year != cal.year )
        sb ++= ", " ++= cal.year.toString

      val hour = ( cal.hour12 == 0 ) ? 12 | cal.hour12
      sb ++= ", %d:%02d%s".format( hour, cal.minute, cal.ampm )

      sb.toString
    }
  }

  def toServerTime12Str =
    if ( d == null ) null
    else             Time.TimeFormat12.format( d )

  def toUserTime12Str =
    if ( d == null ) null
    else             Time.TimeFormat12.inUser.format( d )

  def toServerMonthDayStr =
    if ( d == null ) null
    else             Time.MonthDay.format( d )

  def toUserMonthDayStr =
    if ( d == null ) null
    else             Time.MonthDay.inUser.format( d )

  def toServerMonthDayTimeStr =
    if ( d == null ) null
    else             Time.MonthDay.format( d ) + ", " + Time.TimeFormat12.format( d ).toLowerCase 

  def toUserMonthDayTimeStr =
    if ( d == null ) null
    else             Time.MonthDay.inUser.format( d ) + ", " + Time.TimeFormat12.inUser.format( d ).toLowerCase 

  def toServerTime24Str =
    if ( d == null ) null
    else             Time.TimeFormat24.format( d )

  def toUserTime24Str =
    if ( d == null ) null
    else             Time.TimeFormat24.inUser.format( d )

  def toIso8601 = toUtcCalendar.format( iso8601 = true )

  def toAttr =
    if ( d == null ) null
    else             Time.AttrFormat.format( d )
    
  def toAttrGMT =
    if ( d == null ) null
    else             Time.AttrFormatGMT.format( d )
    
  def toRfc1123 =
    if ( d == null ) null
    else             Time.Rfc1123Format.format( d )
    
  def toRfc1123GMT =
    if ( d == null ) null
    else             Time.Rfc1123FormatGMT.format( d )
    
  def toAmzFormat = 
    if ( d == null ) null
    else             Time.AmzFormatGMT.format( d )
  
  def toCustomPointFormat = 
    if ( d == null ) null
    else             Time.CustomPointFormat.format( d )
    
  def toRfc822 =
    toRfc1123 // RFC 1123 is an update for RFC 822, might need to support RFC 822 explicitly though
    
  def daysUntil( other:Date ) = {
    val otherCal = other.toCalendar()
    val myCal = d.toCalendar()
      
    val endL = otherCal.getTimeInMillis + otherCal.getTimeZone.getOffset( otherCal.getTimeInMillis ) 
    val startL = myCal.getTimeInMillis + myCal.getTimeZone.getOffset( myCal.getTimeInMillis )
    ( ( endL - startL ) / Time.OneDayMs ).toInt      
  }
  
  def isWeekend = {
    val day = toCalendar().get( Calendar.DAY_OF_WEEK ) 
    ( day == Calendar.SUNDAY || day == Calendar.SATURDAY )  
  }

  def > ( other:Date ) = d.getTime >  other.getTime
  def >=( other:Date ) = d.getTime >= other.getTime
  def < ( other:Date ) = d.getTime <  other.getTime
  def <=( other:Date ) = d.getTime <= other.getTime

  def +( ms:Long ) = new Date( d.getTime + ms )
  def -( ms:Long ) = new Date( d.getTime - ms )
}

object Time {
  val currentYear = Calendar.getInstance.get( Calendar.YEAR )


  val DateFormat     = new SimpleDateFormat( "MM/dd/yyyy" )
  val DateTimeFormat = new SimpleDateFormat( "MM/dd/yyyy HH:mm:ss" )
  val TimeFormat24   = new SimpleDateFormat( "HH:mm" )
  val TimeFormat12   = new SimpleDateFormat( "hh:mma" )
  val MonthDay       = new SimpleDateFormat( "MMM dd" )
  val MonthDayYear   = new SimpleDateFormat( "MMM dd, yyyy" )
  
  val Rfc1123Format  = new SimpleDateFormat( "EEE, dd MMM yyyy HH:mm:ss z" )
  val Rfc1123FormatGMT  = {
    val f = new SimpleDateFormat( "EEE, dd MMM yyyy HH:mm:ss z" )
    f.setTimeZone( TimeZone.getTimeZone( "GMT" ) )
    f
  }
  
  val AttrFormat     = new SimpleDateFormat( "yyyy-MM-dd'T'HH:mmZ" )
  
  val AttrFormatGMT     = {
    val f   = new SimpleDateFormat( "yyyy-MM-dd'T'HH:mmZ" )
    f.setTimeZone( TimeZone.getTimeZone( "GMT" ) )
    f
  }
  
  val AmzFormatGMT   = {
    val f = new SimpleDateFormat( "yyyyMMdd'T'HHmmss'Z'" )
    f.setTimeZone( TimeZone.getTimeZone( "GMT" ) )
    f
  }
  
  val CustomPointFormat = new SimpleDateFormat( "yyyy-MM-dd'T'HH:mm:ss.'GMT'Z" )

  val OneMinuteMs:Long    =                 60 * 1000
  val FiveMinutesMs:Long  =             5 * 60 * 1000
  val HalfHourMs:Long     =            30 * 60 * 1000
  val OneHourMs:Long      =            60 * 60 * 1000
  val OneDayMs:Long       =       24 * 60 * 60 * 1000
  val OneWeekMs:Long      =   7 * 24 * 60 * 60 * 1000
  val OneYearMs:Long      = 365 * 24 * 60 * 60 * 1000
  
  val MonthNames     = Array( "january", "february", "march", "april", "may", "june",
                              "july", "august", "september", "october", "november", "december" )
  val WeekDayNames   = Array( "sunday", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday" )
  val AmPmNames      = Array( "am", "pm" )
  val FillerWords    = Array( "on", "at" )

  def fourDigitYear( year:Int, len:Int ) =
    if ( len == 4 || year >= 100 ) year
    else if ( year < 50 )          2000 + year
    else                           1900 + year

  def createNullCalendar = {
    val c = Calendar.getInstance( Utc )
    c.set( 0, 0, 0, 0, 0, 0 )
    c
  }

  def now = System.currentTimeMillis
  
  def next( hour:Int, minute:Int, days:Int = 0 ) = {
	  val now = Calendar.getInstance
	  val cal = Calendar.getInstance

	  cal.set( Calendar.HOUR_OF_DAY, hour )
	  cal.set( Calendar.MINUTE, minute )

	  if ( now.after( cal ) ) cal.add( Calendar.DATE, 1 )
	  if ( days != 0 ) cal.add( Calendar.DAY_OF_YEAR, days )
	  
    cal.getTime
  }

  def createUserNowCalendar:Calendar = Calendar.getInstance( T.session.netTimeZone )
  def createUtcNowCalendar:Calendar  = Calendar.getInstance

  val Utc = TimeZone.getTimeZone( "UTC" )

  def toLaxTimeZone( s:String ) = {
    val su = s.toUpperCase
    TimeZone.getTimeZone(
      // see http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4135882
      if ( "AKST".equals( s ) ) "AST"
      else                      su )
  }

  def briefDate( date:Date ):String = {

    if ( date == null )
      return "n/a"

    val tz = T.session.netTimeZone
    val c = date.toCalendar( tz ) 
    val nc = new Date().toCalendar( tz )

    "%s %d%s".format(
      c.monthName.substring( 0, 3 ), c.get( Calendar.DAY_OF_MONTH ),
      if ( c.isSameYearAs( nc ) ) ""
      else                        ", " + c.get( Calendar.YEAR ) )
  }
  
  def duration( now:Date, date:Date, brief:Boolean = false ):String = {

    def display( s:String ) =
      if ( brief ) s.substring( 0, 3 )
      else         s

    if ( date == null )
      return "n/a"

    var effNow = now

    // if now is not specified, always use a non-relative date
    if ( effNow == null )
      effNow = new Date + OneYearMs

    val rawSince = effNow.getTime - date.getTime

    val tz = T.session.netTimeZone
    val nc = effNow.toCalendar( tz )
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

      "%s%s%d:%02d%s".format(
        if ( days == 0 ) {
          ""
        } else if ( days == 1 ) {
          future ? "tomorrow" | "yesterday"
        } else if ( sameWeek ) {
          display( c.weekDayName )
        } else {
          ( future ? "next " | "last " ) + display( c.weekDayName )
        },
        brief ? ", " | " at ",
        c.get( Calendar.HOUR ) match { case 0 => 12 case i => i },
        c.get( Calendar.MINUTE ),
        c.get( Calendar.AM_PM ) match { case 0 => "am" case 1 => "pm" } )
    } else {
      val sb = new StringBuilder
      sb ++= "%s%s %d%s%s%d:%02d".format(
        !brief |* display( c.weekDayName ) + ", ",
        display( c.monthName ), c.get( Calendar.DAY_OF_MONTH ),
        if ( c.isSameYearAs( nc ) ) ""
        else                        ", " + c.get( Calendar.YEAR ),
        brief ? ", " | " at ",
        c.get( Calendar.HOUR ) match { case 0 => 12 case i => i },
        c.get( Calendar.MINUTE ) )

      //val seconds = c.get( Calendar.SECOND )
      //val milli   = c.get( Calendar.MILLISECOND )
      //if ( seconds != 0 || milli != 0 ) {
        //sb ++= ":%02d".format( seconds )

        //if ( milli != 0 )
          //sb ++= ".%03d".format( milli )
      //}
      sb ++= ( c.get( Calendar.AM_PM ) match { case 0 => "am" case 1 => "pm" } )

      sb.toString
    }
  }
}

