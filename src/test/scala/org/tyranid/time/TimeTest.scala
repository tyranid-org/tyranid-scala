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

import org.scalatest.FunSuite

import org.tyranid.Imp._
import org.tyranid.session.Session


class TimeSuite extends FunSuite {
  org.tyranid.test.TestBoot.boot

  test( "timeZoneParsing" ) {

    assert( Time.toLaxTimeZone( "cst" ) == TimeZone.getTimeZone( "CST" ) )
  }

  test( "timeParser" ) {

    val data = Array[String](
      "2008-01-02T02Z",               "2008-01-02 02:00 UTC",
      "2008-01-04T02:30Z",            "2008-01-04 02:30 UTC",
      "2008-01-02T02:30:59Z",         "2008-01-02 02:30:59 UTC",
      "2008-01-02T02:30:59.042Z",     "2008-01-02 02:30:59.042 UTC",
      "2008-01-02T02:30:59.042",      "because an invalid ISO 8601 format was found -- it was missing a 'Z' at the end.",
      "Sep-10-2015",                  "2015-09-10 00:00 UTC",
      "Aug/10/2015",                  "2015-08-10 00:00 UTC",
      "2008-01-02",                   "2008-01-02 00:00 UTC",
      "2008 - 1 - 2 10: 30",          "2008-01-02 10:30 UTC",
      "Jan 2, 2008",                  "2008-01-02 00:00 UTC",
      "january 2, 2008",              "2008-01-02 00:00 UTC",
      "4/20/71",                      "1971-04-20 00:00 UTC",
      "tues, 4/20/1971",              "1971-04-20 00:00 UTC",
      "wednesday jan 3, 2008",        "because Wednesday was specified but that date is on a Thursday.",
      "thu, jan 3, 2008",             "2008-01-03 00:00 UTC",
      "thu, jan 3, 2008 at 10:30p",   "2008-01-03 22:30 UTC",
      "thu, jan 3, 2008 at 10:p",     "because \":p\" was confusing.",
      "today at 2 pm",                "2008-06-21 14:00 CDT",
      "tomorrow 2 pm",                "2008-06-22 14:00 CDT",
      "2 pm tomorrow",                "2008-06-22 14:00 CDT",
      "tom 2p",                       "2008-06-22 14:00 CDT",
      "yesterday, 3pm",               "2008-06-20 15:00 CDT",
      "this saturday at 5pm",         "because \"this\" is confusing, try \"last\" or \"next\".",
      "5pm last",                     "because \"last\" did not have a day of the week after it.",
      "next sat at 5pm",              "2008-06-28 17:00 CDT",
      "5:30p on last fri",            "2008-06-20 17:30 CDT",
      "5:30p",                        "because no date could be found.",
      "today on 5/1/1990",            "because duplicate date information was found at \"5/1/1990\".",
      "5:30p at 5:30p",               "because duplicate time information was found at \"5:30p\".",
      "now",                          "2008-06-21 12:00 CDT",
      "sat, june 21, 2008",           "2008-06-21 00:00 UTC",
      "sat, june 21, 08",             "2008-06-21 00:00 UTC",
      "wed, june 21, 89",             "1989-06-21 00:00 UTC",
      "2008-jan-02",                  "2008-01-02 00:00 UTC",
      "20081103",                     "2008-11-03 00:00 UTC",
      "20081103 1203",                "2008-11-03 12:03 UTC",
      "20081103 120300",              "2008-11-03 12:03 UTC",
      "2008-11-03 120300",            "2008-11-03 12:03 UTC",
      "2008.11.03 120300",            "2008-11-03 12:03 UTC",
      "5pm 20081103",                 "2008-11-03 17:00 UTC",
      "5pm on 20081103",              "2008-11-03 17:00 UTC",
      "5pm on 20081103 cst",          "2008-11-03 17:00 CST",
      "5pm cst on 20081103",          "2008-11-03 17:00 CST",
      "20081103 140300a",             "because a 24 hour time was given while an am/pm (\"a\") was found.",
      "20081103 12:03:00a",           "2008-11-03 00:03 UTC",
      "20081103 12:03:00 a.m.",       "2008-11-03 00:03 UTC",
      "20081103 12:03:00 A. M.",      "2008-11-03 00:03 UTC",
      "20081103 14:03:00 a.m.",       "because a 24 hour time was given while an am/pm (\"a.m.\") was found.",
      "20081103 120300.930 EST",      "2008-11-03 12:03:00.930 EST",
      "2008-01-02 12:00",             "2008-01-02 12:00 UTC",
      "2008-01-02 12:00a",            "2008-01-02 00:00 UTC",
      "2008-01-02 12:00 pm",          "2008-01-02 12:00 UTC",
      "2008-01-02 12:00 CST",         "2008-01-02 12:00 CST",
      "2008-01-02 12:00:02",          "2008-01-02 12:00:02 UTC",
      "2008-01-02 12:00:02 CST",      "2008-01-02 12:00:02 CST",
      "2008-01-02 12:00:02.983",      "2008-01-02 12:00:02.983 UTC",
      "2008-01-02 12:00:02.983 PST",  "2008-01-02 12:00:02.983 PST",
      "2008-01-02 12:00 CST",         "2008-01-02 12:00 CST"
    )

    val tp = new TimeParser
    val savedTz = Session().user.timeZone
    Session().user.timeZone = TimeZone.getTimeZone( "CST" )

    // set a fixed now for regression testing purposes
    tp.now = {
      val c = Calendar.getInstance
      c.setTimeZone( Time.Utc )
      c.set( 2008, 5, 21, 12, 0, 0 )
      c.set( Calendar.MILLISECOND, 0 )
      c
    }
   
    for ( i <- 0 until data.length by 2 ) {
      val time     = data( i )
      val expected = data( i+1 )
   
      try {
        val actual = tp.parse( time ).toDisplay
   
        if ( !expected.equals( actual ) ) {
          println( "\n Parsing: " + time )
          println(   "  Tokens: " + tp.tokenString )
          println(   "  Actual: " + actual )
          println(   "Expected: " + expected )
          assert( expected === actual )
        }
      } catch {
      case ex:java.text.ParseException =>
        val msg = ex.getMessage
        val because = msg.substring( msg.indexOf( "because" ) )
        if ( !expected.equals( because ) ) {
          println( "\n Parsing: " + time )
          println(   "  Tokens: " + tp.tokenString )
          println(   " Problem: " + msg )
          assert( expected === because )
        }
      }
    }

    Session().user.timeZone = savedTz
  }

  test( "stringParsing" ) {

    assert( "Sep-10-2015".toLaxDate === "09/10/2015".toLaxDate )
    assert( "Aug/10/2015".toLaxDate === "08-10-2015".toLaxDate )
    assert( "June-05-2015".toLaxDate === "Jun-05-2015".toLaxDate )
    assert( "June-5-2015".toLaxDate === "Jun-05-2015".toLaxDate )

    //"%u2122".decUrl

    //assert( re.base36  === Base36.toString( re.decimal ) )
  }

  test( "durationText" ) {
    val data = Array[String](
      "2008-01-02 CST",                   "Wednesday, January 2, 2008 at 12:00am",
      "2020-01-10 CST",                   "now",
      "2020-01-10 00:02 CST",             "in a few minutes",
      "2020-01-09 23:58 CST",             "a few minutes ago",
      "2020-01-10 00:32 CST",             "in 32 minutes",
      "2020-01-09 23:20 CST",             "40 minutes ago",
      "2020-01-09 23:05 CST",             "an hour ago",
      "2020-01-10 01:09 CST",             "in an hour",
      "2020-01-09 18:05 CST",             "6 hours ago",
      "2020-01-10 08:09 CST",             "in 8 hours",
      "2020-01-11 08:09 CST",             "tomorrow at 8:09am",
      "2020-01-12 08:09 CST",             "next Sunday at 8:09am",
      "2020-01-09 08:09 CST",             "yesterday at 8:09am",
      "2020-01-08 08:09 CST",             "Wednesday at 8:09am",
      "2020-01-01 08:09 CST",             "Wednesday, January 1 at 8:09am",
      "2019-01-01 08:09 CST",             "Tuesday, January 1, 2019 at 8:09am"
    )

    val now = {
      val c = Calendar.getInstance
      c.setTimeZone( TimeZone.getTimeZone( "CST" ) )
      c.set( 2020, 0, 10, 0, 0, 0 )
      c.set( Calendar.MILLISECOND, 0 )
      c.getTime
    }

    val tp = new TimeParser
    val savedTz = Session().user.timeZone
    Session().user.timeZone = TimeZone.getTimeZone( "CST" )

    for ( i <- 0 until data.length by 2 ) {
      val time     = data( i )
      val expected = data( i+1 )

      assert( Time.duration( now = now, date = tp.parse( time ).getTime ) === expected )
    }

    Session().user.timeZone = savedTz
  }

  test( "iso8601" ) {
    val data = Array[String](
      "2008-01-02 2:30 UTC",  "2008-01-02T02:30Z",
      "2008-01-02 CST",       "2008-01-02T00:00-0600",
      "2008-01-02 CET",       "2008-01-02T00:00+0100"
    )

    for ( i <- 0 until data.length by 2 ) {
      val time     = data( i )
      val expected = data( i+1 )

      assert( time.parseCalendar().toIso8601 === expected )
    }
  }

  test( "dateArithmetic" ) {
    assert( "2010-1-1 5:00a".parseDate().add( Calendar.MINUTE, 5 ).toUtcCalendar.toIso8601 === "2010-01-01T05:05Z" )
    assert( "2010-1-1 5:00a".parseDate().add( Calendar.DATE,   2 ).toUtcCalendar.toIso8601 === "2010-01-03T05:00Z" )
    assert( "2010-1-1 5:00a".parseDate().add( Calendar.DATE,  -1 ).toUtcCalendar.toIso8601 === "2009-12-31T05:00Z" )
  }
}

