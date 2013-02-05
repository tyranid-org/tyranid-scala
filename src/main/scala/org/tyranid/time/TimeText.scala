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

import java.text.ParseException
import java.util.{ Calendar, Date, TimeZone }

import org.tyranid.Imp._



sealed trait Type

object Type {
  // raw types
  case object INT extends Type
  case object CHAR extends Type
  case object STRING extends Type
  // evaluated types
  case object MONTH extends Type
  case object DAY_OF_WEEK extends Type
  case object AMPM extends Type
  case object TIMEZONE extends Type
  case object FILLER extends Type
  case object USER_RELATIVE extends Type
}
		                
private class Token {
  var t:Type = _
  var pos:Int = _
  var len:Int = _
  var i:Int = _
  var s:String = _
  var ch:Char = _
		
  def endPos = pos + len
		
  def comp( c:Comp ):Boolean =
    c match {
    case Comp.SEP          => t == Type.CHAR
    case Comp.DASH         => t == Type.CHAR && ch == '-'
    case Comp.SLASH        => t == Type.CHAR && ch == '/'
    case Comp.PERIOD       => t == Type.CHAR && ch == '.'
    case Comp.COLON        => t == Type.CHAR && ch == ':'
    case Comp.COMMA        => t == Type.CHAR && ch == ','
    case Comp.YEAR         => t == Type.INT && len <= 4
    case Comp.MONTH        => month
    case Comp.DAY_OF_WEEK  => dayOfWeek
    case Comp.DAY_OF_MONTH => t == Type.INT && i >= 1 && i <= 31
    case Comp.HOUR         => t == Type.INT && i < 24
    case Comp.MINUTE       => t == Type.INT && i < 60
    case Comp.SECOND       => t == Type.INT && i < 60
    case Comp.MILLI        => t == Type.INT && i < 1000
    case Comp.AMPM         => ampm
    case Comp.TIMEZONE     => timeZone
    case _                 => throw new RuntimeException
    }
		
  def sep = t == Type.CHAR
  def sep( ch:Char ) = t == Type.CHAR && ch == this.ch
  def otherSepThan( ch:Char ) = t == Type.CHAR && ch != this.ch
		
  def str( s:String ) = t == Type.STRING && s.equals( this.s )
		
  def year          = t == Type.INT && len <= 4
  def fourDigitYear = t == Type.INT && len == 4
  def month =
    t match {
    case Type.INT    => i >= 1 && i <= 12
    case Type.STRING =>
      if ( eval( Type.MONTH, Time.MonthNames ) ) {
        t = Type.MONTH
        i += 1
        true
      } else
        false
    case Type.MONTH  => true
    case _           => false
    }
		
  def dayOfMonth = t == Type.INT && i >= 1 && i <= 31
  def dayOfWeek =
    t match {
    case Type.STRING =>
      if ( eval( Type.DAY_OF_WEEK, Time.WeekDayNames ) ) {
        t = Type.DAY_OF_WEEK
        i += 1
        true
      } else
        false 
    case Type.DAY_OF_WEEK => true
    case _                => false
    }
		
  def hour   = t == Type.INT && i < 24
  def minute = t == Type.INT && i < 60
  def second = t == Type.INT && i < 60
  def milli  = t == Type.INT && i < 1000
  def ampm =
    t match {
    case Type.STRING => eval( Type.AMPM, Time.AmPmNames )
    case Type.AMPM   => true
    case _           => false
    }
		
  def timeZone =
    t match {
    case Type.STRING   =>
      val tz = Time.toLaxTimeZone( s )
				
      if ( tz != null ) {
        t = Type.TIMEZONE
        s = tz.getID
        true
      } else
        false
    case Type.TIMEZONE => true
    case _             => false
    }
		
  def filler =
    t match {
    case Type.STRING => eval( Type.FILLER, Time.FillerWords )
    case Type.FILLER => true
    case _           => false
    }
		
  def userRelatives =
    t match {
    case Type.STRING        => if ( dayOfWeek )
                                 false // prioritize things like "mon" as "monday" instead of "month" ... probably a cleaner way to do this
                               else
                                 eval( Type.USER_RELATIVE, Relative.Words )
    case Type.USER_RELATIVE => true
    case _                  => false
    }
		
  private def eval( nt:Type, types:Array[String] ):Boolean = {
    for ( j <- 0 until types.length ) {
      if ( types( j ).startsWith( s ) ) {
        t = nt
        i = j
        return true
      }
    }
			
    false
  }
		
  def text( txt:String ) = if ( t == Type.STRING ) s
                           else                    txt.substring( pos, pos+len )

  override def toString =
    t match {
    case Type.INT           => i.toString
    case Type.STRING        => "\"" + s + '"'
    case Type.CHAR          => "\'" + ch + '\''
    case Type.MONTH         => "!" + Time.MonthNames( i-1 )
    case Type.DAY_OF_WEEK   => "!" + Time.WeekDayNames( i-1 )
    case Type.AMPM          => "!" + Time.AmPmNames( i )
    case Type.TIMEZONE      => "tz" + s.toString
    case Type.FILLER        => "!" + Time.FillerWords( i )
    case Type.USER_RELATIVE => "!" + Relative.Words( i )
    }
}

object Relative {
	val TOMORROW  = 0
	val YESTERDAY = 1
	val TODAY     = 2
	val NEXT      = 3
	val THIS      = 4
	val LAST      = 5
	val NOW       = 6
	val WEEK      = 7
	val MONTH     = 8
	val YEAR      = 9
	val DECADE    = 10
	val CENTURY   = 11

  val Words  = Array( "tomorrow", "yesterday", "today", "next", "this", "last", "now", "week", "month", "year", "decade", "century" )
}

sealed trait Comp

object Comp {
  case object SEP extends Comp
  case object DASH extends Comp
  case object SLASH extends Comp
  case object PERIOD extends Comp
  case object COLON extends Comp
  case object COMMA extends Comp
  case object YEAR extends Comp
  case object MONTH extends Comp
  case object DAY_OF_WEEK extends Comp
  case object DAY_OF_MONTH extends Comp
  case object HOUR extends Comp
  case object MINUTE extends Comp
  case object SECOND extends Comp
  case object MILLI extends Comp
  case object AMPM extends Comp
  case object TIMEZONE extends Comp
}

class TimeParser {

	var now:Calendar = Time.createUserNowCalendar
	private var sb = new StringBuilder
	private var text:String = _
	
	
	/*
	 * --- Components
	 */
	
  private def comp( t:Int, c0:Comp, c1:Comp, c2:Comp ) =
    t+3 <= tcount && tks(t).comp( c0 ) && tks(t+1).comp( c1 ) && tks(t+2).comp( c2 )
    
  private def comp( t:Int, c0:Comp, c1:Comp, c2:Comp, c3:Comp ) =
    t+4 <= tcount && tks(t).comp( c0 ) && tks(t+1).comp( c1 ) && tks(t+2).comp( c2 ) && tks(t+3).comp( c3 )
    
	
	/*	
	 * --- Tokens
	 */
	
	private var tcount = 0
	private var tks = {
    val a = new Array[Token]( 16 )
		for ( i <- 0 until a.length )
			a( i ) = new Token
    a
  }

	def tokenString = {
		sb.setLength( 0 )
		sb ++= tcount.toString
		sb ++= " token"
		sb ++= ( if ( tcount != 1 ) "s " else "  " )
	
    for ( i <- 0 until tcount )
      sb += ' ' ++= tks( i ).toString
		
		sb.toString
	}
	
	private def fail( reason:String ) =
    throw new ParseException( "Could not parse \"" + text + "\" because " + reason, tcount > 0 |* tks( tcount-1 ).pos )
	
	private def sep( tp:Int ) =
		if ( tp < tcount && tks( tp ).sep ) tp + 1
    else                                tp
	
	
	/*
	 * --- Conflicting Duplicate Information detection
	 */

  private var monthDays = 0
  private var years     = 0
	private var dates     = 0
	private var times     = 0
	private var timeZones = 0

	private def dateFound( tp:Int ) {
		if ( dates > 0 || monthDays > 0 || years > 0 )
			fail( "duplicate date information was found at \"" + text.substring( tks( tp ).pos ) + "\"." )
		
		dates += 1
	}

	private def monthDayFound( tp:Int ) {
		if ( dates > 0 || monthDays > 0 )
			fail( "duplicate date information was found at \"" + text.substring( tks( tp ).pos ) + "\"." )
	
    monthDays += 1
    if ( years > 0 )
      dates += 1
	}

	private def yearFound( tp:Int ) {
		if ( dates > 0 || years > 0 )
			fail( "duplicate date information was found at \"" + text.substring( tks( tp ).pos ) + "\"." )
		
    years += 1
    if ( monthDays > 0 )
      dates += 1
	}

	private def timeFound( tp:Int ) {
		if ( times > 0 )
			fail( "duplicate time information was found at \"" + text.substring( tks( tp ).pos ) + "\"." )
		
		times += 1
	}
	
	private def timeZoneFound( tp:Int ) {
		if ( timeZones > 0 )
			fail( "duplicate time zone information was found at \"" + text.substring( tks( tp ).pos ) + "\"." )

		timeZones += 1
	}

  private def copyNowDate = {
    year       = now.get( Calendar.YEAR )
    month      = now.get( Calendar.MONTH )
    dayOfMonth = now.get( Calendar.DAY_OF_MONTH )
  }

  private def copyNowDateTime = {
    copyNowDate
    hour   = now.get( Calendar.HOUR_OF_DAY )
    minute = now.get( Calendar.MINUTE )
    second = now.get( Calendar.SECOND )
    milli  = now.get( Calendar.MILLISECOND )
  }


	/*
	 * --- Date Parsing
	 */
	
	private def matchDate:Boolean = {
		val t = tks( tp )
    
    // ISO 8601 parsing ... i.e. 2008-01-02T02:30Z
		if (   tp + 7 < tcount && tks( tp+5 ).s == "T"
        && t.year && tks( tp+1 ).sep( '-' ) && tks( tp+2 ).month && tks( tp+3 ).sep( '-' ) && tks( tp+4 ).dayOfMonth
        && tks( tp+6 ).hour ) {

      year       = Time.fourDigitYear( t.i, t.len )
      month      = tks( tp+2 ).i-1
      dayOfMonth = tks( tp+4 ).i
      dateFound( tp )

      hour       = tks( tp+6 ).i
      timeFound( tp )
      tp += 7

      if ( tp + 2 < tcount && tks( tp ).sep( ':' ) && tks( tp+1 ).second ) {
        minute = tks( tp+1 ).i
        tp += 2
        if ( tp + 2 < tcount && tks( tp ).sep( ':' ) && tks( tp+1 ).second ) {
          second = tks( tp+1 ).i
          tp += 2
        } else {
          second = 0
        }
      } else {
        minute = 0
      }

      matchTimeMilli

      if ( tp < tcount ) {
        if ( tks( tp ).str( "z" ) ) {
          tp += 1
          timeZoneFound( tp )
        } else if ( tks( tp ).sep( '-' ) || tks( tp ).sep( '+' ) ) {
          val minus = tks( tp ).sep( '-' )
          tp += 1
          var h = 0
          var m = 0

          if ( tp + 3 <= tcount && tks( tp ).hour && tks( tp+1 ).sep( ':' ) && tks( tp+2 ).minute ) {
            h = tks( tp     ).i
            m = tks( tp + 2 ).i
            tp += 3
          } else if ( tp < tcount && tks( tp ).t == Type.INT && tks( tp ).len == 4 ) {
            h = tks( tp ).i
            m = h % 100; h /= 100
            tp += 1
          } else if ( tp < tcount && tks( tp ).hour ) {
            h = tks( tp ).i
            tp += 1
          } else {
            fail( "an invalid ISO 8601 time zone format was found: \"" + text.substring( tks( tp ).pos ) + "\"." )
          }

          tzOffsetMin = h * 60 + m
          if ( minus ) tzOffsetMin *= -1
          timeZoneFound( tp )
        } else {
          fail( "an invalid ISO 8601 time zone format was found: \"" + text.substring( tks( tp ).pos ) + "\"." )
        }

      } else {
        fail( "an invalid ISO 8601 format was found -- it was missing a 'Z' at the end." )
      }

      return true
    }

		if (   tp + 4 < tcount
        && t.year && tks( tp+2 ).month && tks( tp+4 ).dayOfMonth
			  && tks( tp+1 ).t == Type.CHAR && tks( tp+3 ).t == Type.CHAR ) {
			val sep = tks( tp+1 ).ch
			
			if ( ( sep == '-' || sep == '.' ) && sep == tks( tp+3 ).ch ) {
				year       = Time.fourDigitYear( t.i, t.len )
				month      = tks( tp+2 ).i-1
				dayOfMonth = tks( tp+4 ).i
				dateFound( tp )
		    tp += 5
		    return true
			}
		}

		if (   tp + 4 < tcount
        && t.month && tks( tp+2 ).dayOfMonth && tks( tp+4 ).year
			  && tks( tp+1 ).t == Type.CHAR && tks( tp+3 ).t == Type.CHAR
           // don't try to interpret an hour as a year in Aug 09 19:00:00
        && ( tp + 5 >= tcount || tks( tp+5 ).ch != ':' ) ) {
			val sep = tks( tp+1 ).ch
			
			if ( ( sep == '-' || sep == '.' || sep == '/' ) && sep == tks( tp+3 ).ch ) {
				year       = Time.fourDigitYear( tks( tp+4 ).i, tks( tp+4 ).len )
				month      = t.i-1
				dayOfMonth = tks( tp+2 ).i
				dateFound( tp )
		    tp += 5
		    return true
			}
		}

		if ( t.len == 8 && t.t == Type.INT ) {
			var y = t.i
			val d = y % 100; y /= 100
			val m = y % 100; y /= 100
			
			if (   d >= 1 && d <= 31
			    && m >= 1 && m <= 12 ) {
				year       = y
				month      = m-1
				dayOfMonth = d
				dateFound( tp )
				tp += 1
				return true
			}
		}
		
		if ( t.month ) {
			if ( comp( tp+1, Comp.DAY_OF_MONTH, Comp.COMMA, Comp.YEAR ) ) {
				year       = Time.fourDigitYear( tks(tp+3).i, tks(tp+3).len )
				month      = t.i-1
				dayOfMonth = tks(tp+1).i
				dateFound( tp )
		    tp += 4
		    return true
			}
			
			if ( comp( tp+1, Comp.SLASH, Comp.DAY_OF_MONTH, Comp.SLASH, Comp.YEAR ) ) {
				year       = Time.fourDigitYear( tks(tp+4).i, tks(tp+4).len )
				month      = t.i-1
				dayOfMonth = tks(tp+2).i
				dateFound( tp )
		    tp += 5
		    return true
			}
		}

    if (   tp + 3 < tcount
        && t.dayOfMonth && tks( tp+1 ).month && tks( tp+1 ).t == Type.MONTH && tks( tp+2 ).fourDigitYear ) {
      year       = tks( tp + 2 ).i
      month      = tks( tp + 1 ).i-1
      dayOfMonth = t.i
      dateFound( tp )
      tp += 3
      return true
    }

		if ( t.userRelatives ) {
			t.i match {
			case Relative.TODAY | Relative.YESTERDAY | Relative.TOMORROW =>
				dateFound( tp )
        copyNowDate
        tp += 1
				tp = sep( tp )
				t.i match {
				case Relative.YESTERDAY => rollDaysNeeded = -1
				case Relative.TOMORROW  => rollDaysNeeded = 1
        case _                  =>
				}
				userTime = true
				return true
			case Relative.LAST | Relative.NEXT =>
				dateFound( tp )
        tp += 1

        if ( tp < tcount ) {

          if ( tks( tp ).dayOfWeek ) {
					
            t.i match {
            case Relative.LAST => rollDaysNeeded = -1
            case Relative.NEXT => rollDaysNeeded = 1
            }

            dayOfWeek = tks( tp ).i
            rollToDayOfWeek = true
            copyNowDate
            tp += 1
            tp = sep( tp )
            userTime = true
            return true
          } else if ( tks( tp ).userRelatives ) {
            tks( tp ).i match {
            case Relative.WEEK    =>
              t.i match {
              case Relative.LAST => rollDaysNeeded = -7
              case Relative.NEXT => rollDaysNeeded = 7
              }

              copyNowDateTime
              tp += 1
              tp = sep( tp )
              userTime = true
              return true

            case Relative.MONTH   =>
              copyNowDateTime

              t.i match {
              case Relative.LAST => month -= 1; if ( month < 0 )  { month = 11; year -= 1 }
              case Relative.NEXT => month += 1; if ( month > 11 ) { month = 0;  year += 1 }
              }

              tp += 1
              tp = sep( tp )
              userTime = true
              return true

            case duration if duration == Relative.YEAR || duration == Relative.DECADE || duration == Relative.CENTURY =>
              copyNowDateTime

              val d =
                duration match {
                case Relative.YEAR    => 1
                case Relative.DECADE  => 10
                case Relative.CENTURY => 100
                }

              t.i match {
              case Relative.LAST => year -= d
              case Relative.NEXT => year += d
              }

              tp += 1
              tp = sep( tp )
              userTime = true
              return true

            }
          }
        }

			  fail( "\"" + t.s + "\" did not have a day of the week or time duration after it." )

			case Relative.THIS =>
				fail( "\"this\" is confusing, try \"last\" or \"next\"." )
			case Relative.NOW =>
				dateFound( tp )
				timeFound( tp )
				tp = sep( tp+1 )
        copyNowDateTime
				userTime = true
				return true

      case _ =>
			}
		}
			
		false
	}
	
	private def matchMonthDay:Boolean = {
		val t = tks( tp )
    
		if ( tp + 1 < tcount && t.t == Type.MONTH && tks( tp+1 ).dayOfMonth ) {
      month      = tks( tp ).i-1
      dayOfMonth = tks( tp+1 ).i
      monthDayFound( tp )
      tp += 2
      return true
		}

		false
	}
	
	private def matchYear:Boolean = {
		val t = tks( tp )
    
		if ( tp < tcount && t.year && t.len == 4 ) {
      year = tks( tp ).i
      yearFound( tp )
      tp += 1
      return true
		}

		false
	}
	
	
	/*
	 * --- Time Parsing
	 */
	
	private def matchTimeMilli =
		if ( tp+1<tcount && tks(tp).sep( '.' ) && tks(tp+1).milli ) {
			milli = tks( tp+1 ).i
			tp += 2
		}
	
	private def matchAmpm {
		var am:Boolean = false
		var error:String = null;

		if (   tp+3<tcount
			  && tks( tp+1 ).sep( '.' ) && tks( tp+3 ).sep( '.' )
			  && ( tks( tp ).str( "a" ) || tks( tp ).str( "p" ) )
			  && tks( tp+2 ).str( "m" ) ) {
			if ( hour > 12 )
				error = text.substring( tks( tp ).pos, tks( tp+3 ).endPos )

			am = tks( tp ).str( "a" )
			tp += 4
		} else if ( tp<tcount && tks( tp ).ampm ) {
			if ( hour > 12 )
				error = tks( tp ).text( text )
		
			am = ( tks( tp ).i == 0 )
      tp += 1
		} else {
			return
		}
		
		if ( error != null )
			fail( "a 24 hour time was given while an am/pm (\"" + error + "\") was found." )
		
		if ( am ) {
			if ( hour == 12 )
				hour = 0
		} else {
			if ( hour < 12 )
				hour += 12
		}
	}
	
	private def matchTime:Boolean = {
		val t = tks( tp )
		
		try {
      if ( t.t == Type.INT ) {
        if ( t.len == 6 ) {
          var h = t.i
          val s = h % 100; h /= 100
          val m = h % 100; h /= 100
				
          if ( h < 24 && m < 60 && s < 60 ) {
            hour   = h
            minute = m
            second = s
            timeFound( tp )
            tp += 1
					
            matchTimeMilli
            matchAmpm
            return true
          }
        }
			
        if ( t.len == 4 ) {
          var h = t.i
          val m = h % 100; h /= 100
				
          if ( h < 24 && m < 60 ) {
            hour   = h
            minute = m
            timeFound( tp )
            tp += 1
					
            matchAmpm
            return true
          }
        }
      }
			
			if ( t.hour ) {
				if ( tp+1<tcount && tks( tp+1 ).otherSepThan( ':' ) )
					// it's probably a date
					return false
				
				hour = t.i
				timeFound( tp )
				tp += 1
				
				if ( tp+1<tcount && tks(tp).sep( ':' ) && tks(tp+1).minute ) {
					minute = tks( tp+1 ).i
					tp += 2
					
					if ( tp+1<tcount && tks(tp).sep( ':' ) && tks(tp+1).second ) {
						second = tks( tp+1 ).i
						tp += 2
	
						matchTimeMilli
					}
				}
	
				matchAmpm
				return true
			}
		} catch {
    case ex:NullPointerException =>
			fail( "it is a date but time information was found: \"" + text.substring( t.pos ) + "\"." )
		}
		
		false
	}

	
	/*
	 * --- Time Zones
	 */
	
	private def matchTimeZone:Boolean = {
	  if ( dateOnly )
	    return false
	    
		val t = tks( tp )
		
    if ( tp<tcount && tks( tp ).timeZone ) {
      try {
				timeZoneFound( tp )
				tz = TimeZone.getTimeZone( tks( tp ).s )
        tp += 1
				return true
      } catch {
      case ex:NullPointerException =>
        fail( "it is a date but time zone information was found: \"" + text.substring( t.pos ) + "\"." )
      }
		} else if (   tp + 1 < tcount
               && ( tks( tp ).sep( '-' ) || tks( tp ).sep( '+' ) )
               && ( tks( tp+1 ).t == Type.INT && tks( tp+1 ).len == 4 ) ) {
      val minus = tks( tp ).sep( '-' )
      var h = tks( tp+1 ).i
      var m = h % 100; h /= 100

      tp += 2

      tzOffsetMin = h * 60 + m
      if ( minus ) tzOffsetMin *= -1
      timeZoneFound( tp )
      return true
    }

		false
	}
	
		
	private var dayOfWeek = 0
	private var rollDaysNeeded = 0
	private var rollToDayOfWeek = false
	private var userTime = false
  private var year = 0
  private var month = 0
  private var dayOfMonth = 0
  private var hour = 0
  private var minute = 0
  private var second = 0
  private var milli = 0
  private var tz = Time.Utc
  private var tzOffsetMin = 0
  private var dateOnly = false

	private var tp = 0

	def parse( text:String, dv:Calendar = Time.createNullCalendar, dateOnly:Boolean = false, forceUserTime:Boolean = false ):Calendar = {
    year = 0
    month = 0
    dayOfMonth = 0
    hour = 0
    minute = 0
    second = 0
    milli = 0
    tz = Time.Utc
    tzOffsetMin = 0
		this.text = text
    this.dateOnly = dateOnly

    userTime = forceUserTime
		
		monthDays = 0
		years     = 0
		dates     = 0
		times     = 0
		timeZones = 0

		dayOfWeek = 0
		rollDaysNeeded = 0
		rollToDayOfWeek = false

		tokenize

    tp = 0
	
    var cond = true
		while ( tp < tcount && cond ) {
			
			if (   ( dates     == 0 && matchDate )
				  || ( times     == 0 && matchTime )
				  || ( monthDays == 0 && matchMonthDay )
				  || ( years     == 0 && matchYear ) ) {
				;
      } else {
			
        val t = tks( tp )
			
        if ( t.dayOfWeek ) {
          dayOfWeek = t.i
          tp += 1
          tp = sep( tp )
        } else if ( t.filler ) {
          tp += 1
        } else if (   ( timeZones == 0 && matchTimeZone )
                   || matchDate
				           || matchTime ) {
        } else {
          fail( "\"" + text.substring( t.pos ) + "\" was confusing." )
        }
      }
		}
		
		if ( dates == 0 )
			fail( "no date could be found." )

    if ( !dateOnly && timeZones == 0 && userTime )
      tz = T.session.netTimeZone

    dv.setTimeZone( tz )
    dv.set( year, month, dayOfMonth, hour, minute, second )
    dv.set( Calendar.MILLISECOND, milli )

		if ( rollDaysNeeded != 0 && dayOfWeek == 0 )
			dv.add( Calendar.DAY_OF_MONTH, rollDaysNeeded )
		
		if ( dayOfWeek != 0 ) {
			if ( rollToDayOfWeek )
				dv.rollToNextDayOfWeek( dayOfWeek, rollDaysNeeded )
			else
				if ( dayOfWeek != dv.get( Calendar.DAY_OF_WEEK ) )
					fail(  Time.WeekDayNames( dayOfWeek-1 ).capitalize
						   + " was specified but that date is on a "
					     + Time.WeekDayNames( dv.get( Calendar.DAY_OF_WEEK ) - 1 ).capitalize + "." )
		}

    if ( tzOffsetMin != 0 )
      dv.add( Calendar.MINUTE, tzOffsetMin )

    dv
	}
	
	private def tokenize {
		try {
			tcount = 0
			val len = text.length
			var pos = 0

      def inner:Boolean = {
				var ch = text.charAt( pos )
        pos += 1
				while ( Character.isWhitespace( ch ) ) {
					if ( pos >= len )
						return false
					
					ch = text.charAt( pos )
          pos += 1
				}

				var digit = Character.digit( ch, 10 )
				val start = pos-1
				if ( digit != -1 ) { // is it a digit?
					var i = digit
					var tlen = 1
				
          var cond = true
					while ( pos < len && cond ) {
						ch = text.charAt( pos )
						digit = Character.digit( ch, 10 )
						
						if ( digit == -1 ) {
							cond = false
            } else {
              i = 10 * i + digit
              tlen += 1
              pos += 1
            }
					}
					
					val t = tks( tcount )
          tcount += 1
					t.t = Type.INT
					t.pos = start
					t.len = tlen
					t.i = i
					return true
				}
				
				ch match { // is it a character?
				case '/' | '-' | '+' | ':' | '.' | ',' =>
					val t = tks( tcount )
          tcount += 1
					t.t = Type.CHAR
					t.pos = start
					t.len = 1
					t.ch = ch
					return true

        case 'T' => // ISO 8601 time separator
          if ( pos+1 < len && Character.isDigit( text.charAt( pos+1 ) ) ) {
            val t = tks( tcount )
            tcount += 1
            t.t = Type.STRING
            t.pos = start
            t.len = 1
            t.s = "T"
            return true
          }

        case _ =>
				}
				
				// must be a string
				sb.setLength( 0 )
				sb.append( Character.toLowerCase( ch ) )
			
        var cond = true
				while ( pos < len && cond ) {
					val ch = text.charAt( pos )
					
					if ( Character.isWhitespace( ch ) ) {
						pos += 1
						cond = false
					} else if ( ch == '-' || ch == ',' || ch == '.' || ch == '/' ) {
						cond = false
          } else {
            sb.append( Character.toLowerCase( ch ) )
            pos += 1
          }
				}

				val t = tks( tcount )
        tcount += 1
				t.t = Type.STRING
				t.pos = start
				t.s = sb.toString
				t.len = t.s.length
        return true
			}

			while ( pos < len )
        if ( !inner )
          return
		} catch {
    case ex:ArrayIndexOutOfBoundsException =>
			if ( tcount >= tks.length ) {
				val olen = tks.length
				tks = tks.resize( olen * 2 )
        for ( i <- olen until tks.length )
					tks( i ) = new Token
			}

			tokenize
		}
	}
}

