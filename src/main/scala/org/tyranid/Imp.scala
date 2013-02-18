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

package org.tyranid

import java.io.InputStream
import java.text.DateFormat
import java.util.{ Calendar, Date }

import scala.collection.mutable.Buffer
import scala.xml.{ NodeSeq, Unparsed }

import org.tyranid.log.Log
import org.tyranid.session.Session


/**
 * IMPlicit IMPorts.
 */
object Imp { 

  type ObjectMap = scala.collection.mutable.LinkedHashMap[String,Any]

  def T = org.tyranid.session.ThreadData()
  def B = org.tyranid.boot.Boot.instance

  // extensions
	implicit def anyImp[ T <: Any ]( v:T )                          = new org.tyranid.any.AnyImp[T]( v )
	implicit def anyRefImp[ T <: AnyRef ]( v:T )                    = new org.tyranid.any.AnyRefImp[T]( v )
  implicit def array[A]( a:Array[A] )                             = new org.tyranid.collection.ArrayImp( a )
	implicit def boolean( v:Boolean )                               = new org.tyranid.logic.BooleanImp( v )
  implicit def bufferImp[A]( a:Buffer[A] )                        = new org.tyranid.collection.BufferImp( a )
	implicit def calendarImp( v:Calendar )                          = new org.tyranid.time.CalendarImp( v )
	implicit def charImp( v:Char )                                  = new org.tyranid.text.CharImp( v )
	implicit def dateImp( v:Date )                                  = new org.tyranid.time.DateImp( v )
	implicit def dateFormatImp( v:DateFormat )                      = new org.tyranid.time.DateFormatImp( v )
	implicit def option[A]( v:Option[A] )                           = new org.tyranid.collection.OptionImp( v )
  implicit def intImp( v:Int )                                    = new org.tyranid.math.IntImp( v )
  implicit def longImp( v:Long )                                  = new org.tyranid.math.LongImp( v )
  implicit def doubleImp( v:Double )                              = new org.tyranid.math.DoubleImp( v )
	implicit def stringImp( v:String )                              = new org.tyranid.text.StringImp( v )
  implicit def objectMapImp( v:scala.collection.Map[String,Any] ) = new org.tyranid.collection.ObjectMapImp( v )
  implicit def seqImp[A]( a:Seq[A] )                              = new org.tyranid.collection.SeqImp( a )
  implicit def byteArray( ba:Array[Byte] )                        = new org.tyranid.math.ByteArray( ba )
	implicit def symbol( v:Symbol )                                 = v.name
	implicit def throwableImp( t:Throwable )                        = new org.tyranid.logic.ThrowableImp( t )
	implicit def jackson( v:org.codehaus.jackson.JsonNode )         = new org.tyranid.json.JsonNodeImp( v )
	implicit def inputStreamImp( v:InputStream )                    = new org.tyranid.io.InputStreamImp( v )

  import javax.servlet.http.{ HttpServletRequest, HttpServletResponse }
  implicit def servletRequest ( v:HttpServletRequest  )           = new org.tyranid.http.HttpServletRequestOps ( v )
  implicit def servletResponse( v:HttpServletResponse )           = new org.tyranid.http.HttpServletResponseOps( v )


  /**
   * Try a block of code that returns a string, if it succeeds return the string, otherwise return the exception message.
   */
  def trystr( block: => String ) =
    try {
      block
    } catch {
    case e =>
      e.printStackTrace
      e.getMessage
    }

  def trylog( block: => Unit ) =
    try {
      block
    } catch {
    case e => e.log
    }

  def spam( msg:Any ) =
    if ( msg != null ) println( "SPAM: " + msg.toString )
    else               println( "SPAM: null" )

  def look[T]( msg:String, block: => T ): T = {
    spam( "look[" + msg + "]=>[...evaluating...]" )
    val result = block
    spam( "LOOK[" + msg + "]=>[" + ( if ( result != null ) result.toString else "null" ) + "]" )
    result
  }

	@volatile private var depth = 1
	def time[ T ]( title:String )( block: => T ): T = {
		val startTime = System.currentTimeMillis
		println( ( ">" * depth ) + " " * 13 + title + " START: " + ( new Date ).toServerDateTimeStr )
		depth += 1

		try {
			block
		} finally {
			depth -= 1
			println( ( "<" * depth ) + " " + "%8d".format( System.currentTimeMillis - startTime ) + "ms. " + " " * title.length + " END" )
		}
	}

  def background( block: => Unit ) {
    val s = Session()
    
    scala.concurrent.ops.spawn {
      T.becomeSession( s )
      
      trylog {
        block
      }
    }
  }

  def background_?( test:Boolean, block: => Unit) {
    test ? background { block } | block
  }

  def tryapi( block: => Map[Any,Any] ) {
    try {
      T.web.json( block + ( "status" -> "ok" ) )
    } catch {
      case e:Exception =>
        e.printStackTrace
        log( Event.StackTrace, "m" -> "Error in API call.", "ex" -> e )
        T.web.json( Map( "status" -> "error", "message" -> e.getMessage ) )
    }
  }
 
  val Event = org.tyranid.log.Event
  val Log   = org.tyranid.log.Log
  def log( event:org.tyranid.log.Event, opts:(String,Any)* ) = org.tyranid.log.Log.log( event, opts:_* )

  def problem( desc:String ) = throw new org.tyranid.db.ModelException( desc )

  def eye( tid:String ) = org.tyranid.db.meta.Tid.eye( tid )

  val Sbt = org.tyranid.json.Sbt
}
