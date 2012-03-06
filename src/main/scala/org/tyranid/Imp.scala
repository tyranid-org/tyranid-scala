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

package org.tyranid

import java.util.{ Calendar, Date }

import scala.xml.{ NodeSeq, Unparsed }

import org.tyranid.log.Log


/**
 * IMPlicit IMPorts.
 */
object Imp {

  type ObjectMap = scala.collection.mutable.LinkedHashMap[String,Any]

  def T = org.tyranid.session.ThreadData()
  def B = org.tyranid.boot.Boot.instance

  /**
   * Try a block of code that returns a string, if it succeeds return the string, otherwise return the exception message.
   */
  def trystr[T]( block: => String ) =
    try {
      block
    } catch {
      case e =>
        e.printStackTrace
        e.getMessage
    }

  def spam( msg:AnyRef ) =
    if ( msg != null ) println( "SPAM: " + msg.toString )
    else               println( "SPAM: null" )

  def look[T]( msg:String, block: => T ): T = {
    spam( "look[" + msg + "]=>[...evaluating...]" )
    val result = block
    spam( "LOOK[" + msg + "]=>[" + ( if ( result != null ) result.toString else "null" ) + "]" )
    result
  }

  def background( block: => Unit ) {
    scala.concurrent.ops.spawn {
      try {
        block
      } catch {
      case e =>
        throwableImp( e ).log
      }
    }
  }

  val Log = org.tyranid.log.Log
  def log( event:Int, opts:(String,Any)* ) = org.tyranid.log.Log.log( event, opts:_* )

	implicit def anyImp[ T <: Any ]( v:T )                          = new org.tyranid.any.AnyImp[T]( v )
	implicit def anyRefImp[ T <: AnyRef ]( v:T )                    = new org.tyranid.any.AnyRefImp[T]( v )
	implicit def boolean( v:Boolean )                               = new org.tyranid.logic.BooleanImp( v )
	implicit def calendarImp( v:Calendar )                          = new org.tyranid.time.CalendarImp( v )
	implicit def charImp( v:Char )                                  = new org.tyranid.text.CharImp( v )
	implicit def dateImp( v:Date )                                  = new org.tyranid.time.DateImp( v )
	implicit def option[A]( v:Option[A] )                           = new org.tyranid.collection.OptionImp( v )
  implicit def intImp( v:Int )                                    = new org.tyranid.math.IntImp( v )
  implicit def longImp( v:Long )                                  = new org.tyranid.math.LongImp( v )
	implicit def stringImp( v:String )                              = new org.tyranid.text.StringImp( v )
  implicit def array[A]( a:Array[A] )                             = new org.tyranid.collection.ArrayImp( a )
  implicit def objectMapImp( v:scala.collection.Map[String,Any] ) = new org.tyranid.collection.ObjectMapImp( v )
  implicit def seqImp[A]( a:Seq[A] )                              = new org.tyranid.collection.SeqImp( a )
  implicit def byteArray( ba:Array[Byte] )                        = new org.tyranid.math.ByteArray( ba )
	implicit def symbol( v:Symbol )                                 = v.name
	implicit def throwableImp( t:Throwable )                        = new org.tyranid.logic.ThrowableImp( t )
	implicit def jackson( v:org.codehaus.jackson.JsonNode )         = new org.tyranid.json.JsonNodeImp( v )

  import javax.servlet.http.{ HttpServletRequest, HttpServletResponse }
  implicit def servletRequest ( v:HttpServletRequest  )           = new org.tyranid.http.HttpServletRequestOps ( v )
  implicit def servletResponse( v:HttpServletResponse )           = new org.tyranid.http.HttpServletResponseOps( v )

}

