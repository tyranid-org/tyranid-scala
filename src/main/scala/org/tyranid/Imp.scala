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


/**
 * IMPlicit IMPorts.
 */
object Imp {

  def Tyr = org.tyranid.boot.Boot.instance

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

  def spam( msg:String ) = println( "SPAM: " + msg )
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
        println( "spawn() stack trace:" )
        e.printStackTrace()
      }
    }
  }

  def log( msg:String = "", exception:Exception = null ) = org.tyranid.log.Log.log( msg, exception )

	implicit def anyImp[ T <: Any ]( v:T )                  = new org.tyranid.logic.AnyImp[T]( v )
	implicit def anyRefImp[ T <: AnyRef ]( v:T )            = new org.tyranid.logic.AnyRefImp[T]( v )
	implicit def boolean( v:Boolean )                       = new org.tyranid.logic.BooleanImp( v )
	implicit def calendarImp( v:Calendar )                  = new org.tyranid.time.CalendarImp( v )
	implicit def dateImp( v:Date )                          = new org.tyranid.time.DateImp( v )
	implicit def option[A]( v:Option[A] )                   = new org.tyranid.collection.OptionImp( v )
	implicit def string( v:String )                         = new org.tyranid.text.StringImp( v )
  implicit def array[A]( a:Array[A] )                     = new org.tyranid.collection.ArrayImp( a )
  implicit def seqImp[A]( a:Seq[A] )                      = new org.tyranid.collection.SeqImp( a )
  implicit def byteArray( ba:Array[Byte] )                = new org.tyranid.math.ByteArray( ba )
	implicit def symbol( v:Symbol )                         = v.name
	implicit def jackson( v:org.codehaus.jackson.JsonNode ) = new org.tyranid.json.JsonNodeImp( v )

  import javax.servlet.http.{ HttpServletRequest, HttpServletResponse }
  implicit def servletRequest ( v:HttpServletRequest  )   = new org.tyranid.http.HttpServletRequestOps ( v )
  implicit def servletResponse( v:HttpServletResponse )   = new org.tyranid.http.HttpServletResponseOps( v )

}

