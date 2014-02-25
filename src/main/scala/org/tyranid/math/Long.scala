/**
 * Copyright (c) 2008-2014 Tyranid <http://tyranid.org>
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

package org.tyranid.math

import org.tyranid.Imp._
import org.tyranid.time.Time

import scala.math

object LongImp {
  
  val KB:Double = 1000
  val MB:Double = math.pow( KB, 2 )
  val GB:Double = math.pow( KB, 3 )
  val TB:Double = math.pow( KB, 4 )
  val PB:Double = math.pow( KB, 5 )
}

case class LongImp( l:Long ) {
  def toBytesString = {
    if ( l >= LongImp.PB )
      String.format( "%2.2f petabytes", double2Double( ( l / LongImp.TB ).asInstanceOf[Double] ) )
    else if ( l >= LongImp.TB )
      String.format( "%2.2f terabytes", double2Double( ( l / LongImp.TB ).asInstanceOf[Double] ) )
    else if ( l >= LongImp.GB ) 
      String.format( "%1.2f gigabytes", double2Double( ( l / LongImp.GB ).asInstanceOf[Double] ) )
    else if ( l >= LongImp.MB ) 
      String.format( "%1.2f megabytes", double2Double( ( l / LongImp.MB ).asInstanceOf[Double] ) )
    else if ( l >= LongImp.KB )
      String.format( "%1.2f kilobytes", double2Double( ( l / LongImp.KB ).asInstanceOf[Double] ) )
    else 
      l + " bytes"
  }
  
  def toBytesAbbrString = {
    if ( l >= LongImp.PB )
      String.format( "%2.2f PB", double2Double( ( l / LongImp.TB ).asInstanceOf[Double] ) )
    else if ( l >= LongImp.TB )
      String.format( "%2.2f TB", double2Double( ( l / LongImp.TB ).asInstanceOf[Double] ) )
    else if ( l >= LongImp.GB ) 
      String.format( "%1.2f GB", double2Double( ( l / LongImp.GB ).asInstanceOf[Double] ) )
    else if ( l >= LongImp.MB ) 
      String.format( "%1.2f MB", double2Double( ( l / LongImp.MB ).asInstanceOf[Double] ) )
    else if ( l >= LongImp.KB )
      String.format( "%1.2f KB", double2Double( ( l / LongImp.KB ).asInstanceOf[Double] ) )
    else 
      l + " B"
  }
  
  def pow( p:Int ) = math.pow( l.toDouble, p.toDouble ).toLong
  
  def toDurationString = {
    val days  = l / Time.OneDayMs
    val hrs = ( l - ( days * Time.OneDayMs ) ) / Time.OneHourMs
    val mins = ( l - ( days * Time.OneDayMs ) - ( hrs * Time.OneHourMs ) ) / Time.OneMinuteMs
    val secs = ( l - ( days * Time.OneDayMs ) - ( hrs * Time.OneHourMs ) - ( mins * Time.OneMinuteMs ) )  / 1000
    
    if ( l >= Time.OneDayMs )
      "%d days %d hrs %d mins %d secs".format( days, hrs, mins, secs )
    else if ( l >= Time.OneHourMs ) 
      "%d hrs %d mins %d secs".format( hrs, mins, secs )
    else  
      "%d mins %d secs".format( mins, secs )
  }
  
  def or( o:Long ):Long = ( ( l == 0 ) ? o | l )  
}




