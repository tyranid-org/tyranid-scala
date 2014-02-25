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

import scala.math

import org.tyranid.Imp._
import org.tyranid.time.Time

object DoubleImp {
  
}

case class DoubleImp( d:Double ) {
  val minutesFormat = "%02d:%02d" // mm:ss
  val hoursFormat = "%02d:" + minutesFormat // hh:mm:ss
  val daysFormat = "%02d:" + hoursFormat // dd:hh:mm:ss
    
  val OneDaySecs = 60 * 60 * 24
  
  def toMoneyString = {
    String.format( "$%2.2f", double2Double( d ) )
  }
  
  def toFormat( fmt:String ) = {
    String.format( fmt, double2Double( d ) )
  }
  
  def toTimeFormat = {
    val days = ( d / OneDaySecs )._i
    val hours = ( ( d % OneDaySecs ) / 3600 )._i
    val minutes = ( ( d % 3600 ) / 60 )._i
    val seconds = ( d % 60 )._i

    if ( days > 0 )
      daysFormat.format( days, hours, minutes, seconds )
    else if ( hours > 0 )
      hoursFormat.format( hours, minutes, seconds )
    else 
      minutesFormat.format( minutes, seconds )
  }
}




