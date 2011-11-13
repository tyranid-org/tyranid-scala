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

import java.util.{ Date }

object Time {

  val OneMinuteMs   =           60 * 1000
  val FiveMinutesMs =       5 * 60 * 1000
  val HalfHourMs    =      30 * 60 * 1000
  val OneDayMs      = 24 * 60 * 60 * 1000
  
  val DateFormat = new java.text.SimpleDateFormat( "MM/dd/yyyy" )
  val DateTimeFormat = new java.text.SimpleDateFormat( "MM/dd/yyyy HH:mm:ss" )
  

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

