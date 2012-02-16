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

package org.tyranid.math

import org.tyranid.Imp._

case class LongImp( l:Long ) {
  val kb:Long = Math.pow( 2.toDouble, 10.toDouble ).toLong
  val mb:Long = Math.pow( 2.toDouble, 20.toDouble ).toLong
  val gb:Long = Math.pow( 2.toDouble, 30.toDouble ).toLong
  val tb:Long = Math.pow( 2.toDouble, 40.toDouble ).toLong
  
  def toBytesString = {
    if ( l >= tb )
      String.format( "%2.2d terabytes", double2Double( ( l / tb ).asInstanceOf[Double] ) )
    else if ( l >= gb ) 
      String.format( "%1.2d gigabytes", double2Double( ( l / gb ).asInstanceOf[Double] ) )
    else if ( l >= mb ) 
      String.format( "%1.2d megabytes", double2Double( ( l / mb ).asInstanceOf[Double] ) )
    else if ( l >= kb )
      String.format( "%1.2d kilobytes", double2Double( ( l / kb ).asInstanceOf[Double] ) )
    else 
      l + " bytes"
  }
  
  def toBytesAbbrString = {
    println( "l=" + l + ", tb = " + tb )
    
    if ( l >= tb )
      String.format( "%2.2d TB", double2Double( ( l / tb ).asInstanceOf[Double] ) )
    else if ( l >= gb ) 
      String.format( "%1.2d GB", double2Double( ( l / gb ).asInstanceOf[Double] ) )
    else if ( l >= mb ) 
      String.format( "%1.2d MB", double2Double( ( l / mb ).asInstanceOf[Double] ) )
    else if ( l >= kb )
      String.format( "%1.2d KB", double2Double( ( l / kb ).asInstanceOf[Double] ) )
    else 
      l + " B"
  }
  
  def pow( p:Int ) = Math.pow( l.toDouble, p.toDouble ).toLong
}



