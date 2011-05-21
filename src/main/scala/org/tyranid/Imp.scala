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


/**
 * IMPlicit IMPorts.
 */
object Imp {

  def spam( msg:String ) = println( "SPAM: " + msg )

  def log( msg:String = "", exception:Exception = null ) = org.tyranid.log.Log.log( msg, exception )

	implicit def boolean( v:Boolean )     = new org.tyranid.logic.BooleanImp( v )
	implicit def option[A]( v:Option[A] ) = new org.tyranid.collection.OptionImp( v )
	implicit def string( v:String )       = new org.tyranid.text.StringImp( v )
	implicit def symbol( v:Symbol )       = v.name
}

