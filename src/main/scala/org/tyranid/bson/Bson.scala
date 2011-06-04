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

package org.tyranid.bson

import org.bson.types.ObjectId


/**
 * Represents either BsonObjects or things that can behave like a BsonObject.
 */
trait BsonObject {
  def apply( key:String ):AnyRef
  def update( key:String, v:AnyRef ):Unit

  def id = apply( "id" )

  //def a( key:String )       = apply( key ).asInstanceOf[Array]
  def o( key:String )         = apply( key ).asInstanceOf[BsonObject]
  def b( key:String )         = apply( key ).asInstanceOf[Boolean]
  def d( key:String )         = apply( key ).asInstanceOf[Double]
  def i( key:String )         = apply( key ).asInstanceOf[Int]
  def l( key:String )         = apply( key ).asInstanceOf[Long]
  def oid( key:String )       = apply( key ).asInstanceOf[ObjectId]
  //def r( key:String )       = apply( key ).asInstanceOf[Long]
  def s( key:String ):String = {
    val v = apply( key )
    if ( v != null ) v.toString else ""
  }
  //def d( key:String ) = apply( key ).toString
}


