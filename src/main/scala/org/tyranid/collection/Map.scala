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

package org.tyranid.collection

import org.tyranid.Imp._


object ObjectMapImp {
  val EmptyAnyRefArray = new Array[AnyRef]( 0 )
}

class ObjectMapImp[A]( map:scala.collection.Map[String,Any] ) {

  def a( key:String ) =
    map.get( key ) match {
    case Some( v ) => v.as[Array[AnyRef]]
    case None      => null
    }

  def a_?( key:String ) =
    map.get( key ) match {
    case Some( v ) => v.as[Array[AnyRef]]
    case None      => ObjectMapImp.EmptyAnyRefArray
    }

  def b( key:String ) =
    map.get( key ) match {
    case Some( v ) => v.coerceBoolean
    case None      => false
    }

  def d( key:String ) =
    map.get( key ) match {
    case Some( v ) => v.coerceDouble
    case None      => 0.0
    }

  def i( key:String ) =
    map.get( key ) match {
    case Some( v ) => v.coerceInt
    case None      => 0
    }

  def l( key:String ) =
    map.get( key ) match {
    case Some( v ) => v.coerceLong
    case None      => 0L
    }

  def s( key:String ) =
    map.get( key ) match {
    case Some( v ) => v.coerceString
    case None      => ""
    }

  def t( key:String ) =
    map.get( key ) match {
    case Some( v ) => v.coerceDate
    case None      => null
    }
}

