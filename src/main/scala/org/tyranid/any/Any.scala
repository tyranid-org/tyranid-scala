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

package org.tyranid.any

import java.util.Date

import scala.collection.mutable.LinkedHashMap

import org.tyranid.Imp._


class AnyImp[T <: Any]( v:T ) {

  def is[U] = v.isInstanceOf[U]
  def as[U] = v.asInstanceOf[U]

  def toJsonStr = new org.tyranid.json.JsonString( v ).toString

  def coerceBoolean:Boolean =
    v match {
    case b:java.lang.Boolean => b
    case s:String            => s.toLaxBoolean
    case null                => false
    }

  def coerceDouble = 
    v match {
    case i:java.lang.Integer => i.doubleValue
    case d:java.lang.Number  => d.doubleValue
    case s:String            => s.toLaxDouble
    case null                => 0
    }

  def coerceInt =
    v match {
    case i:java.lang.Integer => i.intValue
    case n:java.lang.Number  => n.intValue
    case s:String            => s.toLaxInt
    case null                => 0
    }

  def coerceLong =
    v match {
    case n:java.lang.Number => n.longValue
    case s:String           => s.toLaxLong
    case null               => 0
    }

  def coerceDate =
    v match {
    case d:Date   => d
    case s:String => s.toLaxDate // TODO:  replace with more generic parsing method
    case null     => null
    }

  def coerceString =
    if ( v != null ) v.toString else ""

  def asJsonObject =
    if ( v != null ) v.as[LinkedHashMap[String,Any]] else null
}

