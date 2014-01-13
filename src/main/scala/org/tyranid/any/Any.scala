/**
 * Copyright (c) 2008-2013 Tyranid <http://tyranid.org>
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

import scala.collection.JavaConversions._
import scala.collection.mutable.LinkedHashMap

import org.bson.types.ObjectId
import com.mongodb.DBObject

import org.tyranid.Imp._


class AnyImp[T <: Any]( v:T ) {

  /**
   * null-safe toString().  Need a better name.
   */
  def safeString:String =
    if ( v != null ) v.toString
    else             ""

  def is[U:Manifest] = manifest[U].runtimeClass.isAssignableFrom( v.getClass )
  def as[U]          = v.asInstanceOf[U]

  def toJsonStr( pretty:Boolean = false, client:Boolean = false ) = new org.tyranid.json.JsonString( v, pretty = pretty, client = client ).toString

  def _b:Boolean =
    v match {
    case b:java.lang.Boolean => b
    case s:String            => s.toLaxBoolean
    case d:java.lang.Number  => d.doubleValue != 0
    case null                => false
    }

  def _d = 
    v match {
    case d:java.lang.Number  => d.doubleValue
    case s:String            => s.toLaxDouble
    case null                => 0
    }

  def _i =
    v match {
    case n:java.lang.Number  => n.intValue
    case s:String            => s.toLaxInt
    case null                => 0
    }

  def _l =
    v match {
    case n:java.lang.Number => n.longValue
    case s:String           => s.toLaxLong
    case null               => 0
    }

  def _t =
    v match {
    case d:Date   => d
    case s:String => s.parseDate()
    case l:Long   => new Date( l )
    case null     => null
    }

  def _s = safeString
  def _oid:ObjectId = 
    v match {
    case oid:ObjectId => oid
    case s:String => new ObjectId( s ) 
    case null     => null
    }

  def asJsonObject = if ( v != null ) v.as[LinkedHashMap[String,Any]] else null

  def toDBObject:AnyRef =
    v match {
    case null =>
      null

    case arr:Array[_] =>
      arr.toMlist // TODO:  this needs to be recursive

    case obj:java.util.LinkedHashMap[_,_] =>
      var map:collection.Map[String,Any] = obj.as[java.util.LinkedHashMap[String,Any]];
      obj.as[Map[String,Any]].toDBObject

    case map:scala.collection.Map[_,_] =>
      // this doesn't work because it doesn't convert nested arrays (and other thing?) properly
      //new com.mongodb.BasicDBObject( map )

      val dbobj = new com.mongodb.BasicDBObject()

      for ( entry <- map;
            key   = entry._1;
            value = entry._2 )
        dbobj.append( key._s, value.toDBObject )

      dbobj

    case _ =>
      v
    }
}

