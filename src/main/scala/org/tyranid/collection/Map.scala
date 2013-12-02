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

package org.tyranid.collection

import scala.collection.JavaConversions._

import org.tyranid.Imp._


object ObjectMapImp {
  val EmptyObject = scala.collection.immutable.Map[String,Any]()
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

  def o( key:String ) =
    map.get( key ) match {
    case Some( v ) => v.as[ObjectMap]
    case None      => null
    }

  def o_?( key:String ) =
    map.get( key ) match {
    case Some( v ) => v.as[ObjectMap]
    case None      => ObjectMapImp.EmptyObject
    }

  def b( key:String ) =
    map.get( key ) match {
    case Some( v ) => v._b
    case None      => false
    }

  def d( key:String ) =
    map.get( key ) match {
    case Some( v ) => v._d
    case None      => 0.0
    }

  def i( key:String ) =
    map.get( key ) match {
    case Some( v ) => v._i
    case None      => 0
    }

  def l( key:String ) =
    map.get( key ) match {
    case Some( v ) => v._l
    case None      => 0L
    }

  def s( key:String ) =
    map.get( key ) match {
    case Some( v ) => v._s
    case None      => ""
    }

  def t( key:String ) =
    map.get( key ) match {
    case Some( v ) => v._t
    case None      => null
    }

  def toDBObject =
    if ( map == null ) {
      null
    } else {
      // this doesn't work because it doesn't convert nested arrays (and other thing?) properly
      //new com.mongodb.BasicDBObject( map )

      val dbobj = new com.mongodb.BasicDBObject()

      for ( entry <- map;
            key   = entry._1;
            value = entry._2 ) {

        value match {
        case arr:Array[_] =>
          dbobj.append( key, arr.toMlist )

        case _ =>
          dbobj.append( key, value )
        }
      }

      dbobj
    }
}

