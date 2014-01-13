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

import org.tyranid.Imp._

import scala.collection.JavaConversions._


class AnyRefImp[T <: AnyRef]( ref:T ) {

  /**
   * Elvis operator.
   */
  def ?|( default: => T ):T =
    if ( ref != null ) ref
    else               default

  def ?*[ A >: Null <: AnyRef ]( block: (T) => A ):A =
    if ( ref != null ) block( ref )
    else               null

  def toDBObject:AnyRef =
    ref match {
    case null =>
      null

    case arr:Array[_] =>
      arr.toMlist

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
        dbobj.append( key._s, value.asInstanceOf[AnyRef].toDBObject )

      dbobj

    case _ =>
      ref
    }
}

