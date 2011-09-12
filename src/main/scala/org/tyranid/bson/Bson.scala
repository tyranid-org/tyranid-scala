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
import com.mongodb.BasicDBList

import org.tyranid.Imp._


/**
 * Represents either BsonObjects or things that can behave like a BsonObject.
 */
trait BsonObject {
  def has( key:String ):Boolean
  def apply( key:String ):AnyRef
  def update( key:String, v:Any ):Unit

  def id = apply( "_id" )

  def a( key:String )         = apply( key ).asInstanceOf[BasicDBList]
  def o( key:String )         = apply( key ).asInstanceOf[BsonObject]
  def b( key:String )         = apply( key ).asInstanceOf[Boolean]
  def d( key:String )         = apply( key ).asInstanceOf[Double]
  def i( key:String )         =
    apply( key ) match {
    case i:java.lang.Integer => i.intValue
    case s:String => s.toLaxInt
    case null => 0
    }
  def l( key:String )         = apply( key ).asInstanceOf[Long]
  def oid( key:String )       = apply( key ).asInstanceOf[ObjectId]
  //def r( key:String )       = apply( key ).asInstanceOf[Long]
  def s( key:String ):String = {
    val v = apply( key )
    if ( v != null ) v.toString else ""
  }
  //def d( key:String ) = apply( key ).toString
}

trait BsonList extends BsonObject with Seq[Any] {

  def apply( idx:Int ):AnyRef
  def update( idx:Int, v:Any ):Unit


  def a( idx:Int )         = apply( idx ).asInstanceOf[BasicDBList]
  def o( idx:Int )         = apply( idx ).asInstanceOf[BsonObject]
  def b( idx:Int )         = apply( idx ).asInstanceOf[Boolean]
  def d( idx:Int )         = apply( idx ).asInstanceOf[Double]
  def i( idx:Int )         = apply( idx ).asInstanceOf[Int]
  def l( idx:Int )         = apply( idx ).asInstanceOf[Long]
  def oid( idx:Int )       = apply( idx ).asInstanceOf[ObjectId]
  //def r( idx:Int )       = apply( idx ).asInstanceOf[Long]
  def s( idx:Int ):String = {
    val v = apply( idx )
    if ( v != null ) v.toString else ""
  }
  //def d( idx:Int ) = apply( idx ).toString
}

