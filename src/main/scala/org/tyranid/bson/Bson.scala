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

import java.util.Date

import org.bson.types.ObjectId
import com.mongodb.{ BasicDBList, DBObject }

import org.tyranid.Imp._
import org.tyranid.any.Deep
import org.tyranid.db.mongo.Imp._


/**
 * Represents either BsonObjects or things that can behave like a BsonObject.
 */
trait BsonObject extends Deep {
  def has( key:String ):Boolean
  def apply( key:String ):AnyRef
  def update( key:String, v:Any ):Unit

  def deep:BsonObject

  def id = apply( "_id" )
  def oid = id.as[ObjectId]

  def a_?( key:String ) =
    apply( key ) match {
    case null          => Mongo.EmptyArray
    case a:BasicDBList => a
    }
  def a( key:String )         = apply( key ).asInstanceOf[BasicDBList]
  def b( key:String ):Boolean =
    apply( key ) match {
    case b:java.lang.Boolean => b
    case s:String            => s.toLaxBoolean
    case null                => false
    }
  def d( key:String )         = 
    apply( key ) match {
    case i:java.lang.Integer => i.doubleValue
    case d:java.lang.Number => d.doubleValue
    case s:String => s.toLaxDouble
    case null => 0
    }
  def i( key:String )         =
    apply( key ) match {
    case i:java.lang.Integer => i.intValue
    case s:String => s.toLaxInt
    case null => 0
    }
  def l( key:String )         =
    apply( key ) match {
    case n:java.lang.Number => n.longValue
    case s:String           => s.toLaxLong
    case null => 0
    }
  def o( key:String )         = apply( key ).asInstanceOf[BsonObject]
  def oid( key:String )       = apply( key ).asInstanceOf[ObjectId]
  //def r( key:String )       = apply( key ).asInstanceOf[Long]
  def s( key:String ):String = {
    val v = apply( key )
    if ( v != null ) v.toString else ""
  }
  def t( key:String )         =
    apply( key ) match {
    case d:Date   => d
    case s:String => s.toLaxDate // TODO:  replace with more generic parsing method
    case null     => null
    }


  /*
   * * *  Option-variants
   */

  def opta( key:String ):Option[BasicDBList] =
    apply( key ) match {
    case null          => None
    case l:BasicDBList => Some( l )
    }

  def optb( key:String ):Option[Boolean] =
    apply( key ) match {
    case null                => None
    case b:java.lang.Boolean => Some( b )
    case s:String            => Some( s.toLaxBoolean )
    }

  def optd( key:String ):Option[Double] =
    apply( key ) match {
    case null                => None
    case d:java.lang.Double  => Some( d )
    case n:Number            => Some( n.doubleValue )
    case s:String            => Some( s.toLaxDouble )
    }

  def opti( key:String ):Option[Int] =
    apply( key ) match {
    case null                 => None
    case i:java.lang.Integer  => Some( i )
    case n:Number             => Some( n.intValue )
    case s:String             => Some( s.toLaxInt )
    }

  def optl( key:String ):Option[Long] =
    apply( key ) match {
    case null              => None
    case l:java.lang.Long  => Some( l )
    case n:Number          => Some( n.longValue )
    case s:String          => Some( s.toLaxLong )
    }

  def opto( key:String ):Option[DBObject] =
    apply( key ) match {
    case null       => None
    case o:DBObject => Some( o )
    }

  def opts( key:String ):Option[String] =
    apply( key ) match {
    case null      => None
    case s:String  => Some( s )
    case o         => Some( o.toString )
    }

  def optt( key:String ):Option[Date] =
    apply( key ) match {
    case null    => None
    case t:Date  => Some( t )
    }
}

trait BsonList extends BsonObject with Seq[Any] {

  def apply( idx:Int ):AnyRef
  def update( idx:Int, v:Any ):Unit

  def deep:BsonList


  def a( idx:Int )         = apply( idx ).asInstanceOf[BasicDBList]
  def b( idx:Int )         = apply( idx ).asInstanceOf[Boolean]
  def d( idx:Int )         = apply( idx ).asInstanceOf[Double]
  def i( idx:Int )         = apply( idx ).asInstanceOf[Int]
  def l( idx:Int )         = apply( idx ).asInstanceOf[Long]
  def o( idx:Int )         = apply( idx ).asInstanceOf[BsonObject]
  def oid( idx:Int )       = apply( idx ).asInstanceOf[ObjectId]
  //def r( idx:Int )       = apply( idx ).asInstanceOf[Long]
  def s( idx:Int ):String = {
    val v = apply( idx )
    if ( v != null ) v.toString else ""
  }
  def t( idx:Int )         = apply( idx ).asInstanceOf[Date]
}

