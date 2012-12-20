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

package org.tyranid.bson

import java.util.Date

import scala.collection.mutable

import org.bson.types.ObjectId
import com.mongodb.{ BasicDBObject, BasicDBList, DBObject }

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

  def isNew:Boolean = has( "_id" )

  def copy( other:BsonObject ) =
    for ( key <- other.keys )
      update( key, other( key ) )

  def keys:Seq[String]

  def id = apply( "_id" )
  def oid = id.as[ObjectId]

  @deprecated(
    message = "Use removeKey()/removeValue() instead since remove() has a conflict:  DbListImp extends from both BsonObject ( remove( Any ) means removing a KEY ) and java.util.ArrayList ( remove( Any ) means removing a VALUE",
    since = "2012.12.18" )
  def remove( key:String )

  def removeKey( key:String )

  def removeValue( value:Any ) =
    for ( key <- keys;
          if apply( key ) == value )
      removeKey( key )

  def tidFor( key:String,  en:org.tyranid.db.Entity ) = {
    a_?( key ).find( t => en.hasTid( t._s ) ).getOrElse( "" )._s
  }
  
  // return list or empty guard array
  def a_?( key:String ) =
    apply( key ) match {
    case null          => Mongo.EmptyArray
    case a:BasicDBList => a
    }
  
  // return mutable array (even if empty)
  def a_!( key:String ) =
    apply( key ) match {
    case null          => val a = new BasicDBList
                          update( key, a )
                          a
    case a:BasicDBList => a
    }
  
  def a( key:String )   = apply( key ).asInstanceOf[BasicDBList]

  def b( key:String )   = apply( key )._b
  def d( key:String )   = apply( key )._d
  def i( key:String )   = apply( key )._i
  def l( key:String )   = apply( key )._l
  //def r( key:String ) = apply( key ).asInstanceOf[Long]
  def s( key:String )   = apply( key )._s
  def t( key:String )   = apply( key )._t

  def o_?( key:String ):BsonObject = {
    apply( key ) match {
    case null            => Mongo.EmptyObject
    case bo:BsonObject   => bo
    case o:DBObject      => o
    }
  }
  def o_!( key:String ):BsonObject =
    apply( key ) match {
    case null            => val o = new BasicDBObject
                            update( key, o )
                            o
    case bo:BsonObject   => bo
    case o:DBObject      => o
    }
  def o( key:String )   = apply( key ).asInstanceOf[BsonObject]

  def oid( key:String ):ObjectId =
    apply( key ) match {
    case id:ObjectId =>
      id

    case obj:BasicDBObject =>
      obj.oid( "_id" )

    case null =>
      null
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

  def toPretty( markup:Boolean = false ) = {
    val sb = new StringBuilder
    var depth = 0

    def m( s:String, cls:String ) =
      if ( markup )
        "<span class=\"bson" + cls + "\">" + s + "</span>"
      else
        s

    def enter( v:Any ) {
      v match {
      case a:org.bson.types.BasicBSONList =>
        sb ++= m( "[", "Bracket" )

        if ( a.size > 0 ) {
          depth += 1
          var first = true

          sb += '\n'
          for ( i <- 0 until a.size ) {
            if ( first )
              first = false
            else
              sb ++= ",\n"

            sb ++= ( "  " * depth )
            enter( a.get( i ) )
          }

          depth -= 1
          sb += '\n' ++= ( "  " * depth )
        }
        
        sb ++= m( "]", "Bracket" )

      case o:DBObject =>
        sb ++= m( "{", "Brace" )
        val keys = o.keys.sorted

        if ( keys.size > 0 ) {
          depth += 1
          var first = true
          sb += '\n'

          for ( k <- o.keys.sorted ) {
            if ( first )
              first = false
            else
              sb ++= ",\n"
          
            sb ++= ( "  " * depth ) ++= m( k, "Key" ) ++= ": "
            enter( o( k ) )
          }

          depth -= 1
          sb += '\n' ++= ( "  " * depth )
        }
        
        sb ++= m( "}", "Brace" )

      case null =>
        sb ++= m( "null", "Literal" )

      case b:Boolean =>
        sb ++= m( if ( b ) "true" else "false", "Literal" )

      case t:Date =>
        sb ++= m( "ISODate(\"", "Type" ) ++= m( t.toIso8601, "String" ) ++= m( "\")", "Type" )

      case s:String =>
        sb ++= m( "\"" + s.encJson.encHtml + '"', "String" )

      case i:Integer =>
        sb ++= m( "NumberInt(", "Type" ) ++= i.toString ++= m( ")", "Type" )

      case l:java.lang.Long =>
        sb ++= m( "NumberLong(", "Type" ) ++= l.toString ++= m( ")", "Type" )

      case oid:org.bson.types.ObjectId =>
        sb ++= m( "ObjectId('", "Type" ) ++= oid.toString ++= m( "')", "Type" )

      case v =>
        sb ++= v.toString
      }
    }

    enter( this )
    sb.toString
  }
}

trait BsonList extends BsonObject with mutable.Seq[Any] {

  def apply( idx:Int ):AnyRef
  def update( idx:Int, v:Any ):Unit

  def deep:BsonList

  def rollRight( value:Any, size:Int ) {
    val esize = this.size

    for ( i <- ( size min esize ) until 0 by -1 )
      update( i, apply( i-1 ) )

    update( 0, value )
    truncate( size )
  }

  def truncate( newSize:Int ) =
    for ( i <- newSize until this.size )
      removeKey( i.toString )

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

