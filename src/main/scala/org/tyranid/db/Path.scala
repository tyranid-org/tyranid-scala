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

package org.tyranid.db

import java.util.Date

import scala.annotation.tailrec

import org.bson.types.ObjectId
import com.mongodb.{ BasicDBList, BasicDBObject, DBObject }

import scala.collection.mutable
import scala.xml.NodeSeq

import org.tyranid.Imp._
import org.tyranid.bson.BsonObject
import org.tyranid.db.es.Es
import org.tyranid.db.mongo.MongoEntity
import org.tyranid.db.mongo.Imp._
import org.tyranid.logic.{ Invalid, Valid }
import org.tyranid.ui.{ UiObj }


/*
 * * *  Path
 */

/*
    TODO:  eliminate the call to reverse() and instead always store Paths in reverse order so that
           common subpaths can be shared?  will save a lot of space when doing operations like Diff.
 */


trait Pathable {
  def path:Path
}

trait PathNode {
  def name:String
  def label:String
}

case class ArrayIndex( idx:Int ) extends PathNode {

  def name  = idx.toString
  def label = name
}

trait Path extends Pathable {

  def path = this

  def name  = ( 0 until pathSize ).map( i => pathAt( i ).name ).mkString( "." )
  def label = ( 0 until pathSize ).map( i => pathAt( i ).label ).mkString( " . " )

  def pathSize:Int
  def pathAt( idx:Int ):PathNode

  def leaf:ViewAttribute = pathAt( pathSize - 1 ).asInstanceOf[ViewAttribute]

  def matches( other:Seq[String], ostart:Int = 0 ) = {

    !( ostart until other.size ).exists( i => other( i ) != pathAt( i-ostart ).name )
  }

  def pathName = name
}

case class MultiPath( nodes:PathNode* ) extends Path {

  def pathSize = nodes.length
  def pathAt( idx:Int ) = nodes( idx )

  override lazy val pathName = nodes.map( _.name ).mkString( "_" )
}

object PathValue {

  implicit val order = new Ordering[PathValue] {
    def compare( a:PathValue, b:PathValue ) = Path.order.compare( a.path, b.path )
  }

  def fromDbObject( root:View, obj:DBObject ):Iterable[PathValue] = {
    import scala.collection.JavaConversions._

    for ( key <- obj.keySet;
          if key != "_id" )
      yield PathValue( Path.parse( root, key ), obj( key ) )
  }

  def toDbObject( pathValues:Iterable[PathValue] ):DBObject = {
    val obj = new BasicDBObject

    for ( pv <- pathValues )
      obj( pv.path.pathName ) = pv.value
      
    obj
  }
}

case class PathValue( path:Path, value:Any ) extends Pathable {
  override def toString = path.name + "=" + value.toString
}

object PathDiff {

  def fromDbObject( root:View, obj:DBObject ):Iterable[PathDiff] = {
    import scala.collection.JavaConversions._

    for ( key <- obj.keySet;
          if key != "_id";
          diffs = obj.o( key ) )
      yield PathDiff( Path.parse( root, key ), diffs( 'a ), diffs( 'b ) )
  }

  def toDbObject( pathDiffs:Iterable[PathDiff] ):DBObject = {
    val obj = new BasicDBObject

    for ( pd <- pathDiffs )
      obj( pd.path.pathName ) = Mobj( "a" -> pd.a, "b" -> pd.b )
      
    obj
  }
}

case class PathDiff( path:Path, a:Any, b:Any ) extends Pathable {
  override def toString = path.name + ": " + a.toString + " => " + b.toString
}

object Path {

  implicit val order = new Ordering[Path] {
    def compare( a:Path, b:Path ):Int = {
      @tailrec
      def compare0( i:Int ):Int = {
        if ( i >= a.pathSize ) {
          if ( i >= b.pathSize )
            0
          else
            -1
        } else if ( i >= b.pathSize ) {
          1
        } else {
          a.pathAt( i ).name.compareTo( b.pathAt( i ).name ) match {
          case n if n != 0 => n
          case _           => compare0( i+1 )
          }
        }
      }

      compare0( 0 )
    }
  }

  def parse( root:View, path:String ):Path = {

    val names =
      path.split(
        if ( path.indexOf( '.' ) != -1 ) "\\."
        else                             "_" )

    val nlen = names.length

    if ( nlen == 1 ) {
      root( names( 0 ) )
    } else {
      val pbuf = new Array[PathNode]( nlen )

      var view = root

      for ( ni <- 0 until nlen ) {
        names( ni ) match {
        case s if s.isInt =>
          pbuf( ni ) = ArrayIndex( s.toInt )

        case s =>
          val va = view( s )
          pbuf( ni ) = va

          if ( ni+1 < nlen )
            view = va.toView
        }
      }

      MultiPath( pbuf:_* )
    }
  }

  def flatten( rec:Record ):Seq[PathValue] = {
    var pathValues:List[PathValue] = Nil

    @tailrec
    def findva( path:List[PathNode] ):ViewAttribute =
      path.head match {
      case va:ViewAttribute => va
      case _                => findva( path.tail )
      }

    def record( path:List[PathNode], rec:Record ) {
      for ( va <- rec.view.vas ) {
        va.att.domain match {
        case en:Entity   => record( va :: path, rec.rec( va ) )
        case arr:DbArray => array( va :: path, rec.a( va ) )
        case dom         => simple( va :: path, rec( va ) )
        }
      }
    }

    def array( path:List[PathNode], arr:BasicDBList ) {
      if ( arr == null || arr.size == 0 ) {
        Nil
      } else {
        val va = findva( path )
        val dom = va.att.domain.asInstanceOf[DbArray].of

        for ( i <- 0 until arr.size ) {
          val ipath = ArrayIndex( i ) :: path
          val value = arr( i )

          dom match {
          case en:MongoEntity => record( ipath, en.recify( arr( 0 ), parent = null /* TODO:  should pass in parent ? */, rec => arr( 0 ) = rec ) )
          case arr:DbArray    => array( ipath, value.asInstanceOf[BasicDBList] )
          case dom            => simple( ipath, value )
          }
        }
      }
    }

    def simple( path:List[PathNode], value:Any ) {
      val va = findva( path )
      val a  = va.att
      val d  = a.domain

      if ( d.isSet( value ) ) {
        pathValues ::= PathValue( MultiPath( path.reverse:_* ), value )

        if ( d.isInstanceOf[DbLink] ) {
          // TODO: recurse the link
        }
      }
    }

    record( Nil, rec )
    pathValues
  }

  case class Diff( as:Seq[PathValue],
                   bs:Seq[PathValue],
                   diffs:Seq[PathDiff] ) {
    def nonEmpty = as.nonEmpty || bs.nonEmpty || diffs.nonEmpty
  }

  def diff( a:Record, b:Record ):Diff = {
    val al = flatten( a ).sorted
    val bl = flatten( b ).sorted

    val as    = mutable.ArrayBuffer[PathValue]()
    val bs    = mutable.ArrayBuffer[PathValue]()
    val diffs = mutable.ArrayBuffer[PathDiff]()

    @tailrec
    def diff0( ai:Int, bi:Int ) {
      if ( ai < al.size && bi < bl.size ) {
        val apv = al( ai )
        val bpv = bl( bi )

        Path.order.compare( apv.path, bpv.path ) match {
        case i if i < 0 =>
          as += apv
          diff0( ai+1, bi )
        case i if i > 0 =>
          bs += bpv
          diff0( ai, bi+1 )
        case 0 =>
          if ( apv.value != bpv.value )
            diffs += PathDiff( apv.path, apv.value, bpv.value )

          diff0( ai+1, bi+1 )
        }
      } else if ( ai < al.size ) {
        as += al( ai )
        diff0( ai+1, bi )
      } else if ( bi < bl.size ) {
        bs += bl( bi )
        diff0( ai, bi+1 )
      }
    }

    diff0( 0, 0 )
    Diff( as, bs, diffs )
  }
}

