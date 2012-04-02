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

package org.tyranid.collection

import org.tyranid.Imp._


class SeqImp[A]( seq:Seq[A] ) {

  /**
   * Like Seq.groupBy() except this requires that the underlying data is a
   * sequence of ( K, V ) tuples.  This is a grouped-version of toMap().
   */
  def group[K,V]( implicit ev: A <:< (K, V) ):Map[K,Seq[V]] =
    // TODO:  implement more efficiently
    seq.groupBy( _._1 ).mapValues( _.map( _._2 ) )

  /**
   * Like Seq.groupBy() except that you can specify both the key and the value.
   */
  def groupBy2[K,V]( kf: ( A ) => K, vf: ( A ) => V ):Map[K,Seq[V]] =
    // TODO:  implement more efficiently
    seq.map( a => ( kf( a ), vf( a ) ) ).group

  /**
   * This filters the sequence of A to members of subtype B and returns a Seq of
   * subtype B.
   *
   * equivalent to:  seq.filter( _.isInstanceOf[B] ).map( _.asInstanceOf[B] )
   */
  def of[ B <: A : Manifest ]:Seq[B] = {
    val cls = manifest[B].erasure
    if ( cls.isPrimitive && cls == classOf[Int] ) {
      seq.flatMap(
        _ match {
        case i:Int     => Some( i )
        case i:Integer => Some( i.intValue )
        case _         => None
        }
      ).as[Seq[B]]
    } else {
      seq.filter( obj => cls.isAssignableFrom( obj.getClass ) ).map( _.asInstanceOf[B] )
    }
  }

  def findOf[ B <: A : Manifest ]:Option[B] = {
    val cls = manifest[B].erasure
    seq.find( obj => cls.isAssignableFrom( obj.getClass ) ).map( _.asInstanceOf[B] )
  }

  def whileDo( _while: (A) => Boolean )( _do: (A) => Unit ) {

    for ( a <- seq ) {
      if ( !_while( a ) )
        return

      _do( a )
    }
  }
}




