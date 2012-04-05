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

class OptionImp[A]( opt:Option[A] ) {

	def flatten[B]( getter: ( A ) => B, fallback: => B ) = opt match {
		case Some( obj ) => getter( obj )
		case None        => fallback
	}

  /**
   * This filters the sequence of A to members of subtype B and returns a Seq of
   * subtype B.
   *
   * equivalent to:  seq.filter( _.isInstanceOf[B] ).map( _.asInstanceOf[B] )
   */
  def of[ B <: A : Manifest ]:Option[B] = {
    val cls = manifest[B].erasure
    opt.filter( obj => cls.isAssignableFrom( obj.getClass ) ).map( _.asInstanceOf[B] )
  }

}

