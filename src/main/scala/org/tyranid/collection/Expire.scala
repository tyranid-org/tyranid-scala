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


class Expire[ V >: Null <: AnyRef ]( expireMs:Long, block: () => V ) {

	private var value:V = null
	private var lastMs:Long = 0

	def get:V = {

		val nowMs = System.currentTimeMillis

		if ( nowMs - expireMs > lastMs ) {
			value = block()
			lastMs = nowMs
		}

		value
	}

	def clear { lastMs = 0 }
}

class ConcurrentExpire[ V >: Null <: AnyRef ]( expireMs:Long, block: () => V ) {

	private var value:V = null
	private var lastMs:Long = 0

	def get:V = synchronized {
		val nowMs = System.currentTimeMillis

		if ( nowMs - expireMs > lastMs ) {
			value = block()
			lastMs = nowMs
		}

		value
	}

	def clear = synchronized { lastMs = 0 }
}

class ConcurrentExpireMap[ K <: Any, V >: Null <: AnyRef ]( expireMs:Long ) {

	private case class Slot( var lastMs:Long, var value:V )

	private val map = new collection.mutable.HashMap[K,Slot]

  def contains( key:K ) = synchronized { map.contains( key ) }

	def getOrElseUpdate( key:K, value: => V ):V = synchronized {
		val nowMs = System.currentTimeMillis

		map.get( key ) match {
		case Some( slot ) =>
			if ( nowMs - expireMs > slot.lastMs ) {
				slot.value = value
				slot.lastMs = nowMs
			}

			slot.value

		case None =>
			val slot = new Slot( nowMs, value )
			map( key ) = slot
			slot.value
		}
	}

  def get( key:K ) = synchronized { map.get( key ).map( _.value ) }

	def update( key:K, value:V ) = synchronized {
    map( key ) = new Slot( System.currentTimeMillis, value )
  }

	def clear = synchronized { map.clear }
}

class ConcurrentExpireAutoMap[ K <: Any, V >: Null <: AnyRef ]( expireMs:Long, block: ( K ) => V ) {

	private case class Slot( var lastMs:Long, var value:V )

	private val map = new collection.mutable.HashMap[K,Slot]
	private var lastMs:Long = 0

	def apply( key:K ):V = synchronized {
		val nowMs = System.currentTimeMillis

		map.get( key ) match {
		case Some( slot ) =>
			if ( nowMs - expireMs > slot.lastMs ) {
				slot.value = block( key )
				slot.lastMs = nowMs
			}

			slot.value

		case None =>
			val slot = new Slot( nowMs, block( key ) )
			map( key ) = slot
			slot.value
		}
	}

  def check( key:K ) = synchronized { map.get( key ).map( _.value ) }
	def clear( key:K ) = synchronized { map.get( key ).foreach( _.lastMs = 0 ) }
	def clear          = synchronized { map.clear }
}

