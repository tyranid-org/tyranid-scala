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

package org.tyranid.math

import org.tyranid.Imp._


/**
 * Base36 number support.
 */
object Base36 {

  def make( len:Int ) = {
    def digit = {
      ( ( math.random * 36 ).toInt match {
      case n if n < 10 => '0'.toInt + n
      case n           => 'a'.toInt + n - 10
      } ).toChar
    }
    
    val sb = new StringBuilder
    for ( i <- 1 to len )
      sb += digit
        
    sb.toString
  }

	def toLong( text:String ) = {
		var value = 0L
		var negative = false
		
		for ( i <- 0 until text.length ) {
			val ch = text.charAt( i )

      if ( ch == '-' && i == 0 ) {
        negative = true
      } else {
			  val digit =
			    if      ( ch >= '0' && ch <= '9' ) ch - '0'
			    else if ( ch >= 'A' && ch <= 'Z' ) ch - 'A' + 10
			    else if ( ch >= 'a' && ch <= 'z' ) ch - 'a' + 10
			    else
				    throw new IllegalArgumentException(
					    "Illegal base-36 character '" + ch + "' in \"" + text + "\"." )

			  value = 36 * value + digit
      }
		}
		
		if ( negative ) -value else value
	}
	
	def toString( value:Long ) = {
		val sb = new StringBuilder( 8 )
    var v = value
		
		val negative =
		  if ( v < 0 ) {
			  v *= -1
			  true
		  } else {
			  false
		  }
		
		do {
			val digit = ( v % 36 )

			val ch =
			  if ( digit < 10 ) '0' + digit
			  else              'A' + digit - 10
		
			sb += ch.asInstanceOf[Char]
			v /= 36
		} while ( v != 0 )
		
		if ( negative )
			sb += '-'
		
		sb.reverse.toString
	}

  private val BigInt36 = BigInt( 36 )

	def toBigInt( text:String ) = {
		var value = BigInt( 0 )
		var negative = false
		
		for ( i <- 0 until text.length ) {
			val ch = text.charAt( i )

      if ( ch == '-' && i == 0 ) {
        negative = true
      } else {
			  val digit =
			    if      ( ch >= '0' && ch <= '9' ) ch - '0'
			    else if ( ch >= 'A' && ch <= 'Z' ) ch - 'A' + 10
			    else if ( ch >= 'a' && ch <= 'z' ) ch - 'a' + 10
			    else
				    throw new IllegalArgumentException(
					    "Illegal base-36 character '" + ch + "' in \"" + text + "\"." )

			  value = BigInt36 * value + digit
      }
		}
		
		if ( negative ) -value else value
	}
	
	def toString( value:BigInt ) = {
		val sb = new StringBuilder( 8 )
    var v = value
		
		val negative =
		  if ( v < 0 ) {
			  v *= -1
			  true
		  } else {
			  false
		  }
		
		do {
      val ( div, rem:BigInt ) = v /% 36
      val digit = rem.toInt
      v = div

			val ch =
			  if ( digit < 10 ) '0' + digit
			  else              'A' + digit - 10
		
			sb += ch.asInstanceOf[Char]
		} while ( v != 0 )
		
		if ( negative )
			sb += '-'
		
		sb.reverse.toString
	}
}


/**
 * Base62 number support.
 */
object Base62 {
  
  def make( len:Int ) = {
    def digit = {
      ( ( math.random * 62 ).toInt match {
      case n if n < 10 => '0'.toInt + n
      case n if n < 36 => 'A'.toInt + n - 10
      case n           => 'a'.toInt + n - 36
      } ).toChar
    }
    
    val sb = new StringBuilder
    for ( i <- 1 to len )
      sb += digit
        
    sb.toString
  }

	def toLong( text:String ) = {
		var value = 0L
		var negative = false
		
		for ( i <- 0 until text.length ) {
			val ch = text.charAt( i )

      if ( ch == '-' && i == 0 ) {
        negative = true
      } else {
			  val digit =
			    if      ( ch >= '0' && ch <= '9' ) ch - '0'
			    else if ( ch >= 'A' && ch <= 'Z' ) ch - 'A' + 10
			    else if ( ch >= 'a' && ch <= 'z' ) ch - 'a' + 36
			    else
				    throw new IllegalArgumentException(
					    "Illegal base-62 character '" + ch + "' in \"" + text + "\"." )

			  value = 62 * value + digit
      }
		}
		
		if ( negative ) -value else value
	}
	
	def toString( value:Long ) = {
		val sb = new StringBuilder( 8 )
    var v = value
		
		val negative =
		  if ( v < 0 ) {
			  v *= -1
			  true
		  } else {
			  false
		  }
		
		do {
			val digit = ( v % 62 )

			val ch =
			  if      ( digit < 10 ) '0' + digit
			  else if ( digit < 36 ) 'A' + digit - 10
			  else                   'a' + digit - 36
		
			sb += ch.asInstanceOf[Char]
			v /= 62
		} while ( v != 0 )
		
		if ( negative )
			sb += '-'
		
		sb.reverse.toString
	}

  private val BigInt62 = BigInt( 62 )

	def toBigInt( text:String ) = {
		var value = BigInt( 0 )
		var negative = false
		
		for ( i <- 0 until text.length ) {
			val ch = text.charAt( i )

      if ( ch == '-' && i == 0 ) {
        negative = true
      } else {
			  val digit =
			    if      ( ch >= '0' && ch <= '9' ) ch - '0'
			    else if ( ch >= 'A' && ch <= 'Z' ) ch - 'A' + 10
			    else if ( ch >= 'a' && ch <= 'z' ) ch - 'a' + 36
			    else
				    throw new IllegalArgumentException(
					    "Illegal base-62 character '" + ch + "' in \"" + text + "\"." )

			  value = BigInt62 * value + digit
      }
		}
		
		if ( negative ) -value else value
	}
	
	def toString( value:BigInt ) = {
		val sb = new StringBuilder( 8 )
    var v = value
		
		val negative =
		  if ( v < 0 ) {
			  v *= -1
			  true
		  } else {
			  false
		  }
		
		do {
      val ( div, rem:BigInt ) = v /% 62
      val digit = rem.toInt
      v = div

			val ch =
			  if      ( digit < 10 ) '0' + digit
			  else if ( digit < 36 ) 'A' + digit - 10
			  else                   'a' + digit - 36
		
			sb += ch.asInstanceOf[Char]
		} while ( v != 0 )
		
		if ( negative )
			sb += '-'
		
		sb.reverse.toString
	}
}


/**
 * Base64 number support.
 */
object Base64 {

  /**
   * Uses URL-safe encoding characters (i.e., "-" and "_").
   */
  def toString( bytes:Array[Byte] ) =
    org.apache.commons.codec.binary.Base64.encodeBase64URLSafeString( bytes )

  def toBytes( str:String ) =
    org.apache.commons.codec.binary.Base64.decodeBase64( str )
}

