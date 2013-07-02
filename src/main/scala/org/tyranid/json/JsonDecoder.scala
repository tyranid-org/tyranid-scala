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

package org.tyranid.json

import scala.collection.mutable

import org.tyranid.Imp._


/*
 * Adapted from https://github.com/ritschwumm/scjson/blob/master/src/main/scala/scjson/codec/JSDecoderFast.scala
 */

object JsonDecoder {
	def apply( s:String ):Any = new JsonDecoder( s ).decode
}

private final class JsonDecoder( text:String ) {
	val NO_CHAR	= -1
	var	offset	= 0

	private def decode:Any	= {
		val value	= decodeNext
		ws
		if (!eof)	expected("end of input")
		value
	}

	private def decodeNext:Any = {
		ws
		if ( eof)		         expected("any char")
		if ( is( "null" ) )	 return null
		if ( is( "true" ) )	 return true
		if ( is( "false" ) ) return false
		if ( is( '[' ) ) {
			val	out	= mutable.ArrayBuilder.make[Any] 
			ws
			if ( is( ']' ) ) return out.result
			while ( true ) {
				val value	= decodeNext
				out	+= value
				ws
				if ( is( ']' ) ) return out.result

				if ( !is( ',' ) ) expectedClass(",]")
			}
		}
		if ( is( '{' ) ) {
			val out	= new mutable.LinkedHashMap[String,Any]()
      ws
		  if ( is( '}' ) ) return out.result
			while (true) {
        ws
        val c = next

        val key =
          if ( c == '\'' || c == '\"' ) {
            decodeNext match {
            case s:String	=> s
            case _			  => expected( "a key" )
            }
          } else {
            val before = offset

            if ( isIdentifierStart )
              while ( isIdentifierPart )
                {}

            val s = from( before )
            if ( s.isBlank ) expected( "a key" )
            s
          }

				ws
				if ( !is( ':' ) )	expectedClass(":")
				out( key ) = decodeNext
				ws
				if ( is( '}' ) )	return out.result
				if ( !is( ',' ) )	expectedClass( ",}" )
			}
		}
		if ( is('"') || is( '\'' ) ) {
			val out	= new mutable.StringBuilder
      val sep = prev.toChar
			while ( true ) {
				if ( is( '\\' ) ) {
					if ( eof ) expected("escape continuation")

          val c = next
          offset += 1

          c match {
          case '"' | '\\' | '/' => out += c.toChar
          case 't'              => out += '\t'
          case 'r'              => out += '\r'
          case 'n'              => out += '\n'
          case 'f'              => out += '\f'
          case 'b'              => out += '\b'
          case 'u'              =>
						hex4 match {
            case NO_CHAR => expected( "4 hex digits" )
            case h       => out	+= h.toChar
            }
          case _                => offset -= 1; expectedClass("\"\\/trnfbu")
					}
				} else if ( is( sep ) ) {
					return out.result

				} else if ( rng( '\u0000', '\u001f' ) ) {
					offset	-= 1
					expected( "no control character" )

				} else {
					if ( eof ) expected( "more chars" )
					out	+= next.toChar
					consume
				}
			}
		}

		val before = offset
		is('-')
		digits

		if ( !is('.') ) {
      try {
        Integer.parseInt( from( before ) )
      } catch {
      case e:NumberFormatException	=>
        try {
          java.lang.Long.parseLong( from( before ) )
        } catch {
        case e:NumberFormatException	=>
          offset= before
          expected("valid number")
        }
      }
    } else {
      digits
      is('e') || is('E')
      is('+') || is('-')
      digits

      try {
        java.lang.Double.parseDouble( from( before ) )
      } catch {
      case e:NumberFormatException	=>
        offset	= before
        expected("valid number")
      }
    }
	}
	
	private def expected     (what:String)	    = throw JsonDecoderException( text, offset, what )
	private def expectedClass(charClass:String)	= throw JsonDecoderException( text, offset, "one of " + charClass )


	private def hex4:Int	= {
		val before = offset
		val	h1	= hexDigit;	if (h1 != NO_CHAR)	{ offset += 1 } else { offset = before; return NO_CHAR }
		val	h2	= hexDigit;	if (h2 != NO_CHAR)	{ offset += 1 } else { offset = before; return NO_CHAR }
		val	h3	= hexDigit;	if (h3 != NO_CHAR)	{ offset += 1 } else { offset = before; return NO_CHAR }
		val	h4	= hexDigit;	if (h4 != NO_CHAR)	{ offset += 1 } else { offset = before; return NO_CHAR }
		( h1 << 12 ) | ( h2 << 8 ) | ( h3 << 4 ) | h4
	}
			
	private def hexDigit:Int	= {
		val	c	= next
			   if ( c >= '0' && c <= '9')	c - '0'
		else if ( c >= 'a' && c <= 'f')	c - 'a' + 10
		else							              NO_CHAR
	}

	private def digits:Boolean= {
		val before = offset
		var keepOn = true
		while ( !eof && keepOn )
			next match {
      case c if c >= '0' && c <= '9' => consume
      case _                         => keepOn = false
      }

		offset != before
	}
	
	private def ws {
		while ( !eof )
      next match {
      case ' ' | '\t' | '\r' | '\n' => consume
			case _                        => return
    }
	}
	
	private def rng( start:Char, end:Char ) =
		next match {
    case NO_CHAR                   => false
    case c if c < start || c > end => false
		case _                         => consume; true
	  }
	
	private def is( c:Char ) = {
			   if ( eof )       false
		else if ( next != c )	false
		else                  { consume; true }
	}
	
	private def is( s:String ) = {
		val end	= offset + s.length

	  if ( end <= text.length && text.substring( offset, end ) == s ) {
      offset = end
      true
		} else {
      false
    }
	}

  private def isIdentifierStart =
    if ( Character.isJavaIdentifierStart( next ) ) {
      offset += 1
      true
    } else {
      false
    }

  private def isIdentifierPart =
    if ( Character.isJavaIdentifierPart( next ) ) {
      offset += 1
      true
    } else {
      false
    }
	
	private def eof = offset == text.length
	
	private def next:Int =
	 	if ( eof ) NO_CHAR
		else  		 text.charAt( offset )
	
	private def prev:Int =
		if ( offset == 0 ) NO_CHAR
		else				       text.charAt( offset-1 )
			
	private def from( before:Int ) = text.substring( before, offset )
	
	private def consume {
		if (eof)	sys.error( "already finished" )
		offset	+= 1
	}
}

case class JsonDecoderException( input:String, offset:Int, expectation:String ) extends Exception {

	override def getMessage = "at offset:" + offset + " expected:" + expectation + " got:" + lookingAt

	def lookingAt:String = {
		val width	= 80
		val end		= (offset+width) min input.length
		input substring ( offset, end ) replaceAll ( "[\r\n]", "§" )
	}
}

