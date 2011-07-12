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

package org.tyranid.json

import scala.collection.JavaConversions._

import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.node.{ ArrayNode, JsonNodeFactory, MissingNode, ObjectNode }

import org.tyranid.Imp._

object Jobj {
  def apply = JsonNodeFactory.instance.objectNode
}

object Json {

  val factory = new org.codehaus.jackson.JsonFactory().configure( org.codehaus.jackson.JsonParser.Feature.ALLOW_UNQUOTED_FIELD_NAMES, true )

  def parse( json:String ) = new org.codehaus.jackson.map.ObjectMapper( factory ).readTree( json )

  val Missing = MissingNode.getInstance
}

class JsonNodeImp( node:JsonNode ) extends Dynamic {

  def applyDynamic( name:String )( args:Any* ) = node.path( name )

  def /( name:String ) = node.path( name )
  def /( idx:Int )     = node.path( idx )

  def apply( name:String ) = node.path( name )
  def apply( idx:Int )     = node.path( idx )

  def update( name:String, value:Boolean ) = node.asInstanceOf[ObjectNode].put( name, value )
  def update( name:String, value:Int     ) = node.asInstanceOf[ObjectNode].put( name, value )
  def update( name:String, value:Long    ) = node.asInstanceOf[ObjectNode].put( name, value )
  def update( name:String, value:Double  ) = node.asInstanceOf[ObjectNode].put( name, value )
  def update( name:String, value:String  ) = node.asInstanceOf[ObjectNode].put( name, value )

  def b = node.getValueAsBoolean
  def d = node.getValueAsDouble
  def i = node.getValueAsInt
  def l = node.getValueAsLong
  def s = node.getValueAsText.denull

  import Json.Missing
  def opt = if ( node ne Missing ) Some( node ) else None
  def ob  = if ( node ne Missing ) Some( b )    else None
  def od  = if ( node ne Missing ) Some( d )    else None
  def oi  = if ( node ne Missing ) Some( i )    else None
  def ol  = if ( node ne Missing ) Some( l )    else None
  def os  = Option( node.getValueAsText )

  def children =
    node match {
    case node:ArrayNode => node.getElements.toIterable
    case _              => throw new RuntimeException( "children not supported on a " + node.getClass.getName + " node" )
    }
}


