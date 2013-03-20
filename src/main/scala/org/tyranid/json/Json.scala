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

package org.tyranid.json

import scala.collection.JavaConversions._
import scala.xml.NodeSeq

import org.bson.types.ObjectId
import com.mongodb.BasicDBList

import org.codehaus.jackson.map.ObjectMapper
import org.codehaus.jackson.{ JsonNode, JsonFactory, JsonParser }
import org.codehaus.jackson.node.{ ArrayNode, JsonNodeFactory, MissingNode, ObjectNode }

import org.tyranid.Imp._
import org.tyranid.db.{ DbArray, DbLink, Record }
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity, MongoRecord }
import org.tyranid.session.Notification
import org.tyranid.web.WebResponse


// Sbt is abbrev for Submit
object Sbt {

  def apply( href:String = null, top:Boolean = false, modal:String = null, js:String = null, opts:Map[String,Any] = null ) = {
    val sb = new StringBuilder
    var first = true

    def comma =
      if ( first ) first = false
      else         sb += ','

    sb += '{'

    if ( href.notBlank ) {
      comma
      sb ++= "\"href\":\"" ++= href.encJson += '"'
    }

    if ( top ) {
      comma
      sb ++= "\"top\":1"
    }

    if ( modal.notBlank ) {
      comma
      sb ++= "\"modal\":\"" ++= modal += '"'
    }

    if ( js.notBlank ) {
      comma
      sb ++= "\"extraJS\":\"" ++= js.encJson += '"'
    }

    if ( opts != null ) {
      comma
      sb ++= "\"opts\":" ++= opts.toJsonStr( false )
    }

    sb += '}'

    scala.xml.Unparsed( sb.toString )
  }
}


sealed trait JsCmd 

case class Js( js:String ) extends JsCmd 

case class JqHtml( // this is the target selector to place the html at
                   target:String,
                   html:NodeSeq,

                   // this is the target selector for a modal dialog to open, if present
                   modal:String = null,

                   transition:String = null,
                   duration:Int = 250,
                   handler:String = null ) extends JsCmd

                   
// @name if the name is null, it is a local model, otherwise it is a global model
case class JsModel( map:Map[String,Any], name:String = null ) extends JsCmd

case class JsData( data:Seq[Record] ) extends JsCmd

case object JsNop extends JsCmd 

object Jobj {
  def apply = JsonNodeFactory.instance.objectNode
}

object Json {

  lazy val factory = {
    val factory = new JsonFactory()
    
    factory.configure( JsonParser.Feature.ALLOW_UNQUOTED_FIELD_NAMES, true )
  }

  def parse( json:String ) = { new ObjectMapper( factory ) }.readTree( json )

  val EmptyArray  = JsonNodeFactory.instance.arrayNode
}

class JsonNodeImp( node:JsonNode ) /*extends Dynamic*/ {

  //def applyDynamic( name:String )( args:Any* ) = node.path( name )

  def /( name:String ) = node.path( name )
  def /( idx:Int )     = node.path( idx )

  def apply( name:String ) = node.path( name )
  def apply( idx:Int )     = node.path( idx )

  def update( name:String, value:Boolean ) = node.asInstanceOf[ObjectNode].put( name, value )
  def update( name:String, value:Int     ) = node.asInstanceOf[ObjectNode].put( name, value )
  def update( name:String, value:Long    ) = node.asInstanceOf[ObjectNode].put( name, value )
  def update( name:String, value:Double  ) = node.asInstanceOf[ObjectNode].put( name, value )
  def update( name:String, value:String  ) = node.asInstanceOf[ObjectNode].put( name, value )

  def a( key:String ):ArrayNode = apply( key ).a
  def b( key:String ) = apply( key ).getValueAsBoolean
  def d( key:String ) = apply( key ).getValueAsDouble
  def i( key:String ) = apply( key ).getValueAsInt
  def l( key:String ) = apply( key ).getValueAsLong
  def s( key:String ) = apply( key ).getValueAsText.denull

  def a  =
    node match {
    case array:ArrayNode     => array
    case missing:MissingNode => Json.EmptyArray
    }
  def b = node.getValueAsBoolean
  def d = node.getValueAsDouble
  def i = node.getValueAsInt
  def l = node.getValueAsLong
  def s = node.getValueAsText.denull



  def Missing = MissingNode.getInstance
  //import Json.Missing
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

case class JsonString( root:Any, pretty:Boolean = false, client:Boolean = false ) {

  private val sb = new StringBuilder

  override def toString = {
    write( root )
    pretty ? new ObjectMapper().defaultPrettyPrintingWriter().writeValueAsString( Json.parse( sb.toString ) ) | sb.toString
  }

  private def write( obj:Any ):Unit =
    obj match {
    case s:String            => sb += '"' ++= s.encJson += '"'
    case i:java.lang.Integer => sb ++= i.toString

    // This will probably have to be parsed better to replace all inner quotes
    case xml:NodeSeq         => 
      sb += '"' ++= xml.toString.replaceAll( "\\\\", "{--TY_DS--}" ).replaceAll( "\"", "\\\\\"" ).replaceAll( "\t", "" ).replaceAll( "\n", "{--TY_NL--}" ).replaceAll( "\r", "{--TY_LF--}" ) += '"'

    case a:Array[_]          =>
      sb += '['
      for ( i <- 0 until a.length ) {
        if ( i > 0 ) sb += ','
        write( a( i ) )
      }
      sb += ']'
    case l:BasicDBList       =>
      sb += '['
      for ( i <- 0 until l.size ) {
        if ( i > 0 ) sb += ','
        write( l( i ) )
      }
      sb += ']'

    case l:Seq[_]            =>
      var first = true
      sb += '['
      for ( v <- l ) {
        if ( first )
          first = false
        else
          sb += ','
        write( v )
      }
      sb += ']'
    case p:Pair[_,_] =>
      sb += '{'
      write( p._1 )
      sb += ':'
      write( p._2 )
      sb += '}'

    case rec:Record =>
      if ( client ) rec.computeClient
      
      sb += '{'
      var first = true
      for ( va <- rec.view.vas;
            if !client || va.att.client;
            if rec.has( va ) ) {
        if ( first )
          first = false
        else
          sb += ','

        va.domain match {
        case d:MongoEntity =>
          write( va.name )
          sb += ':'
          write( rec.rec( va ) )
        case d:DbArray if d.of.is[MongoEntity] =>
          val arr = rec( va ).as[BasicDBList]
          val mEnt = va.domain.as[DbArray].of.as[MongoEntity]

          for ( i <- 0 until arr.size() )
            mEnt.recify( arr( i ), rec.as[MongoRecord], rec => arr( i ) = rec )

          write( va.name )
          sb += ':'
          write( arr )
        case DbMongoId if va.name == "_id" && va.att.isId && client =>
          write( "id" )
          sb += ':'
          write( va.att.entity.idToTid( rec( va ) ) )
        case d:DbLink if client =>
          val id = rec( va )
          
          if ( va.name == "_id" ) {
            write( "id" )
            sb += ':'
            write(
              if ( id != null )
                va.att.entity.idToTid( id )
              else
                null )
          } else {
            write( va.name )
            sb += ':'
            write(
              if ( id != null )
                d.toEntity.idToTid( id )
              else
                null )
          }
              
        case _ =>
          write( va.name )
          sb += ':'
          write( rec( va ) )
        }
      }

      sb += '}'

    case o:collection.Map[_,_]          =>
      sb += '{'
      var first = true
      for ( e <- o ) {
        if ( e._2 != null ) {
          if ( first )
            first = false
          else
            sb += ','
          write( e._1 )
          sb += ':'
          write( e._2 )
        }
      }
      sb += '}'
        
    case b:java.lang.Boolean  => sb ++= b.toString
    case d:java.lang.Double   => sb ++= d.toString
    case l:java.lang.Long     => sb ++= l.toString
    case f:java.lang.Float    => sb ++= f.toString
    case d:java.util.Date     => sb ++= d.getTime._s
    case c:java.util.Calendar => sb ++= c.getTime.getTime._s
    
    case oid:ObjectId         => sb += '"' ++= oid.toString += '"'
      
    case ws:WebResponse       => sb ++= ws.toJsonStr
    case note:Notification    => write( note.msg )
    case null => println( "Warning, turning NULL into JSON (empty string)" ); sb ++= "\"\""
    case u => 
      println( "Don't know how to turn " + u + " (" + u.getClass() + ") into JSON" ); sb ++= "\"\"" 
    }
}

