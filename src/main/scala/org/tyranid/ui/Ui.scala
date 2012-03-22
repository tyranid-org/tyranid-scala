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

package org.tyranid.ui

import scala.collection.mutable
import scala.xml.{ Node, NodeSeq, Unparsed }

import org.tyranid.Imp._
import org.tyranid.db.{ Record, Path, Scope, View, ViewAttribute }
import org.tyranid.logic.Invalid


object Link {

  def fromUrl( url:String ) =
    if ( url.isBlank ) {
      NodeSeq.Empty
    } else {
      val u = url.asUrl
      <a href={ u }>{ u }</a>
    }
}

object Button {

  def link( name:String, href:String, color:String ) =
    <a class={ color + "Btn" } href={ href }><span>{ name }</span></a>

  def bar( buttons:Node* ) =
    <table class="btnbar">
     <tr>
      { buttons.map( btn => <td>{ btn }</td> ) }
     </tr>
    </table>
}

object Input {

  def apply( name:String, value:String, opts:(String,String)* ) = {
    val sb = new StringBuilder

    sb ++= "<input value=\"" ++= ( if ( value == null ) "" else value ) ++= "\""

    var typ = "text"

    for ( opt <- opts )
      opt match {
      case ( "class", v ) => sb ++= " class=\"" ++= v += '"'
      case ( "style", v ) => sb ++= " style=\"" ++= v += '"'
      case ( "type",  v ) => typ = v
      case ( "readonly", v ) => sb ++= " readonly=\"readonly\""
      case ( "placeholder", v ) => sb ++= " placeholder=\"" ++= v += '"'
      case ( n,       v ) => throw new RuntimeException( "Unknown field option " + n + " = " + v )
      }

    sb ++= " name=\"" + name + "\" id=\"" + name + "\"" + " type=\"" ++= typ ++= "\"/>"

    Unparsed( sb.toString )
  }
}

object TextArea {

  def apply( name:String, value:String, opts:(String,String)* ) = {
    val sb = new StringBuilder
    sb ++= "<textarea "
      
      // Don't do anything with typ yet
    var typ = "text"
    var rows = ""
    var cols = ""
      
    for ( opt <- opts )
      opt match {
      case ( "class", v ) => sb ++= " class=\"" ++= v += '"'
      case ( "style", v ) => sb ++= " style=\"" ++= v += '"'
      case ( "type",  v ) => typ = v
      case ( "rows",  v ) => rows = v
      case ( "cols",  v ) => cols = v
      case ( n,       v ) => throw new RuntimeException( "Unknown field option " + n + " = " + v )
      }

    sb ++= " name=\"" ++= name ++= "\" id=\"" ++= name ++= "\"" ++= ( rows |* " rows=\"" + rows + "\"" ) ++= ( cols |* " cols=\"" + cols + "\"" ) ++= ">" ++= ( value |* value ) ++= "</textarea>" 

    Unparsed( sb.toString )
  }
}

object Checkbox {

  def apply( name:String, value:Boolean, opts:(String,String)* ) = {
    val sb = new StringBuilder

    sb ++= "<input "

    var typ = "checkbox"

    for ( opt <- opts )
      opt match {
      case ( "class", v ) => sb ++= " class=\"" ++= v += '"'
      case ( "style", v ) => sb ++= " style=\"" ++= v += '"'
      case ( "type",  v ) => typ = v
      case ( n,       v ) => throw new RuntimeException( "Unknown field option " + n + " = " + v )
      }

    if ( value )
      sb ++= " checked"

    sb ++= " value=\"1\"" ++= " name=\"" ++= name ++= "\" id=\"" ++= name ++= "\" type=\"" ++= typ ++= "\"/>"

    Unparsed( sb.toString )
  }
}

object ToggleLink {

  def apply( name:String, value:Boolean, opts:(String,String)* ) = {
    var label = value ? "Yes" | "No"
    val sb = new StringBuilder

    sb ++= "<input type=\"hidden\" name=\"" ++= name ++= "\" id=\"" ++= name ++= "\" value=\"" ++= ( if ( value ) "1" else "0" ) ++= "\"/>" ++= label 

    val linkLabel = new StringBuilder
    linkLabel ++= " (<a"
      
    var cssClass = "toggleLink"
    var toggleLabel = value ? "No" | "Yes"

    if ( opts != null )
      for ( opt <- opts )
        opt match {
        case ( "class", v ) => cssClass = v
        case ( "style", v ) => linkLabel ++= " style=\"" ++= v += '"'
        case ( "href", v ) => linkLabel ++= " href=\"" ++= v += '"'
        case ( "labels", v ) => toggleLabel = {
          val parts = v.split( "\\|" )
          
          if ( value )
            if( parts.length == 2 )
              parts( 1 )
            else
              ""
          else
            parts( 0 )
        }
        case ( n,       v ) => throw new RuntimeException( "Unknown field option " + n + " = " + v )
        }

    Unparsed( ( sb ++= ( ( if ( toggleLabel notBlank ) ( linkLabel ++= " class=\"" ++= cssClass += '"' ++= ">" ++= toggleLabel ++= "</a>)" ).toString else "" ).toString ) ).toString ) 
  }
}

object Select {

  def apply( name:String, value:String, values:Seq[ (String,String) ], opts:(String,String)* ) = {
    var selectOptions = values
    val sb = new StringBuilder

    sb ++= "<select name=\"" ++= name ++= "\" id=\"" ++= name += '"'

    for ( opt <- opts )
      opt match {
      case ( "class", v ) => sb ++= " class=\"" ++= v += '"'
      case ( "style", v ) => sb ++= " style=\"" ++= v += '"'
      case ( "sort", v ) => ( selectOptions = selectOptions.toSeq.sortBy( _._2 ) )
      case ( n,       v ) => throw new RuntimeException( "Unknown Select.field option " + n + " = " + v )
      }

    sb += '>'

    for ( v <- selectOptions ) {
      sb ++= "<option value=\"" ++= v._1 += '"'
      if ( v._1 == value )
        sb ++= " selected"
      sb += '>' ++= v._2 ++= "</option>"
    }


    sb ++= "</select>"

    Unparsed( sb.toString )
  }
}


/*
 * * *   UI
 */


sealed trait UiStyle

object UiStyle {
  case object Default extends UiStyle
  case object Toggle  extends UiStyle
}

trait UiObj {

  def bind( view:View ):UiObj

  def draw( scope:Scope ):NodeSeq = NodeSeq.Empty

  def extract( scope:Scope ):Unit

  def fields:Iterable[PathField]
}


case class Row( fields:PathField* ) extends UiObj {

  def bind( view:View ) = {
    for ( field <- fields )
      field.bind( view )
    this // TODO:  return an immutable version
  }

  def extract( scope:Scope ) =
    for ( field <- fields )
      field.extract( scope )
}

case class Grid( rows:Row* ) extends UiObj {
  val boxSpan = rows.map( _.fields.length ).max

  var view:View = null

  def bind( view:View ) = {
    this.view = view

    for ( row <- rows )
      row.bind( view )

    this // TODO:  return an immutable version
  }

  def extract( scope:Scope ) =
    for ( row <- rows )
      row.extract( scope )

  def fields = rows flatMap { _.fields }

  override def draw( pScope:Scope ) = {
    val scope = pScope.copy( initialDraw = true )
    
    for ( row <- rows ) yield
      <tr>{
        for ( f <- row.fields ) yield
          <td colspan={ f.span.toString } class="cell">{ f.draw( scope ) }</td>
      }</tr>
  }
}

