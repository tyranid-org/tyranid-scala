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

import net.liftweb.http.{ S, SHtml }
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.JE.JsRaw

import org.tyranid.Imp._
import org.tyranid.db.{ Record, Path, Scope, View, ViewAttribute }
import org.tyranid.logic.Invalid

object Opts {
  val Empty = Opts()
}

case class Opts( opts:(String,String)* )


object Button {

  def link( name:String, href:String, color:String ) =
    <a class={ color + "Btn" } href={ href }><span>{ name }</span></a>

  def submit( name:String, act:() => Unit, color:String ) =
    SHtml.submit( name, act, "class" -> ( color + "Btn" ) )

  def ajaxButton( name:String, act:() => JsCmd, color:String ) =
    SHtml.ajaxButton( name, act, "class" -> ( color + "Btn" ) )

  def button( name:String, act:( String ) => JsCmd, color:String, inline:Boolean = false ) =
    <button class={ color + "Btn" } style={ inline |* "display:inline;" } onclick={ SHtml.ajaxCall( JsRaw( "''" ), act )._2.toJsCmd }>{ Unparsed( name ) }</button>

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

    sb ++= "<input value=\"" ++= value ++= "\""

    var typ = "text"

    for ( opt <- opts )
      opt match {
      case ( "class", v ) => sb ++= " class=\"" ++= v += '"'
      case ( "style", v ) => sb ++= " style=\"" ++= v += '"'
      case ( "type",  v ) => typ = v
      case ( n,       v ) => throw new RuntimeException( "Unknown field option " + n + " = " + v )
      }

    sb ++= " name=\"" + name + "\" id=\"" + name + "\" type=\"" ++= typ ++= "\"/>"

    Unparsed( sb.toString )
  }
}

object TextArea {

  def apply( name:String, value:String, opts:(String,String)* ) = {
    val sb = new StringBuilder
    sb ++= "<textarea "
      
      // Don't do anything with typ yet
    var typ = "text"

    for ( opt <- opts )
      opt match {
      case ( "class", v ) => sb ++= " class=\"" ++= v += '"'
      case ( "style", v ) => sb ++= " style=\"" ++= v += '"'
      case ( "type",  v ) => typ = v
      // look for rows, cols
      case ( n,       v ) => throw new RuntimeException( "Unknown field option " + n + " = " + v )
      }

    sb ++= " name=\"" + name + "\" id=\"" + name + "\">" ++= value ++= "</textarea>" 

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

    sb ++= " value=\"1\"" ++= " name=\"" + name + "\" id=\"" + name + "\" type=\"" ++= typ ++= "\"/>"

    Unparsed( sb.toString )
  }
}

object Select {

  def apply( name:String, value:String, values:Seq[ (String,String) ], opts:(String,String)* ) = {

    val sb = new StringBuilder

    sb ++= "<select name=\"" ++= name ++= "\" id=\"" ++= name += '"'

    for ( opt <- opts )
      opt match {
      case ( "class", v ) => sb ++= " class=\"" ++= v += '"'
      case ( "style", v ) => sb ++= " style=\"" ++= v += '"'
      case ( n,       v ) => throw new RuntimeException( "Unknown Select.field option " + n + " = " + v )
      }

    sb += '>'

    for ( v <- values ) {
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


trait UiObj {

  def bind( view:View ):UiObj

  def draw    ( scope:Scope ):NodeSeq = NodeSeq.Empty
  def drawLift( scope:Scope ):NodeSeq = NodeSeq.Empty

  def extract( scope:Scope ):Unit

  def fields:Iterable[Field]
}


object Field {

  implicit def string2Field( name:String ) = Field( name )
  implicit def symbol2Field( name:Symbol ) = Field( name.name )

  def text( s:Scope, f:Field, opts:(String,String)* ):NodeSeq =
    input( s, f, s.rec.s( f.va.name ), opts:_* )

  def input( s:Scope, f:Field, value:String, opts:(String,String)* ):NodeSeq = {
    val ret = optsMapper( s, f, value, opts:_* )
    Input( ret._1, ret._2, ret._3:_*  )
  }
  
  def select( s:Scope, f:Field, value:String, values:Seq[ (String,String) ], opts:(String,String)* ) = {
    val ret = optsMapper( s, f, value, opts:_* )
    Select( ret._1, ret._2, values, ret._3:_* )
  }

  def textArea( s:Scope, f:Field, value:String, opts:(String,String)* ):NodeSeq = {
    val ret = optsMapper( s, f, value, opts:_* )
    TextArea( ret._1, ret._2, ret._3:_*  )
  }
  
  private def optsMapper( s:Scope, f:Field, value:String, opts:(String,String)* ):( String, String, Seq[(String,String)] ) = { 
    var id = f.id

    val opts2 = opts.flatMap {
      _ match {
      case ( "id" | "name", v )   =>
        if ( id != null && v != id )
          throw new RuntimeException( "Form element being named " + v + " and " + id )

        id = v
        None

      case p =>
        Some( p )
      }
    }

    if ( id == null ) {
      id = /* TODO: form id + '_' + */ f.va.name
      f.id = id
    } else if ( id != f.id ) {
      f.id = id
    }

    return ( id, value, opts2 )
  }
  
}

case class Field( name:String, opts:Opts = Opts.Empty, span:Int = 1, edit:Boolean = true, inputOnly:Boolean = false, onSet:Option[ ( Field ) => JsCmd ] = None, focus:Boolean = false ) extends UiObj {

  var id:String = null

  var path:Path = null
  def va = path.leaf

  def bind( view:View ) = {
    path = view.path( name )
    this // TODO:  return an immutable version
  }

  def extract( pScope:Scope ) = {
    val scope = pScope.at( path )
    va.att.domain.extract( scope, this )
  }

  def fields = Seq( this )

  private def invalidLines( invalids:Seq[Invalid] ) =
    for ( invalid <- invalids )
      yield <span>{ invalid.message }</span>

  override def draw( pScope:Scope ) =
    if ( inputOnly ) {
      va.att.domain.ui( pScope.at( path ), this, ( opts.opts ++ Seq( "id" -> va.name ) ):_* )
    } else {
      val scope = pScope.at( path )
      val invalids = va.invalids( scope )
      val invalid = !invalids.isEmpty
      val rec = scope.rec
      rec.invalids( va.index ) = !invalids.isEmpty

      va.att.domain.show( scope ) |*
      <div id={ va.name + "_c" } class={ "fieldc" + ( invalid |* " invalid" ) }>
       <div class="labelc">{ va.label( rec, opts.opts:_* ) }{ va.att.required |* <span class="required">*</span> }</div>
       <div class={ "inputc" + va.att.domain.inputcClasses }>{ va.att.domain.ui( scope, this, ( opts.opts ++ Seq( "id" -> va.name ) ):_* ) }</div>
       <div id={ va.name + "_e" } class="notec">{ !invalids.isEmpty |* invalidLines( invalids ) }</div>
      </div>
    }

  override def drawLift( pScope:Scope ) =
    if ( inputOnly ) {
      va.att.domain.uiLift( pScope.at( path ), this, ( opts.opts ++ Seq( "id" -> va.name ) ):_* )
    } else {
      val scope = pScope.at( path )
      val invalids = va.invalids( scope )
      val rec = scope.rec
      rec.invalids( va.index ) = !invalids.isEmpty
    
      va.att.domain.show( scope ) |*
      <div id={ va.name + "_c" } class={ "fieldc" + ( !invalids.isEmpty |* " invalid" ) }>
       <div class="labelc">{ va.label( rec, opts.opts:_* ) }{ va.att.required |* <span class="required">*</span> }</div>
       <div class={ "inputc" + va.att.domain.inputcClasses }>{ va.att.domain.uiLift( scope, this, ( opts.opts ++ Seq( "id" -> va.name ) ):_* ) }</div>
       <div id={ va.name + "_e" } class="notec">{ !invalids.isEmpty |* invalidLines( invalids ) }</div>
      </div>
    }

  def updateDisplayCmd( scope:Scope ):JsCmd = {
    
    val onSetCmd = onSet.flatten( _( this ), Noop )
    if ( onSetCmd != Noop )
      return onSetCmd
    
    val rec = scope.rec
    val invalids = va.invalids( scope.copy( initialDraw = false, path = Some( va ) ) )
      
    if ( invalids.isEmpty ) {
      if ( rec.invalids( va.index ) ) {
        rec.invalids -= va.index


        SetHtml( va.name + "_e", NodeSeq.Empty ) &
        JsRaw( "$('#" + va.name + "_c').removeClass('invalid');" )
      } else {
        Noop
      }
    } else {
      rec.invalids += va.index

      SetHtml( va.name + "_e", invalidLines( invalids ) ) &
      JsRaw( "$('#" + va.name + "_c').addClass('invalid');" )
    }
  }
}

case class Row( fields:Field* ) extends UiObj {

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

  override def drawLift( pScope:Scope ) = {
    val scope = pScope.copy( initialDraw = true )
    
    for ( row <- rows ) yield
      <tr>{
        for ( f <- row.fields ) yield
          <td colspan={ f.span.toString } class="cell">{ f.drawLift( scope ) }</td>
      }</tr>
  }
}

