/**
 * Copyright (c) 2008-2014 Tyranid <http://tyranid.org>
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

import scala.language.postfixOps

import java.util.Date

import scala.collection.mutable
import scala.xml.{ Node, NodeSeq, Unparsed, Text }

import org.tyranid.Imp._
import org.tyranid.db.{ Record, Path, Scope, View, ViewAttribute }
import org.tyranid.logic.Invalid
import org.tyranid.web.Weblet

object Ui {
  // Prints with break at end
  def br( s:String, strong:Boolean = false ) = 
    if ( s.nonBlank )
      if ( strong )
        <strong>{ s }{ Unparsed("<br>") }</strong>
      else 
        { Text( s ) ++ Unparsed("<br>") }
    else NodeSeq.Empty
    
  def gridGuide( cols:Int = 12 ) = { B.DEV |* <div class="row"> { for ( i <- 0 until cols) yield {<div class="row-guide span1"></div> } } </div> }
}

object Help {

  def apply( help:NodeSeq ) = <span class="helpText">{ help }</span><span class="helpIcon"></span>
}

object Tags {

  def tagUi( id:String, label:String, placeholder:String = "" ) =
    <li class="tag">
     <span>{ label }<a class="closeTag"><i class="fa fa-times"></i></a><input type="hidden" style="display:none;" value={ id } id="to" name="to[]" placeholder={ placeholder }/></span>
    </li>
}

object Dialog {

  def closeBtn( unicodeSupport:Boolean = true ) =
    <div class="closeBtn">
     <button type="button" class="close" data-dismiss="modal">{ Unparsed( unicodeSupport ? """&#x2715;""" | """&times;""" ) }</button>
    </div>
}

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

  def btn( id:String, label:String, color:String = "", disabled:Boolean = false, tip:String = null ) =
    Unparsed( "<button id=\"" + id + "\" class=\"" + color + ( tip.notBlank |* " tip" ) + " btn\" " + ( disabled |* " disabled=\"disabled\"" ) + ( tip.notBlank |* "tip=\"" + tip + "\"" ) + ">" + label + "</button>" )

  def img( id:String, iconName:String, color:String = "", style:String = null, disabled:Boolean = false, tip:String = null ) =
    Unparsed( "<button id=\"" + id + "\" class=\"" + color + " btn\" " + ( disabled |* " disabled=\"disabled\"" ) + ( style.notBlank |* " style=\"" + style + "\"" ) + "><span class=\"" + ( tip.notBlank |* "tip " ) + "ui-icon ui-icon-" + iconName + "\"" + ( tip.notBlank |* " tip=\"" + tip + "\"" ) + "/></button>" )

  def bigimg( id:String, iconName:String, color:String = "", style:String = null, disabled:Boolean = false, tip:String = null ) =
    Unparsed( "<button id=\"" + id + "\" class=\"" + color + " btn\" " + ( disabled |* " disabled=\"disabled\"" ) + ( style.notBlank |* " style=\"" + style + "\"" ) + "><span class=\"" + ( tip.notBlank |* "tip " ) + "bigIcon " + iconName + "Icon\"" + ( tip.notBlank |* " tip=\"" + tip + "\"" ) + "/></button>" )

  def link( name:String, href:String, color:String = null, redirectEndpoint:String = null, dialogTitle:String = null, opts:Seq[(String,String)] = null ) =
    if ( redirectEndpoint == null ) {
      <a class={ if ( color.notBlank ) color + " btn" else "btn" } href={ href } id={ name + "btn" }><span>{ name }</span></a>
    } else {
      val optsStr:StringBuilder= new StringBuilder

      if ( opts != null ) {
        opts.foreach {
          _ match {
          case ( n, v ) =>
            optsStr.append( "td.option( \"" + n + "\", \"" + v + "\" );" )
          case _ =>
          }
        }    
      }
      
      <a class={ if ( color.notBlank ) color + " btn" else "btn" } href={ href } onClick={ "var td = new TDialog( '" + href + "','" + redirectEndpoint + "', '" + dialogTitle + "' );" + optsStr.toString + " td.open(); return false;" }><span>{ name }</span></a>
    } 
    
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
      case ( "readonly", v ) => if ( v.notBlank && v != "0" ) sb ++= " readonly=\"readonly\""
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

    sb ++= " name=\"" ++= name ++= "\" id=\"" ++= name ++= "\"" ++= ( rows |* " rows=\"" + rows + "\"" ) ++= ( cols |* " cols=\"" + cols + "\"" ) ++= ">" ++= value |* value ++= "</textarea>" 

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

case class TabBar( weblet:Weblet, tabs:Tab* ) {

  lazy val defaultTab = tabs.find( _.default ).getOrElse( throw new RuntimeException( "Missing default tab." ) )

  def has( rpath:String ) = tabs.exists( _.rpath == rpath )

  def draw( qs:String = "", except:Seq[String] = Nil ) = {
    val rpath = weblet.rpath

    val activeTabs = tabs filter { tab => !except.exists( _ == tab.rpath ) }

    activeTabs.find( _.rpath == rpath ) match {
    case Some( tab ) =>
      T.session.setPathChoiceAt( weblet.wpath, tab.rpath )
    
    case None =>
      val tabPath = T.session.pathChoiceAt( weblet.wpath, defaultTab.rpath )
    }

    <div class="tabbar">
     <ul class="nav nav-tabs">
      { activeTabs map ( _.draw( this, qs ) ) }
     </ul>
    </div>;
  }

  def choice = {
    val p = T.session.pathChoiceAt( weblet.wpath, defaultTab.rpath )
    tabs.find( _.rpath == p ).pluck( _.rpath, defaultTab.rpath )
  }
}

case class Tab( rpath:String, label:NodeSeq, cls:String = null, default:Boolean = false ) {

  def draw( bar:TabBar, qs:String ) = {
    val fpath = bar.weblet.wpath + rpath
    
    val choice = bar.choice

    <li>{
      var cls = this.cls 

      if ( rpath == choice ) {
        if ( cls == null )
          cls = "active";
        else
          cls += " active"
      }

      <a class={ cls }
         data-sbt={ Some( Text( Form.attrJson( Map( "href" -> ( fpath + qs ) ) ) ) ) }>{ label }</a>
    }</li>
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

trait UiObj extends Serializable {
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

