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

object Glyph {
  val Checkmark = Unparsed( "&#10004;" )
}

object Focus {

  def apply( id:String ) =
    <script>{ Unparsed( """$( $('""" + id + """').focus() );""" ) }</script>
}


/*
     options:


     1.  not so monolothic ... use HTML to layout, not some Grid class ?


     2.  when we submit to a URL, we need a programmatic way to scrape the Form



 */

object Form {

  // probably swap this convention ... text() takes a NodeSeq, stext() takes a String ?
  def htext( label:String, value:NodeSeq, href:String = null, controlClass:String = "control" ) =
    <div class={ controlClass }>
     <div class="left">{ label }</div>
     <div class="right">
      { href != null |* <a href={ href } class="greyBtn">Edit</a> }
      <div class="text">{ value }</div>
     </div>
    </div>

  def text( label:String, value:String, href:String = null, controlClass:String = "control" ) =
    <div class={ controlClass }>
     <div class="left">{ label }</div>
     <div class="right">
      { href != null |* <a href={ href } class="greyBtn">Edit</a> }
      <div class="text">{ value }</div>
     </div>
    </div>

  def thumbnail( label:String, src:String, href:String = null, style:String = "width:60px; height:60px;", controlClass:String = "control" ) =
    <div class="control">
     <div class="left">{ label }</div>
     <div class="right">
      { href != null |* <a href={ href } class="greyBtn">Edit</a> }
      <div class="photoPreview">
       <img style={ style } src={ src }/>
      </div>
     </div>
    </div>

  def btns( buttons:NodeSeq ) =
    <div class="control">
     <div class="right">
      <div class="btns bottom">
       { buttons }
      </div>
     </div>
    </div>
}


