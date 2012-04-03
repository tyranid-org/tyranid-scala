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

object Blur {

  def apply( id:String ) =
    <script>{ Unparsed( """$( $('""" + id + """').blur() );""" ) }</script>
}

/*
     options:


     1.  not so monolothic ... use HTML to layout, not some Grid class ?


     2.  when we submit to a URL, we need a programmatic way to scrape the Form



 */

object Form {

  // probably swap this convention ... text() takes a NodeSeq, stext() takes a String ?
  def htext( label:String, value:NodeSeq, href:String = null, editBtnId:String = null, redirectHref:String = null, controlClass:String = "control", dialogTitle:String = null, opts:Seq[(String,String)] = null, link:Boolean = false  ) =
    <div class={ controlClass }>
     <div class="left">{ label }</div>
     <div class="right">
      { href.notBlank |* <a href={ if ( editBtnId == null ) href else "#" + editBtnId } class="greyBtn" id={ editBtnId }>Edit</a> }
      { if ( editBtnId != null ) dialog( editBtnId, href, redirectHref, if ( dialogTitle == null ) label else dialogTitle, opts ) }
      <div class="text">{ if ( link ) <a href={ value.toString } target="_blank">{ value }</a> else { value } }</div>
     </div>
    </div>

  def htext2( label1:String, label2:String, value1:NodeSeq, value2:NodeSeq, href:String = null, editBtnId:String = null, redirectHref:String = null, controlClass:String = "control", dialogTitle:String = null, opts:Seq[(String,String)] = null ) =
    <div class={ controlClass }>
     <div class="left">{ label1 }</div>
     <div class="right">
      { href.notBlank |* <a href={ if ( editBtnId == null ) href else "#" + editBtnId } class="greyBtn" id={ editBtnId }>Edit</a> }
      { if ( editBtnId != null ) dialog( editBtnId, href, redirectHref, if ( dialogTitle == null ) label1 else dialogTitle, opts ) }
      <div class="text">{ value1 }</div>
     </div>
     <div class="left" style="clear:both;">{ label2 }</div>
     <div class="right">
      <div class="text">{ value2 }</div>
     </div>
    </div>

  def htext3( label1:String, label2:String, label3:String, value1:NodeSeq, value2:String, value3:String, href:String = null, editBtnId:String = null, redirectHref:String = null, controlClass:String = "control", dialogTitle:String = null, opts:Seq[(String,String)] = null ) =
    <div class={ controlClass }>
     <div class="left">{ label1 }</div>
     <div class="right">
      { href.notBlank |* <a href={ if ( editBtnId == null ) href else "#" + editBtnId } class="greyBtn" id={ editBtnId }>Edit</a> }
      { if ( editBtnId != null ) dialog( editBtnId, href, redirectHref, if ( dialogTitle == null ) label1 else dialogTitle, opts ) }
      <div class="text">{ value1 }</div>
     </div>
     <div class="left" style="clear:both;">{ label2 }</div>
     <div class="right">
      <div class="text">{ value2 }</div>
     </div>
     <div class="left" style="clear:both;">{ label3 }</div>
     <div class="right">
      <div class="text">{ value3 }</div>
     </div>
    </div>
      
  def text( label:String, value:String, href:String = null, editBtnId:String = null, redirectHref:String = null, controlClass:String = "control", dialogTitle:String = null, opts:Seq[(String,String)] = null, link:Boolean = false ) =
    <div class={ controlClass }>
     <div class="left">{ label }</div>
     <div class="right">
      { href.notBlank |* <a href={ if ( editBtnId == null ) href else "#" + editBtnId } class="greyBtn" id={ editBtnId }>Edit</a> }
      { if ( editBtnId != null ) dialog( editBtnId, href, redirectHref, if ( dialogTitle == null ) label else dialogTitle, opts ) }
      <div class="text">{ if ( link ) <a href={value.toUrl.toString()} target="_blank">{ value }</a> else { value } }</div>
     </div>
    </div>

  def text2( label1:String, label2:String, value1:String, value2:String, href:String = null, editBtnId:String = null, redirectHref:String = null, controlClass:String = "control", dialogTitle:String = null, opts:Seq[(String,String)] = null ) =
    <div class={ controlClass }>
     <div class="left">{ label1 }</div>
     <div class="right">
      { href.notBlank |* <a href={ if ( editBtnId == null ) href else "#" + editBtnId } class="greyBtn" id={ editBtnId }>Edit</a> }
      { if ( editBtnId != null ) dialog( editBtnId, href, redirectHref, if ( dialogTitle == null ) label1 else dialogTitle, opts ) }
      <div class="text">{ value1 }</div>
     </div>
     <div class="left" style="clear:both;">{ label2 }</div>
     <div class="right">
      <div class="text">{ value2 }</div>
     </div>
    </div>

  def text3( label1:String, label2:String, label3:String, value1:String, value2:String, value3:String, href:String = null, editBtnId:String = null, redirectHref:String = null, controlClass:String = "control", dialogTitle:String = null, opts:Seq[(String,String)] = null ) =
    <div class={ controlClass }>
     <div class="left">{ label1 }</div>
     <div class="right">
      { href.notBlank |* <a href={ if ( editBtnId == null ) href else "#" + editBtnId } class="greyBtn" id={ editBtnId }>Edit</a> }
      { if ( editBtnId != null ) dialog( editBtnId, href, redirectHref, if ( dialogTitle == null ) label1 else dialogTitle, opts ) }
      <div class="text">{ value1 }</div>
     </div>
     <div class="left" style="clear:both;">{ label2 }</div>
     <div class="right">
      <div class="text">{ value2 }</div>
     </div>
     <div class="left" style="clear:both;">{ label3 }</div>
     <div class="right">
      <div class="text">{ value3 }</div>
     </div>
    </div>
      
  def int( label:String, value:Int, href:String = null, controlClass:String = "control" ) =
    <div class={ controlClass }>
     <div class="left">{ label }</div>
     <div class="right">
      { href.notBlank |* <a href={ href } class="greyBtn">Edit</a> }
      <div class="text">{ value }</div>
     </div>
    </div>

  def thumbnail( label:String, src:String, href:String = null, editBtnId:String = null, redirectHref:String = null, style:String = "width:60px; height:60px;",
                 controlClass:String = "control", dialogTitle:String = null, opts:Seq[(String,String)] = null, hrefLabel:String = "Edit" ) = {
    <div class="control">
     <div class="left">{ label }</div>
     <div class="right">
      { href.notBlank |* <a href={ if ( editBtnId == null ) href else "#" + editBtnId } class="greyBtn" id={ editBtnId }>{ hrefLabel }</a> }
      { if ( editBtnId != null ) dialog( editBtnId, href, redirectHref, if ( dialogTitle == null ) label else dialogTitle, opts ) }
      <div class="photoPreview">
       <img style={ style } src={ src }/>
      </div>
     </div>
    </div>
  }

  def btns( buttons:NodeSeq ) =
    <div class="control">
     <div class="right">
      <div class="btns bottom">
       { buttons }
      </div>
     </div>
    </div>
  
  
  def dialog( elId: String, postEndpoint:String, redirectEndpoint:String, title:String, opts:Seq[(String,String)] = null ) = 
    <head>
      <script>
        {
         val optsStr:StringBuffer = new StringBuffer

         if ( opts != null ) {
           opts.foreach {
             _ match {
             case ( n, v ) =>
               optsStr.append( "vd.option( \"" + n + "\", \"" + v + "\" );" )
             case _ =>
             }
           }    
         }

         Unparsed("""
$( function() {
  $( '#""" + elId + """' ).on( 'click', function( e ) {
    var vd = new VDialog( """" + postEndpoint + """", """" + redirectEndpoint + """", """" + title + """" ); """ +
    optsStr.toString +
""" vd.open();
    return false;
  });
});
        """)
        }
      </script>
    </head>
  
}


