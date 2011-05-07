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

package org.tyranid.db

import scala.xml.NodeSeq

import org.tyranid.Imp.string
import org.tyranid.logic.Valid


class ViewAttribute( val view:View,
                     val att:Attribute,
                     val index:Int ) {

  def name = att.name
  def label:String = att.label

  def label( r:Record, opts:(String,String)* ):NodeSeq = <label for={ name }>{ label }</label>

  def ui( r:Record, opts:(String,String)* ) = att.domain.ui( r, this, opts:_* )
}

trait View {

  val entity:Entity

  def apply( name:String ):ViewAttribute

}

trait Record extends Valid {
  val view:View

  def apply( key:String ):AnyRef
  def update( key:String, v:AnyRef )

  /**
   * Record/Object/Document/Tuple
   */
  def /( key:String ) = apply( key ).asInstanceOf[Record]
  def /( va:ViewAttribute ) = apply( va.name ).asInstanceOf[Record]

  /**
   * Array
   */
  //def a( key:String ) = apply( key ).asInstanceOf[Array]
  //def a( va:ViewAttribute ) = apply( va.name ).asInstanceOf[Array]

  /**
   * Boolean
   */
  def b( key:String ) = apply( key ).asInstanceOf[Boolean]
  def b( va:ViewAttribute ) = apply( va.name ).asInstanceOf[Boolean]

  /**
   * Double
   */
  def d( key:String ) = apply( key ).asInstanceOf[Double]
  def d( va:ViewAttribute ) = apply( va.name ).asInstanceOf[Double]

  /**
   * Int
   */
  def i( key:String ) = apply( key ).asInstanceOf[Int]
  def i( va:ViewAttribute ) = apply( va.name ).asInstanceOf[Int]

  /**
   * Long
   */
  def l( key:String ) = apply( key ).asInstanceOf[Long]
  def l( va:ViewAttribute ) = apply( va.name ).asInstanceOf[Long]

  /**
   * Regular Expression
   */
  //def r( key:String ) = apply( key ).asInstanceOf[Long]
  //def r( va:ViewAttribute ) = apply( va.name ).asInstanceOf[Long]

  /**
   * String
   */
  def s( key:String ):String = {
    val v = apply( key )
    if ( v != null ) v.toString else ""
  }
  def s( va:ViewAttribute ):String = s( va.name )

  /**
   * Date/Time
   */
  //def d( key:String ) = apply( key ).toString
  //def d( va:ViewAttribute ) = apply( va.name ).toString


  def label( name:String, opts:(String,String)* ):NodeSeq = label( view( name ), opts:_* )
  def label( va:ViewAttribute, opts:(String,String)* ) = va.label( this, opts:_* )

  def ui( name:String, opts:(String,String)* ):NodeSeq = ui( view( name ), opts:_* )
  def ui( va:ViewAttribute, opts:(String,String)* ) = va.ui( this, opts:_* )

  def td( name:String, opts:(String,String)* ):NodeSeq = td( view( name ), opts:_* )
  def td( va:ViewAttribute, opts:(String,String)* ) = {

    <td>{ label( va ) }</td> ++
    <td>{ va.ui( this, ( opts ++ Seq( "id" -> va.name ) ):_* ) }</td>
  }
}

case class Scope( r:Record, va:Option[ViewAttribute] ) {

  def s = va.map( va => r.s( va ) )
}


