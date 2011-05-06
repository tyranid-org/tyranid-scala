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

import org.tyranid.Imp.string

class ViewAttribute( val view:View,
                     val att:Attribute,
                     val index:Int ) {

  def name = att.name

  def ui( r:Record, opts:(String,String)* ) = att.domain.ui( r, this, opts:_* )
}

trait View {

  val entity:Entity

  def apply( name:String ):ViewAttribute

}

trait Record {
  val view:View

  def apply( key:String ):AnyRef
  def update( key:String, v:AnyRef )

  /**
   * Record/Object/Document/Tuple
   */
  def /( key:String ) = apply( key ).asInstanceOf[Record]

  /**
   * Array
   */
  //def a( key:String ) = apply( key ).asInstanceOf[Array]

  /**
   * Boolean
   */
  def b( key:String ) = apply( key ).asInstanceOf[Boolean]

  /**
   * Double
   */
  def d( key:String ) = apply( key ).asInstanceOf[Double]

  /**
   * Int
   */
  def i( key:String ) = apply( key ).asInstanceOf[Int]

  /**
   * Long
   */
  def l( key:String ) = apply( key ).asInstanceOf[Long]

  /**
   * Regular Expression
   */
  //def r( key:String ) = apply( key ).asInstanceOf[Long]

  /**
   * String
   */
  def s( key:String ) = {
    val v = apply( key )
    if ( v != null ) v.toString else ""
  }

  /**
   * Date/Time
   */
  //def d( key:String ) = apply( key ).toString


  def label( name:String, opts:(String,String)* ) = <label for={ name }/>

  def ui( name:String, opts:(String,String)* ) = view( name ).ui( this, opts:_* )

  def td( name:String, opts:(String,String)* ) = {
    val va = view( name )

    <td>{ label( name ) }</td> ++
    <td>{ va.ui( this, ( opts ++ Seq( "id" -> name ) ):_* ) }</td>
  }
}


