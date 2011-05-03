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

  def apply( key:Symbol ):AnyRef = apply( key.toString )
  def update( key:Symbol, v:AnyRef ) { update( key.toString, v ) }

  /**
   * Record/Object/Document/Tuple
   */
  def /( key:String ) = apply( key ).asInstanceOf[Record]
  def /( key:Symbol ) = apply( key.toString ).asInstanceOf[Record]

  /**
   * Array
   */
  //def a( key:String ) = apply( key ).asInstanceOf[Array]
  //def a( key:Symbol ) = apply( key.toString ).asInstanceOf[Array]

  /**
   * Boolean
   */
  def b( key:String ) = apply( key ).asInstanceOf[Boolean]
  def b( key:Symbol ) = apply( key.toString ).asInstanceOf[Boolean]

  /**
   * Long
   */
  def d( key:String ) = apply( key ).asInstanceOf[Double]
  def d( key:Symbol ) = apply( key.toString ).asInstanceOf[Double]

  /**
   * Long
   */
  def i( key:String ) = apply( key ).asInstanceOf[Int]
  def i( key:Symbol ) = apply( key.toString ).asInstanceOf[Int]

  /**
   * Long
   */
  def l( key:String ) = apply( key ).asInstanceOf[Long]
  def l( key:Symbol ) = apply( key.toString ).asInstanceOf[Long]

  /**
   * Regular Expression
   */
  //def r( key:String ) = apply( key ).asInstanceOf[Long]
  //def r( key:Symbol ) = apply( key.toString ).asInstanceOf[Long]

  /**
   * String
   */
  def s( key:String ) = apply( key ).toString
  def s( key:Symbol ) = apply( key.toString ).toString

  /**
   * Date/Time
   */
  //def d( key:String ) = apply( key ).toString
  //def d( key:Symbol ) = apply( key.toString ).toString


  def ui( name:String, opts:(String,String)* ) = view( name ).ui( this, opts:_* )
  def ui( name:Symbol, opts:(String,String)* ) = view( name.toString ).ui( this, opts:_* )
}


