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

import scala.collection.mutable
import scala.xml.NodeSeq

import org.tyranid.Imp._
import org.tyranid.logic.{ Invalid, Valid }


class ViewAttribute( val view:View,
                     val att:Attribute,
                     val index:Int ) extends Valid {

  def temporary = att.temporary

  def name = att.name
  def label:String = att.label

  def label( r:Record, opts:(String,String)* ):NodeSeq = <label for={ name }>{ label }</label>


  /*
   * * *   Validation
   */

  override def validations = att.validations

  def invalids( scope:Scope ) = {
    require( scope.va.get == this )

    for ( invalidOpt <- validations.map( validator => validator( scope ) );
          invalid <- invalidOpt )
      yield invalid
  }
}

trait View {

  val entity:Entity

  def vas:Iterable[ViewAttribute]

  lazy val keyVa   = entity.keyAtt.map( a => apply( a.name ) )
  lazy val labelVa = entity.labelAtt.map( a => apply( a.name ) )

  def apply( name:String ):ViewAttribute
  def apply( idx:Int ):ViewAttribute
}

trait Record extends Valid {
  val view:View

  var isAdding:Boolean = false
  var isInitial:Boolean = true

  final def apply( key:String ):AnyRef = apply( view( key ) )
  final def update( key:String, v:AnyRef ):Unit = update( view( key ), v )

  def apply( va:ViewAttribute ):AnyRef
  def update( va:ViewAttribute, v:AnyRef )


  def idLabel:(AnyRef,String) = ( apply( view.keyVa.get ), s( view.labelVa.get ) )

  /**
   * Record/Object/Document/Tuple
   */
  def /( key:String )       = apply( key ).asInstanceOf[Record]
  def /( va:ViewAttribute ) = apply( va ).asInstanceOf[Record]

  /**
   * Array
   */
  //def a( key:String )       = apply( key ).asInstanceOf[Array]
  //def a( va:ViewAttribute ) = apply( va ).asInstanceOf[Array]

  /**
   * Boolean
   */
  def b( key:String )       = apply( key ).asInstanceOf[Boolean]
  def b( va:ViewAttribute ) = apply( va ).asInstanceOf[Boolean]

  /**
   * Double
   */
  def d( key:String )       = apply( key ).asInstanceOf[Double]
  def d( va:ViewAttribute ) = apply( va ).asInstanceOf[Double]

  /**
   * Int
   */
  def i( key:String )       = apply( key ).asInstanceOf[Int]
  def i( va:ViewAttribute ) = apply( va ).asInstanceOf[Int]

  /**
   * Long
   */
  def l( key:String )       = apply( key ).asInstanceOf[Long]
  def l( va:ViewAttribute ) = apply( va ).asInstanceOf[Long]

  /**
   * Regular Expression
   */
  //def r( key:String )       = apply( key ).asInstanceOf[Long]
  //def r( va:ViewAttribute ) = apply( va ).asInstanceOf[Long]

  /**
   * String
   */
  def s( key:String ):String = {
    val v = apply( key )
    if ( v != null ) v.toString else ""
  }
  def s( va:ViewAttribute ):String = {
    val v = apply( va )
    if ( v != null ) v.toString else ""
  }

  /**
   * Date/Time
   */
  //def d( key:String ) = apply( key ).toString
  //def d( va:ViewAttribute ) = apply( va.name ).toString


  /*
   * * *   Validation
   */

  /**
   * List of currently-invalid view attributes for this record.
   */
  val invalids = mutable.BitSet()

  def invalids( scope:Scope ) =
    for ( va <- view.vas;
          vaScope = scope.at( va );
          invalidOpt <- va.validations.map( validator => validator( vaScope ) );
          invalid <- invalidOpt )
      yield invalid
}

case class Scope( rec:Record, va:Option[ViewAttribute] = None ) {

  def s = va.map( rec.s )

  def at( name:String ):Scope = at( rec.view( name ) )
  def at( va:ViewAttribute )  = copy( va = Some( va ) )

  def required = !rec.isInitial |* s.filter( _.isBlank ).map( s => Invalid( this, "Please fill in." ) )
}


