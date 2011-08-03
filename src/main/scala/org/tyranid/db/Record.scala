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

import org.bson.types.ObjectId
import com.mongodb.BasicDBList

import scala.collection.mutable
import scala.xml.NodeSeq

import org.tyranid.Imp._
import org.tyranid.bson.BsonObject
import org.tyranid.db.es.Es
import org.tyranid.logic.{ Invalid, Valid }
import org.tyranid.ui.{ UiObj }


trait Path {

  def pathSize:Int
  def pathAt( idx:Int ):ViewAttribute

  def leaf:ViewAttribute
}

case class MultiPath( vas:ViewAttribute* ) extends Path {

  def pathSize = vas.length
  def pathAt( idx:Int ) = vas( idx )

  def leaf = vas.last
}

class ViewAttribute( val view:View,
                     val att:Attribute,
                     val index:Int ) extends Valid with Path {

  def temporary = att.temporary

  def name = att.name
  def label:String = att.label

  def label( r:Record, opts:(String,String)* ):NodeSeq = <label for={ name }>{ label }</label>

  def toView:View = att.domain.asInstanceOf[Entity].makeView


  /*
   * * *   Path
   */

  def pathSize = 1
  def pathAt( idx:Int ) = this

  def leaf = this


  /*
   * * *   Validation
   */

  override def validations = att.validations

  def invalids( scope:Scope ) = {
    require( scope.va.get == this )

    ( for ( invalidOpt <- validations.map( validator => validator( scope ) );
            invalid <- invalidOpt )
        yield invalid ) ++
    ( for ( pair <- scope.rec.extraVaValidations;
            if pair._1 == this;
            invalid <- pair._2( scope ) )
        yield invalid )
  }
}

trait View {

  val entity:Entity

  def vas:Iterable[ViewAttribute]

  lazy val keyVa   = entity.keyAtt.map( a => apply( a.name ) )
  lazy val labelVa = entity.labelAtt.map( a => apply( a.name ) )

  def apply( name:String ):ViewAttribute
  def apply( idx:Int ):ViewAttribute


  private val uis = mutable.Map[String,UiObj]()

  def ui( name:String, ui: => UiObj ) = synchronized {
    uis.getOrElseUpdate( name, ui.bind( this ) )
  }

  def path( path:String ):Path = {

    val names =
      path.split(
        if ( path.indexOf( '.' ) != -1 ) "."
        else                             "_" )
    
    val nlen = names.length

    if ( nlen == 1 ) {
      apply( names( 0 ) )
    } else {
      val pbuf = new Array[ViewAttribute]( nlen )

      var ni = 0 
      var view = this

      while ( ni < nlen ) {
        val va = view( names( ni ) )
        pbuf( ni ) = va

        ni += 1

        if ( ni < nlen )
          view = va.toView
      }

      MultiPath( pbuf:_* )
    }
  }
}

object Record {


  def byTid( tid:String, only:Entity = null ):Option[Record] = {

    val ( entityTid, recordTid ) = tid.splitAt( 4 )

    Entity.
      byTid( entityTid ).
      filter( e => only == null || only == e ).
      flatMap( entity => entity.byRecordTid( recordTid ) )
  }

}

trait Record extends Valid with BsonObject {
  val view:View

  val parent:Record

  def entity = view.entity

  var isAdding:Boolean = false

  def submit {
    require( parent == null )
    submitFlagged = true
  }

  def hasSubmitted:Boolean = submitFlagged || ( parent != null && parent.hasSubmitted )

  private var submitFlagged:Boolean = false

  
  final def apply( key:String ):AnyRef = apply( view( key ) )
  final def update( key:String, v:Any ):Unit = update( view( key ), v )

  def apply( va:ViewAttribute ):AnyRef
  def update( va:ViewAttribute, v:Any )
  
  def idLabel:(AnyRef,String) = ( apply( view.keyVa.get ), s( view.labelVa.get ) )

  def tid = entityTid + recordTid

  def entityTid = view.entity.tid
  def recordTid = view.keyVa.flatten( kva => kva.att.domain.tid( this, kva ), "-invalid" )

  /**
   * Record/Object/Document/Tuple
   */
  def /( va:ViewAttribute )    = apply( va ).asInstanceOf[Record]
  
  /**
   * BSON ObjectId
   */
  def oid( va:ViewAttribute ) = apply( va ).asInstanceOf[ObjectId]

  /**
   * BSON Object
   */
  override def o( name:String ) = o( view( name ) )
  def o( va:ViewAttribute ) = apply( va ).asInstanceOf[BsonObject]

  final def rec( name:String ):Record = rec( view( name ) )
  def rec( va:ViewAttribute ):Record

  /**
   * Array
   */
  def a( va:ViewAttribute ) = apply( va ).asInstanceOf[BasicDBList]

  /**
   * Boolean
   */
  def b( va:ViewAttribute ) = apply( va ).asInstanceOf[Boolean]

  /**
   * Double
   */
  def d( va:ViewAttribute ) = apply( va ).asInstanceOf[Double]

  /**
   * Int
   */
  def i( va:ViewAttribute ) = apply( va ).asInstanceOf[Int]

  /**
   * Long
   */
  def l( va:ViewAttribute ) = apply( va ).asInstanceOf[Long]

  /**
   * Regular Expression
   */
  //def r( va:ViewAttribute ) = apply( va ).asInstanceOf[Long]

  /**
   * String
   */
  def s( va:ViewAttribute ):String = {
    val v = apply( va )
    
    // TODO:  need to make this work with BsonObject i.e. View-less Mongo objects
    v != null |* /*{
      va.att.domain match {
        case link:DbLink => link.toEntity.labelFor( v )
        case _           => */ v.toString /*
      }
    }
    */
  }

  /**
   * Date/Time
   */
  //def d( va:ViewAttribute ) = apply( va.name ).toString


  def label( va:ViewAttribute ):String = {
    va.att.domain.asInstanceOf[DbLink].toEntity.labelFor( apply( va ) )
  }

  def label( key:String ):String = label( view( key ) )

  
  /*
   * * *   Validation
   */

  /**
   * List of currently-invalid view attributes for this record.
   */
  val invalids = mutable.BitSet()

  var extraVaValidations:List[ ( ViewAttribute, ( Scope ) => Option[Invalid] ) ] = Nil

  def invalids( scope:Scope ):Iterable[Invalid] =
    ( for ( va <- view.vas;
            vaScope = scope.at( va );
            invalidOpt <- va.validations.map( validator => validator( vaScope ) );
            invalid <- invalidOpt )
        yield invalid ) ++
    ( for ( va <- view.vas;
            if va.att.domain.isInstanceOf[Entity];
            r = rec( va );
            invalid <- r.invalids( scope.at( va ) ) )
        yield invalid )


  /*
   * * *   Persistence
   */

  def save {
    if ( view.entity.isSearchable )
      Es.index( this )
  }
}

case class Scope( rec:Record,
                  initialDraw:Boolean = false,
                  saving:Boolean = true,
                  captcha:Boolean = false,
                  path:Option[Path] = None ) {

  def s = va.map( rec.s )

  def at( name:String ):Scope = at( rec.view.path( name ) )

  def at( path:Path ) = {
    val plen = path.pathSize - 1

    var r = rec
    var pi = 0
    while ( pi < plen ) {
      val va = path.pathAt( pi )
      r = r.rec( va )
      pi += 1
    }

    val va = path.pathAt( pi )

    if ( va.att.domain.isInstanceOf[Entity] )
      copy( rec = r.rec( va ), path = None )
    else
      copy( rec = r, path = Some( path ) )
  }

  def va = path.map( _.leaf )

  def required = rec.hasSubmitted |* s.filter( _.isBlank ).map( s => Invalid( this, "Please fill in." ) )

  def draw( name:String, ui: => UiObj ) = rec.view.ui( name, ui ).draw( this )
}


