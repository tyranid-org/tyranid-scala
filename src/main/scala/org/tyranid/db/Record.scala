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

import java.util.Date

import org.bson.types.ObjectId
import com.mongodb.BasicDBList

import scala.annotation.tailrec
import scala.collection.mutable
import scala.xml.NodeSeq

import org.tyranid.Imp._
import org.tyranid.bson.BsonObject
import org.tyranid.logic.{ Invalid, Valid }
import org.tyranid.ui.{ UiObj }


/*
 * * *  ViewAttribute
 */

case class ViewAttribute( val view:View,
                          val att:Attribute,
                          val index:Int ) extends Valid with Path with PathNode {

  def temporary = att.temporary

  override def name  = att.name
  override def label = att.label

  def label( r:Record, opts:(String,String)* ):NodeSeq = <label for={ name }>{ label }</label>
  def see( v:Any ) = att.see( v )

  def toView = View.from( att.domain )

  def domain = att.domain


  /*
   * * *   Path
   */

  def pathSize = 1
  def pathAt( idx:Int ) = this


  /*
   * * *   Validation
   */

  override def validations = att.validations

  def invalids( scope:Scope ) = {
    require( scope.va == None || scope.va.get == this )

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
//    println( name + " = " + ui )
    
    uis.getOrElseUpdate( name, ui.bind( this ) )
  }

  def ui( name:String ) = synchronized { uis( name ) }

  def path( path:String, sep:Char = 0 ):Path = Path.parse( this, path, sep )

  def paths( paths:Seq[String] ) = paths.map( path => Path.parse( this, path ) )
}

object View {

  @tailrec
  def from( d:Domain ):View =
    d match {
    case en:Entity     => en.makeView
    case link:DbLink   => link.toEntity.makeView
    case array:DbArray => from( array.of )
    case _             => null
    }
}

object Record {


  def byTid( tid:String, only:Entity = null ):Option[Record] = {

    val ( entityTid, recordTid ) = tid.splitAt( 4 )

    try {
      Entity.
        byTid( entityTid ).
        filter( e => only == null || only == e ).
        flatMap( entity => entity.byRecordTid( recordTid ) )
    } catch {
    case e =>
      e.logWith( "m" -> ( "tid[" + tid + "]" )  )
      None
    }
  }

}

trait Record extends Valid with BsonObject {
  val view:View

  val parent:Record

  def entity = view.entity

  def has( key:String ) = has( view( key ) )
  def has( va:ViewAttribute ):Boolean

  final def apply( key:String ):AnyRef = apply( view( key ) )

  final def update( key:String, v:Any ):Unit = update( view( key ), v )

  def apply( va:ViewAttribute ):AnyRef
  def update( va:ViewAttribute, v:Any )
 
  def label = s( view.labelVa.get )
  def idLabel:(AnyRef,String) = ( apply( view.keyVa.get ), label )

  def tid = entityTid + recordTid

  def entityTid = view.entity.tid
  def recordTid = view.keyVa.flatten( kva => kva.att.domain.tid( this, kva ), "-invalid" )

  /**
   * Record/Object/Document/Tuple
   */
  def /( va:ViewAttribute )   = apply( va ).asInstanceOf[Record]

  override def oid = id.as[ObjectId]
  
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
  def t( va:ViewAttribute ) = apply( va.name ).asInstanceOf[Date]


  def label( va:ViewAttribute ):String = {
    va.att.domain.asInstanceOf[DbLink].toEntity.labelFor( apply( va ) )
  }

  def label( key:String ):String = label( view( key ) )


  /*
   * * *   Forms
   */

  var isAdding:Boolean = false

  def submit {
    require( parent == null )
    submitFlagged = true
  }

  def hasSubmitted:Boolean = submitFlagged || ( parent != null && parent.hasSubmitted )

  private var submitFlagged:Boolean = false

  
  /*
   * * *   Validation
   */

  /**
   * List of currently-invalid view attributes for this record.
   */
  val invalids = mutable.BitSet()

  var extraVaValidations:List[ ( ViewAttribute, ( Scope ) => Option[Invalid] ) ] = Nil

  def invalids( scope:Scope ):Iterable[Invalid] = invalids( scope, view.vas )

  def invalids( scope:Scope, vas:Iterable[ViewAttribute] ):Iterable[Invalid] =
    ( for ( va <- vas;
            vaScope = scope.at( va );
            invalid <- va.invalids( vaScope ) )
        yield invalid ) ++
    ( for ( va <- vas;
            if va.att.domain.isInstanceOf[Entity];
            vaScope = scope.at( va );
            invalid <- vaScope.rec.invalids( vaScope ) )
        yield invalid )


  /*
   * * *   Manipulation
   */

  /**
   * This is a copy of the record that was originally read in.  This is used for edit differencing.
   */
  var original:Record = null

  def snapshot = { 
    original = deep; this
  }

  /**
   * This performs a deep copy of this record.  This does not copy the <b>original</b> record, if any.
   */
  def deep:Record

  /*
   * * *   Persistence
   */

  final def save = entity.save( this )
}

case class Scope( rec:Record,
                  initialDraw:Boolean = false,
                  saving:Boolean = true,
                  captcha:Boolean = false,
                  path:Option[Path] = None ) {

  def s = va.map( rec.s )

  def value = va.map( rec.apply )

  def at( name:String ):Scope = at( rec.view.path( name ) )

  def at( path:Path ) = {
    val plen = path.pathSize - 1

    var r = rec
    var pi = 0
    while ( pi < plen ) {
      val va = path.pathAt( pi ).as[ViewAttribute]
      r = r.rec( va )
      pi += 1
    }

    val va = path.pathAt( pi ).as[ViewAttribute]

    if ( va.att.domain.isInstanceOf[Entity] )
      copy( rec = r.rec( va ), path = None )
    else
      copy( rec = r, path = Some( path ) )
  }

  def va = path.map( _.leaf )

  def required = rec.hasSubmitted |* s.filter( _.isBlank ).map( s => Invalid( this, "Please fill in." ) )

  def draw    ( ui:UiObj ) = ui.draw( this )

  def submit( rec:Record, ui:UiObj ) = {
    rec.submit
    ui.extract( this )

    for ( f <- ui.fields;
          path = f.path;
          pathScope = this.at( path );
          invalid <- path.leaf.invalids( pathScope ) )
      yield invalid
  }
}


