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

package org.tyranid.db

import java.util.Date

import org.bson.types.ObjectId
import com.mongodb.BasicDBList

import scala.annotation.tailrec
import scala.collection.mutable
import scala.xml.NodeSeq

import org.tyranid.Imp._
import org.tyranid.bson.BsonObject
import org.tyranid.db.meta.Tid
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ MongoEntity, MongoRecord }
import org.tyranid.logic.{ Invalid, Valid }
import org.tyranid.profile.User
import org.tyranid.report.Run
import org.tyranid.QuickCache
import org.tyranid.ui.{ PathField, Search, UiObj }


/*
 * * *  ViewAttribute
 */

case class ViewAttribute( val view:View,
                          val att:Attribute,
                          val index:Int,
                          val search:Search = null,
                          val fullName:String = null ) extends Valid with Path with PathNode {

  def temporary = search != null || att.temporary

  override lazy val name =
    if ( fullName.isBlank ) att.name
    else                    fullName

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

  def slice( fromIdx:Int, toIdx:Int ):Path = {
    require( fromIdx <= 1 && toIdx <= 1 )
    if ( toIdx == fromIdx ) EmptyPath
    else                    this
  }


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

  lazy val idVa    = entity.idAtt.map( a => apply( a.name ) )
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

  def getByTid( tid:String, only:Entity = null ):Record = {
    if ( tid.isBlank )
      return null

    val ( entityTid, recordTid ) = tid.splitAt( 4 )

    try {
      Entity.
        byTid( entityTid ).
        filter( e => only == null || only == e ).
        flatMap( entity => entity.byRecordTid( recordTid ) ).
        getOrElse( null )
    } catch {
    case e =>
      e.logWith( "m" -> ( "tid[" + tid + "]" )  )
      null
    }
  }

  def byTid( tid:String, only:Entity = null ):Option[Record] = Option( getByTid( tid, only ) )

  def getByTids( tids:Seq[String] ):Seq[Record] =
    ( for ( entry <- tids.groupBy( Tid.entityTid );
            rEntity <- Entity.byTid(entry._1 );
            entity = rEntity.as[MongoEntity];
            entityTids = entry._2 ) yield {
        entity.db.find( Mobj( "_id" -> Mobj( $in -> entityTids.map( entity.tidToId ).toMlist ) ) ).map( entity.apply ).toSeq
      }
    ).toSeq.flatten
}

trait Record extends Valid with BsonObject with QuickCache {
  val view:View

  def flatten = Path.flatten( this )

  val parent:Record

  def root:Record =
    if ( parent == null ) this
    else                  parent.root

  def entity = view.entity

  def has( key:String ) = has( view( key ) )
  def has( va:ViewAttribute ):Boolean

  final def apply( key:String ):AnyRef = apply( view( key ) )
  final def update( key:String, v:Any ):Unit = update( view( key ), v )

  def apply( va:ViewAttribute ):AnyRef
  def update( va:ViewAttribute, v:Any )
 
  def tid = entityTid + recordTid
  def entityTid = view.entity.tid
  def recordTid = view.idVa.flatten( va => va.att.domain.idToRecordTid( this( va ) ), view.entity.problem( "embedded entities don't have IDs" ) )

  override def oid = id.as[ObjectId]

  def ensureId =
    recordTid match {
    case null | "null" | "-invalid" => save
    case _ =>
    }

  def clear:Unit = throw new UnsupportedOperationException

  def remove( key:String ) { remove( view( key ) ) }
  def remove( va:ViewAttribute ):Unit

  def clear( keys:String* ) =
    for ( key <- keys )
      remove( view( key ) )


  /*
   * * *   S e a r c h
   */

  var searchIndex = true

  def searchText:String = null


  /*
   * * *   Labels & Icons
   */

  def label                   = view.labelVa.flatten( va => s( va ), "n/a" )
  def idLabel:(AnyRef,String) = ( apply( view.idVa.get ), label )

  def label( va:ViewAttribute )  = va.att.domain.asInstanceOf[DbLink].toEntity.labelFor( apply( va ) )
  def label( key:String ):String = label( view( key ) )

  def icon:String = {
    entity.iconAtt match {
    case Some( att ) =>
      val icon = s( att.name )
      if ( icon.notBlank )
        return icon
    
    case None =>
    }

    entity.defaultIcon
  }


  /*
   * * *   Security
   */

  def canView( user:User ) = entity.canView( this, user )


  /*
   * * *   Computations
   */

  var _computed = false
  def compute( client:Boolean = false, extra:Seq[String] = Nil, temporary:Boolean = true, force:Boolean = false ) {
    if ( !_computed || force ) {
      for ( c <- view.entity.computations;
            if !client || c.client || extra.contains( c.name );
            if temporary || !c.temporary )
        this( c.name ) = c.computation( this )
  
      for ( va <- view.vas;
            if va.att.client && has( va ) ) {
        this( va ) match {
        case rec:Record =>
          rec.compute( client, Nil )
        case arr:BasicDBList if va.domain.is[DbArray] && va.domain.as[DbArray].of.is[Entity] =>
          val mEnt = va.domain.as[DbArray].of.as[MongoEntity]
  
          for ( i <- 0 until arr.size() )
            mEnt.recify( arr( i ), this.as[MongoRecord], rec => arr( i ) = rec ).compute( client, Nil )
            
        case _ =>
        }
      }
      
      _computed = true
    }
  }

  /**
   * Record/Object/Document/Tuple
   */
  def /( va:ViewAttribute )   = apply( va ).asInstanceOf[Record]

  def oid( va:ViewAttribute ) = apply( va ).asInstanceOf[ObjectId]

  def tid( va:ViewAttribute ):String = va.domain.tid( apply( va ) )
  def tid( key:String ):String       = tid( view( key ) )

  override def o( name:String ) = o( view( name ) )
  def o( va:ViewAttribute ) = apply( va ).asInstanceOf[BsonObject]

  final def rec( name:String ):Record = rec( view( name ) )
  def rec( va:ViewAttribute ):Record

  def a( va:ViewAttribute ) = apply( va ).asInstanceOf[BasicDBList]
  def b( va:ViewAttribute ) = apply( va )._b
  def d( va:ViewAttribute ) = apply( va )._d
  def i( va:ViewAttribute ) = apply( va )._i
  def l( va:ViewAttribute ) = apply( va )._l
  //def r( va:ViewAttribute ) = apply( va ).asInstanceOf[Long] // r = regular expression
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
  def t( va:ViewAttribute ) = apply( va.name )._t


  /*
   * * *   Forms
   */

  var isAdding:Boolean = false

  def submit {
    //require( parent == null )
    submitFlagged = true
  }
  
  def clearSubmit { submitFlagged = false }

  def hasSubmitted:Boolean = submitFlagged || ( parent != null && parent.hasSubmitted )

  private var submitFlagged:Boolean = false

  
  /*
   * * *   Validation
   */

  /**
   * List of currently-invalid view attributes for this record.
   */
  val invalids = mutable.BitSet()

  var extraVaValidations:List[ ( ViewAttribute, Scope => Option[Invalid] ) ] = Nil

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
   * This performs a deep copy of this record.  This does not copy the <strong>original</strong> record, if any.
   */
  def deep:Record


  /*
   * * *   Persistence
   */

  final def save   = entity.save( this )
  final def delete = entity.delete( this )


  def matchesSearch( run:Run ):Boolean = {
    val report = run.report
    val searchRec = report.searchRec

    report.query.searchFields forall { sf =>
      val value = searchRec( sf.name )
      println( sf.name + ": " + value )
      value == null || sf.matchesSearch( run, value, this )
    }
  }

  def eye = Tid.eye( tid )
}

/*
   Levels of Scope:

   root  ... the topmost scope
   db    ... what the closest non-embedded Record is relative to ( currently db scope == root scope, but this might change in the future )
   field ... what the PathFields are relative to
   va    ... the most detailed Scope


   scope.s( f ) vs. scope.s

 */
case class Scope( rec:Record,
                  initialDraw:Boolean = false,
                  saving:Boolean = true,
                  captcha:Boolean = false,
                  path:Path = EmptyPath,
                  pathFromParent:Path = EmptyPath,
                  run:Run = null,
                  filtering:Boolean = false,
                  vaScope:Boolean = false,
                  parent:Scope = null ) {

  def va    = Option( path.leaf )
  def s     = va.map( rec.s )
  def value = va.map( rec.apply )

  @tailrec
  final def root:Scope =
    if ( parent == null ) this
    else                  parent.root

  final def fieldRec:Record =
    if ( vaScope ) parent.fieldRec
    else           rec

  def at( name:String ):Scope = at( rec.view.path( name ) )

  def at( path:Path, vaScope:Boolean = false ):Scope = {
    val plen = path.pathSize

    var r = rec
    var pi = 0
    var lastRecPi = 0
    while ( pi < plen ) {
      val va = path.pathAt( pi ).as[ViewAttribute]

      if (  pi + 1 < plen && path.pathAt( pi+1 ).isInstanceOf[ArrayIndex] ) {
        val ai = path.pathAt( pi+1 ).as[ArrayIndex]
        val array = r.a( va )
        val v = array( ai.idx )

        r = va.domain.as[DbArray].of.as[MongoEntity].recify( v, r.as[MongoRecord], rec => array( ai.idx ) = rec )

        pi += 2
        lastRecPi = pi
      } else if ( pi + 1 < plen && path.pathAt( pi+1 ).isInstanceOf[ArrayId] ) {
        val aid = path.pathAt( pi+1 ).as[ArrayId]
        val array = r.a( va )
        val idx = array.aidToIdx( aid.id )
        val v = array( idx )

        r = va.domain.as[DbArray].of.as[MongoEntity].recify( v, r.as[MongoRecord], rec => array( idx ) = rec )

        pi += 2
        lastRecPi = pi
      } else {
        pi += 1

        if ( va.att.domain.isInstanceOf[Entity] ) {
          r = r.rec( va )
          lastRecPi = pi
        } else {
          if ( pi < plen )
            problem( "intermediate view attributes in a path must point to entities" )
        }
      }
    }

    copy( rec = r, path = path.slice( lastRecPi, plen ), pathFromParent = path.slice( 0, lastRecPi ), parent = this, vaScope = vaScope )
  }

  def required = rec.hasSubmitted |* s.filter( _.isBlank ).map( s => Invalid( this, "Please fill in " + va.get.label + "." ) )

  def draw( ui:UiObj ) = ui.draw( this )

  def submit( rec:Record, ui:UiObj ) = {
    rec.submit
    ui.extract( this )

    for ( f <- ui.fields;
          path = f.path;
          pathScope = this.at( path );
          invalid <- path.leaf.invalids( pathScope ) )
      yield invalid
  }

  def pathName_ = path.name_

  def fullPath = {
    val b = mutable.ArrayBuffer[PathNode]()

    def addParent( p:Scope ) {
      if ( p != null ) {
        addParent( p.parent )
        p.pathFromParent.appendNodesTo( b )
      }
    }

    addParent( parent )
    pathFromParent.appendNodesTo( b )
    path.appendNodesTo( b )

    Path.fromNodes( b )
  }

  def get( f:PathField )        = f.path.get( fieldRec )
  def set( f:PathField, v:Any ) = f.path.set( fieldRec, v )
  def remove( f:PathField )     = f.path.remove( fieldRec )
  def a_?( f:PathField )        = f.path.a_?( fieldRec )
  def b( f:PathField )          = f.path.b( fieldRec )
  def s( f:PathField )          = f.path.s( fieldRec )
  def t( f:PathField )          = f.path.t( fieldRec )
}


