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

import scala.collection.mutable
import scala.collection.mutable.{ ArrayBuffer, Buffer }
import scala.xml.NodeSeq

import org.tyranid.Imp._
import org.tyranid.db.es.{ Es, NoSearch, Searchable }
import org.tyranid.db.meta.{ Tid, TidItem }
import org.tyranid.db.tuple.{ TupleView, Tuple }
import org.tyranid.logic.{ Invalid, Valid }
import org.tyranid.profile.User
import org.tyranid.report.{ Run, Sort }


trait AttributeAnnotation


/*
 * * *   A t t r i b u t e s
 */

class Attribute( val entity:Entity, val name:String ) extends DbItem with Valid {
	var domain:Domain = null
  var label = name.camelCaseToSpaceUpper
  var help = NodeSeq.Empty
  var required = false
  var internal = false
  var owner = false
  var search:Searchable = NoSearch
  var computation: ( Record ) => Any = null
  var noVersion = false
  
  override def toString = entity.name + "." + name

  def see( v:Any ) = domain.see( v )

  /**
   * Indicates whether this va is persisted in the database or is just used as a temporary form field.
   */
  var temporary:Boolean = false

  /**
   * Indicates whether this property is passed by default down to the client.
   */
  var client:Boolean = false

  /**
   * Indicates whether this property requires authorization to be sent down.
   */
  var auth:Boolean = false

  /**
   *    DSL ... TODO:  move this out to a builder object
   */

	def is( domain:Domain ) = { this.domain = domain; this }
  def as( label:String ) = { this.label = label; this }
  def is( str:String ) = {
    str match {
    case "id"        => isId = true
                        if ( entity.isInstanceOf[org.tyranid.db.mongo.MongoEntity] && name != "_id" )
                          throw new IllegalArgumentException( "Cannot mark '" + name + "' as an ID on the MongoDB entity '" + entity.name + "' -- MongoDB IDs must be called '_id'." )
    case "label"     => isLabel = true
    case "required"  => required = true; localValidations ::= ( _.required )
    case "temporary" => temporary = true

    case "owner"     => owner = true
                        if ( !domain.hasLinks )
                          throw new IllegalArgumentException( "Cannot mark '" + name + "' as an owner on the entity '" + entity.name + "' -- it must either be a link/tid or an array of links/tids." )

    // for example, "aid" is internal because it is not exposed to the end-user
    case "internal"  => internal = true
    case "inherit"   => if ( domain == null )
                          throw new IllegalArgumentException( "Cannot find inherited attribute '" + name + "' in the entity '" + entity.name + "'." )
    case "noVersion" => noVersion = true
    case "client"    => client = true
    case "auth"      => auth = true
    }

    this
  }
  def is( search:Searchable ) = { this.search = search; this }
  def help( ns:NodeSeq ):Attribute = { help = ns; this }

  def isComputed = computation != null
  def computed( computation: ( Record ) => Any ) = this.computation = computation

  private var annotations:List[AttributeAnnotation] = Nil
  def is( anno:AttributeAnnotation ) = annotations ::= anno

  def annotated[ T <: AttributeAnnotation :Manifest ] = annotations.findOf[T]

	var isId    = false
	var isLabel = false

  private var localValidations:List[ ( Scope ) => Option[Invalid] ] = Nil

  override def validations =
    if ( domain != null ) {
      localValidations ++ domain.validations
    } else {
      if ( entity == null )
        println( "ERROR:  attribute has no entity" )
      else
        println( "ERROR:  attribute.name=" + name + " has no domain on entity " + entity.name )
      localValidations
    }
}


/*
 * * *   E n t i t i e s
 */

object Entity {

  private val index = mutable.Map[String,Entity]()

  def all = index.values

  def byTid( tid:String ) = ( tid.size >= 4 ) ? index.get( tid.take( 4 ) ) | None 

  def register( en:Entity ) = {
    if ( index.contains( en.tid ) )
      throw new RuntimeException( "Entity \"" + en.name + "\" has a duplicate TID of \"" + en.tid + "\"." )

    index( en.tid ) = en
  }

  def init {
    // 50 is arbitrary, we're checking to make sure that the class loader found all the relevant entities on boot via ClassUtil
    assert( index.values.size > 50 )

    for ( en <- index.values )
      en.checkInit
  }
}

trait Entity extends Domain with DbItem {
  type RecType >: Null <: Record

  def init {}

  lazy val checkInit = init


  override val isSimple = false

  val storageName:String
  val embedded:Boolean

	val sqlName = "invalid"

  def makeView:View

  def make:RecType

  def hasTid( atid:String ) = atid.startsWith( this.tid )


  /*
   * * *  Security
   */

  def canView( rec:Record, user:User ) = true


  /*
   * * *  Search
   */

  val searchIndex = "main"
  lazy val isSearchable = attribs.exists( _.search.text )

  def searchText = false
  def searchIndexable( rec:Record ) = true
  def searchViewableTo( rec:Record, user:User ) = true



	/*
	 * * *  Labels & Icons
	 */

  lazy val labelAtt = attribs.find( _.isLabel )

  lazy val label = name.camelCaseToSpaceUpper

  lazy val iconAtt = attribs.find( a => a.name == "icon" && !a.client ) orElse attribs.find( _.name == "thumbnail" )

  def defaultIcon = "/images/default_icon.png"

  def itemByTid( tid:String ) = TidItem.by( tid )
  def itemById( id:Any ) = TidItem.by( idToTid( id ) )
  


	/*
	 * * *  Attributes
	 */

	val attribs = new mutable.ArrayBuffer[Attribute]
  
  def has( name:String ) = attribs.exists( _.name == name )

	def attrib( name:String ) =
    try {
      attribs.find( _.name == name ).get
    } catch {
      case e:java.util.NoSuchElementException =>
        throw new MissingAttributeException( "Could not find attribute " + name + " in entity " + this.name )
    }

	def attByDbName( dbName:String ) = attribs.find( _.dbName == dbName ).get

	implicit def str2att( name: String ):Attribute =
    attribs.find( _.name == name ) match {
    case Some( a ) =>
      a
  
    case None =>
      val a = new Attribute( this, name )
      attribs += a
      a
    }

  def drop:Unit

  def create:Unit

	def recreate { drop; create }


	/*
	 * * *  IDs and TIDs
	 */

  lazy val idAtt:Option[Attribute] =
    attribs.find( _.isId ) match {
    case None =>
      if ( !embedded )
        problem( "missing ID attribute" )

      None

    case some =>
      some
    }

  /**
   * Tyranid ID.  This is a 3-byte identifier stored as a 4-character base64 string.  All Entity TIDs should be unique.
   */
  val tid:String

  def idToTid( id:Any ) = tid + idToRecordTid( id )
  def tidToId( tid:String ) = {

    val ( entityTid, recordTid ) = Tid.split( tid )

    assert( Entity.byTid( entityTid ).get == this )

    recordTidToId( recordTid )
  }

  override def idToRecordTid( id:Any )               = idAtt.flatten( _.domain.idToRecordTid( id ),        this.problem( "embedded entities don't have IDs" ) )
  override def recordTidToId( recordTid:String ):Any = idAtt.flatten( _.domain.recordTidToId( recordTid ), this.problem( "embedded entities don't have IDs" ) )

  
  /*
   * * *  Computations
   */

  lazy val computations       = attribs.filter( _.isComputed )


  /*
   * * *  Records
   */

  def records:Iterable[Record] = Nil
  
  def query( run:Run, offset:Int = 0, count:Int = 20, sort:Sort = null ):Iterable[Record]

  def getByRecordTid( recordTid:String ):Record      = throw new UnsupportedOperationException
  def byRecordTid( recordTid:String ):Option[Record] = throw new UnsupportedOperationException

  def save( r:Record ) {
    if ( r.searchIndex && isSearchable && searchIndexable( r ) )
      background { Es.index( r ) }
  }

  def delete( id:Any ) {
    
  }

  def delete( r:Record ) {
    //if ( isSearchable )
      // TODO:  Es.unindex( r )
  }


  /*
   * * *  Labels
   */

  def labelFor( id:Any ):String

  def idLabels:Iterable[(AnyRef,String)] = Nil


	/*
	 * * *  Static Data
   *
   * TODO:  move this to EnumEntity below ?
	 */

  def isStatic = staticView != null

	var staticView:TupleView = null
	var staticRecords:Buffer[Tuple] = null
	var staticIdIndex:mutable.HashMap[Long,Tuple] = null

  def makeTuple( view:TupleView ) = new Tuple( view )

  def static( block: ( StaticBuilder ) => Unit ) {
    try {
      val tl = StaticBuilder( this )
      block( tl )
      static( tl.tuples:_* )
    } catch {
      case e =>
        e.printStackTrace
        throw e
    }
  }

	def static( names:Product, tuples:Product* ) {
		val v = new TupleView
		val leafCount = names.productArity
		val vas = new Array[ViewAttribute]( leafCount )
		for ( li <- 0 until leafCount )
			vas( li ) = new ViewAttribute( v, attrib( names.productElement( li ).asInstanceOf[String] ), li )
		v.leaves = vas

		val tlen = tuples.size
    val newTuples = new Array[Tuple]( tlen )
		for ( ti <- 0 until tlen ) {
			val rt = tuples( ti )
			val t = makeTuple( v )
			val values = t.values
      val vlen = rt.productArity
			for ( vi <- 0 until vlen )
				values( vi ) = rt.productElement( vi ).asInstanceOf[AnyRef]
			newTuples( ti ) = t
		}

    static( newTuples:_* )
	}

	def static( tuples:Tuple* ) {
    val v = tuples( 0 ).view
		val vas = v.vas

    staticView = v

		val tlen = tuples.size
    // TODO:  use a toArray method or similar
		staticRecords = new ArrayBuffer[Tuple]( tlen )
		for ( ti <- 0 until tlen )
      staticRecords.insert( ti, tuples( ti ) )

		val keys = staticView.ekeys
		if ( keys.size == 1 ) {
			staticIdIndex = new mutable.HashMap[Long,Tuple]
			val idIdx = keys( 0 ).index

			for ( ti <- 0 until tlen ) {
				val t = staticRecords( ti )
				val id = t( idIdx ).asInstanceOf[Number].longValue
				staticIdIndex( id ) = t
			}
		}
	}

  def addStatic( tuple:Tuple ) = {
    if ( staticRecords == null ) {
      staticView = tuple.view
      staticRecords = Buffer()
    }

    staticRecords += tuple

		val keys = staticView.ekeys
		if ( keys.size == 1 ) {
      if ( staticIdIndex == null )
			  staticIdIndex = new mutable.HashMap[Long,Tuple]

			val idIdx = keys( 0 ).index

			val id = tuple( idIdx ).asInstanceOf[Number].longValue
			staticIdIndex( id ) = tuple
		}
  }

  val addNames:Seq[String] = Nil

  def add( values:Any* ):RecType = {

    require( addNames.size > 0,            "define addNames first before invoking add()" )
    require( addNames.size == values.size, "number of addNames must equal number of add() parameters" )

    if ( staticView == null ) {
		  staticView = new TupleView
		  val leafCount = addNames.size
		  val vas = new Array[ViewAttribute]( leafCount )
		  for ( li <- 0 until leafCount )
			  vas( li ) = new ViewAttribute( staticView, attrib( addNames( li ) ), li )
		  staticView.leaves = vas
    }

    if ( staticRecords == null )
      staticRecords = Buffer()

		val tuple = makeTuple( staticView )
		val tvalues = tuple.values
		for ( vi <- 0 until values.size )
			tvalues( vi ) = values( vi ).asInstanceOf[AnyRef]

    addStatic( tuple )
    tuple.as[RecType]
  }

	def staticLabelFor( id:Long ) =
    if ( id == 0 ) {
      ""
    } else {
		  staticIdIndex.get( id ) match {
      case Some( t ) =>
        val sb = new StringBuilder
        for ( lleaf <- t.view.elabels )
          sb ++= t( lleaf.index ).toString

        sb.toString

      case None =>
        "key " + id
      }
    }

  def problem( desc:String ) = throw new org.tyranid.db.ModelException( "On Entity '" + name + "':  " + desc )

  lazy val defaultSort:Sort = {
    val labels = attribs.filter( _.isLabel )

    labels.size match {
    case 0 =>
      Sort( "id", "ID", attribs.find( _.isId ).map( a => a.name -> -1 ).toSeq:_* )

    case 1 =>
      val a = labels( 0 )
      Sort( a.name, a.label, a.name -> 1 )

    case n =>
      Sort( "label", "Name", labels.map( a => a.name -> 1 ):_* )
    }
  }


  Entity.register( this )
}

case class StaticBuilder( en:Entity ) {

	val v = new TupleView
  val tuples = new mutable.ArrayBuffer[Tuple]()

  private var first = true

  def apply( values:Any* ) {
    if ( values == null ) return
    val vlen = values.length

    if ( first ) {
      val vas = new Array[ViewAttribute]( vlen )
      for ( li <- 0 until vlen )
        vas( li ) = new ViewAttribute( v, en.attrib( values( li ).asInstanceOf[String] ), li )
      v.leaves = vas
      first = false
    } else {
			val t = en.makeTuple( v )
			for ( vi <- 0 until vlen ) {
        val any = values( vi )
				t.values( vi ) =
          if ( any != null ) any.asInstanceOf[AnyRef]
          else               null
      }
      tuples += t
    }
  }
}

