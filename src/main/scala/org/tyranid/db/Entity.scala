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

import scala.collection.mutable.{ ArrayBuffer, HashMap }
import scala.xml.NodeSeq

import org.tyranid.Imp.string
import org.tyranid.db.tuple.{ TupleView, Tuple }
import org.tyranid.logic.{ Invalid, Valid }


/*
 * * *   A t t r i b u t e s
 */

class Attribute( val entity:Entity, val name:String ) extends DbItem with Valid {
	var domain:Domain = null
  var label:String = name.camelCaseToSpaceUpper
  var help:NodeSeq = NodeSeq.Empty
  var required:Boolean = false

  /**
   * Indicates whether this va is persisted in the database or is just used as a temporary form field.
   */
  var temporary:Boolean = false

  /**
   *    DSL ... TODO:  move this out to a builder object
   */

	def is( domain:Domain ) = { this.domain = domain; this }
  def as( label:String ) = { this.label = label; this }
  def is( str:String ) = {
    str match {
    case "key"       => isKey = true
    case "label"     => isLabelAtt = true
    case "required"  => required = true; localValidations ::= ( ( scope:Scope ) => scope.required )
    case "temporary" => temporary = true
    }

    this
  }
  def help( ns:NodeSeq ):Attribute = { help = ns; this }

	var isKey = false
	var isLabelAtt = false

  private var localValidations:List[ ( Scope ) => Option[Invalid] ] = Nil

  override def validations = localValidations ++ domain.validations
}


/*
 * * *   E n t i t i e s
 */

trait Entity extends Domain with DbItem {

	val sqlName = "invalid"

	/*
	 * * *  Attributes
	 */

	val attribs = new ArrayBuffer[Attribute]

	def attrib( name:String ) =
    try {
      attribs.find( _.name == name ).get
    } catch {
      case e:java.util.NoSuchElementException =>
        throw new ModelException( "Could not find attribute " + name + " in entity " + this.name )
    }

	def attByDbName( dbName:String ) = attribs.find( _.dbName == dbName ).get

	val name = getClass.getSimpleName.replace( "$", "" )


	override lazy val idType =
		attribs.filter( _.isKey ) match {
		case as if as.size == 1 => as( 0 ).domain.idType
		case _                  => IdType.ID_COMPLEX
		}

	implicit def str2att( name: String ) = {
		val a = new Attribute( this, name )
		attribs += a
		a
	}

  def drop:Unit

  def create:Unit

	def recreate { drop; create }


	def labelFor( id:Long ) = {
		val t = staticIdIndex( id )

		val sb = new StringBuilder
		for ( lleaf <- t.view.elabels )
			sb ++= t( lleaf.index ).toString

		sb.toString
	}


	/*
	 * * *  Static Data
	 */

	var staticView:TupleView = null
	var staticRecords:Array[Tuple] = null
	var staticIdIndex:HashMap[Long,Tuple] = null

	def static( names:Product, tuples:Product* ) {
		val v = new TupleView
		val leafCount = names.productArity
		val vas = new Array[ViewAttribute]( leafCount )
		for ( li <- 0 until leafCount )
			vas( li ) = new ViewAttribute( v, attrib( names.productElement( li ).asInstanceOf[String] ), li )

		v.leaves = vas
		staticView = v

		val tlen = tuples.size
		staticRecords = new Array[Tuple]( tlen )
		for ( ti <- 0 until tlen ) {
			val rt = tuples( ti )
			val t = new Tuple( v )
			val values = t.values
			for ( li <- 0 until leafCount )
				values( li ) = rt.productElement( li ).asInstanceOf[AnyRef]
			staticRecords( ti ) = t
		}

		val keys = staticView.ekeys
		if ( keys.size == 1 ) {
			staticIdIndex = new HashMap[Long,Tuple]
			val idIdx = keys( 0 ).index

			for ( ti <- 0 until tlen ) {
				val t = staticRecords( ti )
				val id = t( idIdx ).asInstanceOf[Number].longValue
				staticIdIndex( id ) = t
			}
		}
	}
}

