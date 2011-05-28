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

import net.liftweb.common.Full
import net.liftweb.http.SHtml
import net.liftweb.http.SHtml.ElemAttr

import org.tyranid.Imp._
import org.tyranid.logic.{ Valid, Invalid }
import org.tyranid.math.Base64
import org.tyranid.ui.Field


/*
 * * *   Domains
 */

trait Domain extends Valid {

	lazy val idType = IdType.ID_COMPLEX

	val sqlName:String

  def tid( r:Record, va:ViewAttribute ) = "invalid"

  /**
   * Is this field automatic populated by the underlying DBMS.
   */
	def isAuto = false

	def see( v:AnyRef ) =
		v match {
		case null => ""
		case v    => v.toString
		}
	
	def show( s:Scope ) = true

  def ui( s:Scope, f:Field, opts:(String,String)* ):NodeSeq =
    SHtml.ajaxText( s.rec s f.va.name, v => { s.rec( f.va.name ) = v; f.updateDisplayCmd( s ) }, opts.map( ElemAttr.pairToBasic ):_* )

  /**
   * These are the class(es) that should be added to the input container.
   */
  def inputcClasses = ""
}


/*
 * * *   Numbers
 */

abstract class DbIntish extends Domain {
	override lazy val idType = IdType.ID_32

  override def tid( r:Record, va:ViewAttribute ) = Base64.toString( r i va )
}

object DbInt extends DbIntish {
	val sqlName = "INT"
}

object DbIntSerial extends DbIntish {
	val sqlName = "SERIAL"
	override def isAuto = true
}


abstract class DbLongish extends Domain {
	override lazy val idType = IdType.ID_64

  override def tid( r:Record, va:ViewAttribute ) = Base64.toString( r l va )
}

object DbLong extends DbLongish {
	val sqlName = "BIGINT"
}

object DbLongSerial extends DbLongish {
	val sqlName = "BIGSERIAL"
	override def isAuto = true
}


object DbDouble extends Domain {
	val sqlName = "DOUBLE PRECISION"
}


/*
 * * *   Text
 */

object DbText extends Domain {
	val sqlName = "TEXT"
}

trait LimitedText extends Domain {
  val len:Int

  override def validations =
    ( ( scope:Scope ) => scope.s.filter( s => s.notBlank && s.length > len ).map( s => Invalid( scope, "Too long (max " + len + " " + "character".plural( len ) + ")." ) ) ) ::
    super.validations
}

case class DbChar( len:Int ) extends LimitedText {
	val sqlName = "CHAR(" + len + ")"
}

case class DbVarChar( len:Int ) extends LimitedText {
	val sqlName = "VARCHAR(" + len + ")"
}

object DbPassword extends DbVarChar( 64 ) {

  override def ui( s:Scope, f:Field, opts:(String,String)* ) =
    SHtml.ajaxText( s.rec s f.va.name, v => { s.rec( f.va.name ) = v; f.updateDisplayCmd( s ) }, ( opts ++ Seq( "type" -> "password" ) ).map( ElemAttr.pairToBasic ):_* )

  override def validations =
    ( ( scope:Scope ) => {
        // TODO:  Might be cleaner to add a DbPasswordMatch domain and then change this to look for DbPassword and DbPasswordMatch rather than use fixed names like "password" and "password2"
        ( scope.va.get.name == "password2" &&
          scope.rec.s( "password2" ).notBlank &&
          scope.rec( "password" ) != scope.rec( "password2" ) ) |*
          Some( Invalid( scope, "Passwords do not match." ) )
      } ) ::
    ( ( scope:Scope ) => scope.s.filter( s => scope.saving && s.notBlank && s.length < 7 ).map( s => Invalid( scope, "Password too short (7 characters minimum)." ) ) ) ::
    super.validations
}

object DbUrl extends DbVarChar( 256 )

object DbEmail extends DbVarChar( 128 ) {

  override val validations =
    ( ( scope:Scope ) => scope.s.filter( s => scope.saving && s.notBlank && !s.isEmail ).map( s => Invalid( scope, "Invalid email address." ) ) ) ::
    super.validations

}

object DbPhone extends DbChar( 10 )


/*
 * * *   Binary
 */



/*
 * * *   Booleans
 */

object DbBoolean extends Domain {
	val sqlName = "CHAR(1)"
}


/*
 * * *   Times
 */

object DbDate extends Domain {
	val sqlName = "DATE"
}

object DbDateTime extends Domain {
	val sqlName = "TIMESTAMP WITHOUT TIME ZONE"
}


/*
 * * *   Linking & Embedding
 */

case class DbArray( of:Domain ) extends Domain {
	val sqlName = "invalid"

  /*

      1. 

  override def ui( s:Scope, f:Field, opts:(String,String)* ) =



   */
}

case class DbLink( toEn:Entity ) extends Domain {
	lazy val sqlName = toEn.idType match {
		                case IdType.ID_32      => "INT"
		                case IdType.ID_64      => "BIGINT"
		                case IdType.ID_COMPLEX => throw new ModelException( toEn.name + " has a complex ID and cannot be linked to." )
										}

  override def ui( s:Scope, f:Field, opts:(String,String)* ) =
    SHtml.ajaxSelect( toEn.idLabels.map( v => ( v._1.toString, v._2 ) ),
                      Full( s.rec s f.va ), v => { s.rec( f.va ) = v; f.updateDisplayCmd( s ) },
                      opts.map( ElemAttr.pairToBasic ):_* )

  override def inputcClasses = " select"

	override def see( v:AnyRef ) =
		v match {
		case null => ""
		case n:Number => toEn.labelFor( n.longValue )
		}
}



