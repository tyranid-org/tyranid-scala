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

import scala.xml.NodeSeq

import net.liftweb.common.Full
import net.liftweb.http.SHtml
import net.liftweb.http.SHtml.ElemAttr

import org.tyranid.Imp._
import org.tyranid.logic.{ Valid, Invalid }
import org.tyranid.math.Base64
import org.tyranid.time.{ Time }
import org.tyranid.ui.Field

import net.liftweb.http.js.JsCmds.{FocusOnLoad}

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

  def isSet( v:Any ) = v != null

	def see( v:AnyRef ) =
		v match {
		case null => ""
		case v    => v.toString
		}
	
	def show( s:Scope ) = true

  def ui( s:Scope, f:Field, opts:(String,String)* ):NodeSeq = {
      var input =
        SHtml.ajaxText( s.rec s f.va.name, v => { 
          if ( s.rec( f.va.name ) != v ) {
            s.rec( f.va.name ) = v; f.updateDisplayCmd( s ) 
          } }, opts.map( ElemAttr.pairToBasic ):_* )
        
    
      if ( f.focus )
        FocusOnLoad( input )
      else 
        input
    }
    


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

  override def ui( s:Scope, f:Field, opts:(String,String)* ):NodeSeq =
    SHtml.ajaxText( s.rec s f.va.name, v => { 
      if ( s.rec( f.va.name ) != v ) {
        s.rec( f.va.name ) = v.toLaxInt; f.updateDisplayCmd( s ) 
      } }, opts.map( ElemAttr.pairToBasic ):_* )
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

trait DbTextLike extends Domain {

  override def isSet( v:Any ) =
    v match {
    case s:String => s.notBlank
    case _ => false
    }
}

object DbText extends DbTextLike {
	val sqlName = "TEXT"
}

trait LimitedText extends DbTextLike {
  val len:Int

  override def validations =
    ( ( scope:Scope ) => scope.s.filter( s => s.notBlank && s.length > len ).map( s => Invalid( scope, "Too long (max " + len + " " + "character".plural( len ) + ")." ) ) ) ::
    super.validations
}

case class DbChar( len:Int ) extends LimitedText {
	val sqlName = "CHAR(" + len + ")"
}

case class DbLargeChar( len:Int ) extends LimitedText {
	val sqlName = "CHAR(" + len + ")"
	
  override def ui( s:Scope, f:Field, opts:(String,String)* ):NodeSeq =
    SHtml.ajaxTextarea( s.rec s f.va.name, v => { 
      if ( s.rec( f.va.name ) != v ) {
        s.rec( f.va.name ) = v; f.updateDisplayCmd( s ) 
      } }, opts.map( ElemAttr.pairToBasic ):_* )
}

case class DbVarChar( len:Int ) extends LimitedText {
	val sqlName = "VARCHAR(" + len + ")"
}

/**
 * A string that must be lower case.
 */
case class DbLowerChar( len:Int ) extends LimitedText {
	val sqlName = "CHAR(" + len + ")"
}

object DbPassword extends DbVarChar( 64 ) {

  override def ui( s:Scope, f:Field, opts:(String,String)* ) =
    SHtml.ajaxText( s.rec s f.va.name, v => { 
      if ( s.rec( f.va.name ) != v ) {
        s.rec( f.va.name ) = v; f.updateDisplayCmd( s ) 
      } }, ( opts ++ Seq( "type" -> "password" ) ).map( ElemAttr.pairToBasic ):_* )

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
	  
  override def ui( s:Scope, f:Field, opts:(String,String)* ):NodeSeq =
    SHtml.ajaxCheckbox( ( s.rec s f.va.name ) == "Y", (v:Boolean) => { if ( v ) s.rec( f.va.name ) = "Y" else s.rec( f.va.name ) = "N"; f.updateDisplayCmd( s ) }, opts.map( ElemAttr.pairToBasic ):_* )
    
  override def inputcClasses = " checkbox"
}


/*
 * * *   Times
 */

object DbDate extends Domain {
	val sqlName = "DATE"
	
  override def inputcClasses = " date"      
  
  override val validations =
    { ( scope:Scope ) =>
      scope.value.filter {
        _ match {
        case s:String => scope.saving && s.notBlank && !s.isDate
        case d:Date   => false
        }
      }.map( s => { spam( "INVALID DATE[" + s + "]" ); Invalid( scope, "Invalid date (format: mm/dd/yyyy)" ) } )
    } ::
    super.validations

  override def ui( s:Scope, f:Field, opts:(String,String)* ):NodeSeq =
    SHtml.ajaxText(
      Time.toDateStr( s.rec t f.va.name ),
      v => { 
        if ( s.rec( f.va.name ) != v ) {
          s.rec( f.va.name ) = v.toLaxDate; f.updateDisplayCmd( s ) 
        }
      },
      opts.map( ElemAttr.pairToBasic ):_* )
}

object DbDateTime extends Domain {
	val sqlName = "TIMESTAMP WITHOUT TIME ZONE"

  override def ui( s:Scope, f:Field, opts:(String,String)* ):NodeSeq =
    SHtml.ajaxText(
      Time.toDateTimeStr( s.rec t f.va.name ),
      v => { 
        if ( s.rec( f.va.name ) != v ) {
          s.rec( f.va.name ) = v.toLaxDateTime; f.updateDisplayCmd( s ) 
        }
      },
      opts.map( ElemAttr.pairToBasic ):_* )
}


/*
 * * *   Linking & Embedding
 */

case class DbArray( of:Domain ) extends Domain {
	val sqlName = "invalid"

  /*

      1. 

  override def ui( s:Scope, f:Field, opts:(String,String)* ) =


  draw a field for each array element, and add an "Add" button


   */
}

case class DbLink( toEntity:Entity ) extends Domain {
	lazy val sqlName = toEntity.idType match {
		                case IdType.ID_32      => "INT"
		                case IdType.ID_64      => "BIGINT"
		                case IdType.ID_COMPLEX => throw new ModelException( toEntity.name + " has a complex ID and cannot be linked to." )
										}

  override def ui( s:Scope, f:Field, opts:(String,String)* ) =
    SHtml.ajaxSelect( ( "" -> "-Please Select-" ) +: toEntity.idLabels.map( v => ( v._1.toString, v._2 ) ).toSeq,
                      Full( s.rec s f.va ),
                      v => {
                        toEntity.idType match {
                        case IdType.ID_32 => s.rec( f.va ) = v.toLaxInt
                        case IdType.ID_64 => s.rec( f.va ) = v.toLaxLong
                        case _            => s.rec( f.va ) = v
                        }
                        f.updateDisplayCmd( s )
                      },
                      opts.map( ElemAttr.pairToBasic ):_* )

  override def inputcClasses = " select"

	override def see( v:AnyRef ) =
		v match {
		case null => ""
		case n:Number => toEntity.labelFor( n )
		}
}



