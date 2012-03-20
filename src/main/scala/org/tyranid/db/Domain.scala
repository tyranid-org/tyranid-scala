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

import scala.xml.{ NodeSeq, Text, Unparsed }

import org.tyranid.Imp._
import org.tyranid.logic.{ Valid, Invalid }
import org.tyranid.math.Base64
import org.tyranid.report.PathField
import org.tyranid.time.{ Time }
import org.tyranid.ui.{ Field, Glyph }


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

	def see( v:Any ) =
		v match {
		case null => ""
		case v    => v.toString
		}
	
	def show( s:Scope ) = true

  def ui( s:Scope, f:Field, opts:(String,String)* ):NodeSeq = {
    val input = Field.text( s, f, opts:_* )

      /*
        if ( s.rec( f.va.name ) != v ) {
          s.rec( f.va.name ) = v; f.updateDisplayCmd( s ) 
        }
      }, opts.map( ElemAttr.pairToBasic ):_* )
      */

    if ( f.focus )
      throw new RuntimeException( "TODO:  handle focus on load" )
    else 
      input
  }
    
  def extract( s:Scope, f:Field ) {
    s.rec( f.va.name ) = T.web.req.s( f.id )
  }
    
  /**
   * These are the class(es) that should be added to the input container.
   */
  def inputcClasses = ""

  def cell( pf:PathField, r:Record ):NodeSeq = Text( pf.path s r )
}


/*
 * * *   Numbers
 */

abstract class DbIntish extends Domain {
	override lazy val idType = IdType.ID_32

  override def tid( r:Record, va:ViewAttribute ) = Base64.toString( r i va )

  override def extract( s:Scope, f:Field ) {
    s.rec( f.va.name ) = T.web.req.i( f.id )
  }
    
}

object DbInt extends DbIntish {
	val sqlName = "INT"
}

// TODO:  more tightly link to org.tyranid.db.meta.AutoIncrement ?
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
	
  override def ui( s:Scope, f:Field, opts:(String,String)* ):NodeSeq = {
    val ta = Field.textArea( s, f, s.rec.s( f.va.name ), opts:_* )
    
    if ( f.focus )
      throw new RuntimeException( "TODO:  handle focus on load" )
    else 
      ta
	}
	
  override def inputcClasses = "large"
}

case object DbParagraph extends LimitedText {
  val len = 8192
	val sqlName = "CHAR(" + len + ")"
	
  override def ui( s:Scope, f:Field, opts:(String,String)* ):NodeSeq = {
    val ta = Field.textArea( s, f, s.rec.s( f.va.name ), opts:_* )
    
    if ( f.focus )
      throw new RuntimeException( "TODO:  handle focus on load" )
    else 
      ta
	}
	
  override def inputcClasses = "large"

  override def cell( pf:PathField, r:Record ):NodeSeq = Unparsed( pf.path.s( r ).replace( "\n", "<br/>" ) )
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
    super.ui( s, f, ( opts ++ Seq( "type" -> "password" ) ):_* )

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

object DbUrl extends DbVarChar( 256 ) {

  override def cell( pf:PathField, r:Record ):NodeSeq = {
    val base = pf.path.s( r )

    try {
      <a href={ base.toUrl.toString }>{ base }</a>
    } catch {
    case e:Exception =>
      Text( base )
    }
  }
}

object DbEmail extends DbVarChar( 128 ) {

  override val validations =
    ( ( scope:Scope ) => scope.s.filter( s => scope.saving && s.notBlank && !s.isEmail ).map( s => Invalid( scope, "Invalid email address." ) ) ) ::
    super.validations

}

object DbPhone extends DbChar( 14 ) {
  override def inputcClasses = " phone"
}


/*
 * * *   Binary
 */



/*
 * * *   Booleans
 */

object DbBoolean extends Domain {
	val sqlName = "CHAR(1)"
	  
  override def ui( s:Scope, f:Field, opts:(String,String)* ) =
    if ( f.uiStyle == Field.UI_STYLE_TOGGLE )
      Field.toggleLink( s, f, s.rec.b( f.va.name ), opts:_* )
    else
      Field.checkbox( s, f, s.rec.b( f.va.name ), opts:_* )
    
  override def extract( s:Scope, f:Field ) {
    s.rec( f.va.name ) = T.web.req.b( f.id )
  }

  override def inputcClasses = " boolean"

  override def cell( pf:PathField, r:Record ) = pf.path.b( r ) |* Glyph.Checkmark
}


/*
 * * *   Times
 */

trait DbDateLike extends Domain {
	val sqlName = "DATE"

  def dateOnly = true
	
  override def inputcClasses = " date"      
  
  override val validations =
    { var msg:String = ""
      ( scope:Scope ) =>
      scope.value.filter {
        _ match {
        case s:String =>
          try {
            scope.saving && s.notBlank && !s.isDate( dateOnly = dateOnly )
          } catch {
          case e:java.text.ParseException =>
            msg = e.getMessage
            true
          }
        case d:Date   => false
        case null     => false
        }
      }.map( s => Invalid( scope, msg ) )
    } ::
    super.validations

  override def ui( s:Scope, f:Field, opts:(String,String)* ) = {
    val input = Field.input( s, f, s.rec.t( f.va.name ).toDateStr, opts:_* )
    if ( f.focus )
      throw new RuntimeException( "TODO:  handle focus on load" )
    else 
      input
  }

  override def extract( s:Scope, f:Field ) {
    s.rec( f.va.name ) = T.web.req.s( f.id ).toLaxDate
  }

  override def cell( pf:PathField, r:Record ):NodeSeq = {
    val date = pf.path.t( r )
    Text( if ( date != null ) date.toDateStr else "" )
  }
}

object DbDate extends DbDateLike

object DbDateTime extends DbDateLike {
	override val sqlName = "TIMESTAMP WITHOUT TIME ZONE"

  override def dateOnly = false

  override def ui( s:Scope, f:Field, opts:(String,String)* ) = {
    val input = Field.input( s, f, s.rec.t( f.va.name ).toDateTimeStr, opts:_* )
    if ( f.focus )
      throw new RuntimeException( "TODO:  handle focus on load" )
    else 
      input
  }

  override def extract( s:Scope, f:Field ) {
    s.rec( f.va.name ) = T.web.req.s( f.id ).toLaxDateTime
  }

  override def cell( pf:PathField, r:Record ):NodeSeq = {
    val date = pf.path.t( r )
    Unparsed( "<nobr>" + ( if ( date != null ) date.toDateTimeStr else "" ) + "</nobr>" )
  }
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

  override def ui( s:Scope, f:Field, opts:(String,String)* ) = {
    
    /*
     * a.  model-based linking
     * 
     *     see if there is anything else on the form that links to something in toEntity
     * 
     *     for example:  if toEntity == region, and region has a country -> Country,
     *                   and there is a country -> Country in f.form, restrict the search to that
     * 
     * b.  Field.filter: ( rec:Record ) => Boolean
     * 
     * 
     * 
     * 
     */

    val idLabels = f.filter match {
    case Some( filter ) =>
      toEntity.records.filter( filter ).map( _.idLabel )
    case None =>
      toEntity.idLabels
    }
      
    val values = idLabels.map( v => ( v._1.toString, v._2 ) ).toSeq
    
    Field.select(
      s, f, s.rec s f.va,
      ( "" -> "-Please Select-" ) +: values,
      opts:_* )
  }

  override def extract( s:Scope, f:Field ) {
    val v = T.web.req.s( f.id )

    if ( v.isBlank )
      s.rec( f.va ) = null
    else
      toEntity.idType match {
      case IdType.ID_32 => s.rec( f.va ) = v.toLaxInt
      case IdType.ID_64 => s.rec( f.va ) = v.toLaxLong
      case _            => s.rec( f.va ) = v
      }
  }

  override def inputcClasses = " select"

	override def see( v:Any ) =
		v match {
		case null => ""
		case n:Number => toEntity.labelFor( n )
		}
}



