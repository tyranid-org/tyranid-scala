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

import scala.xml.{ NodeSeq, Text, Unparsed }

import org.tyranid.Imp._
import org.tyranid.logic.{ Valid, Invalid }
import org.tyranid.math.Base64
import org.tyranid.time.{ Time }
import org.tyranid.ui.{ Checkbox, Glyph, Input, PathField, Search, Select, TextArea, ToggleLink, UiStyle }
import org.tyranid.web.WebContext


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

  /*
   * This method modifies the data to ensure that it corresponds to restrictions set up by the domain.
   * Examples are translating to upper/lowercase by DbTextlike.
   */
  def transformValue( value:Any ) = value

  // TODO:  this could probably be named more generically
  def needsCaseInsensitiveSearch = false


  /*
   * * *   F o r m s
   */

  protected def commonUi( s:Scope, f:PathField, normal: => NodeSeq ) =
    f.search match {
    case Search.Exists =>
      Checkbox( f.id, s.rec.b( f.va.name ) ) ++ f.labelUi

    case _  =>
      normal
    }

  protected def commonExtract( s:Scope, f:PathField ) =
    f.search match {
    case Search.Exists =>
      val v = T.web.req.b( f.id )

      if ( v ) s.rec( f.va.name ) = true
      else     s.rec.remove( f.va.name )
      true
    case _ =>
      false
    }

  def ui( s:Scope, f:PathField ):NodeSeq =
    commonUi( s, f, {
      val input = Input( f.id, s.rec s f.va.name, f.effOpts:_*  )

      if ( f.focus )
        throw new RuntimeException( "TODO:  handle focus on load" )
      else 
        input
    } )
    
  def extract( s:Scope, f:PathField ) =
    if ( !commonExtract( s, f ) ) {
      val v = T.web.req.s( f.id )

      if ( v.notBlank ) s.rec( f.va.name ) = v.trim
      else              s.rec.remove( f.va.name )
    }

  /**
   * These are the class(es) that should be added to the input container.
   */
  def inputcClasses = ""


  /*
   * * *   G r i d
   */

  def cell( s:Scope, f:PathField ):NodeSeq = Text( s.rec s f.va.name )
}


/*
 * * *   Numbers
 */

abstract class DbIntish extends Domain {
	override lazy val idType = IdType.ID_32

  override def tid( r:Record, va:ViewAttribute ) = Base64.toString( r i va )

  override def extract( s:Scope, f:PathField ) =
    if ( !commonExtract( s, f ) ) {
      val str = T.web.req.s( f.id )

      if ( str.isBlank ) s.rec.remove( f.va.name )
      else               s.rec( f.va.name ) = str.coerceInt
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

  val uppercase = false
  val lowercase = false

  override def isSet( v:Any ) =
    v match {
    case s:String => s.notBlank
    case _ => false
    }

  override def transformValue( value:Any ) =
    if ( uppercase )
      value.as[String].toUpperCase
    else if ( lowercase )
      value.as[String].toLowerCase
    else
      value

  override def needsCaseInsensitiveSearch = !uppercase && !lowercase
}

object DbText extends DbTextLike {
	val sqlName = "TEXT"
	
  override def ui( s:Scope, f:PathField ):NodeSeq = {
    val ta = TextArea( f.id, s.rec.s( f.va.name ), f.effOpts:_*  )
    
    if ( f.focus )
      throw new RuntimeException( "TODO:  handle focus on load" )
    else 
      ta
	}
	
  override def inputcClasses = "large"

  override def cell( s:Scope, f:PathField ) = Unparsed( s.rec.s( f.va.name ).replace( "\n", "<br/>" ) )
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

case class DbVarChar( len:Int ) extends LimitedText {
	val sqlName = "VARCHAR(" + len + ")"
}

/**
 * A string that must be lower case.
 */
case class DbLowerChar( len:Int ) extends LimitedText {
  override val lowercase = true

	val sqlName = "CHAR(" + len + ")"
}

/**
 * A string that must be lower case.
 */
case class DbUpperChar( len:Int ) extends LimitedText {
  override val uppercase = true

	val sqlName = "CHAR(" + len + ")"
}

object DbPassword extends DbVarChar( 64 ) {

  override def ui( s:Scope, f:PathField ) = {
    val input = Input( f.id, s.rec.s( f.va.name ), ( f.effOpts :+ ( "type" -> "password" ) ):_* )

    if ( f.focus )
      throw new RuntimeException( "TODO:  handle focus on load" )
    else 
      input
  }

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

  override def cell( s:Scope, f:PathField ) = {
    val base = s.rec s f.va.name

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
	  
  override def ui( s:Scope, f:PathField ) =
    f.uiStyle match {
    case UiStyle.Toggle => ToggleLink( f.id, s.rec.b( f.va.name ), f.effOpts:_* )
    case _              => Checkbox( f.id, s.rec b f.va.name, f.effOpts:_* ) ++ f.labelUi
    }
    
  override def extract( s:Scope, f:PathField ) =
    if ( T.web.req.b( f.id ) ) s.rec( f.va.name ) = true
    else                       s.rec.remove( f.va.name )

  override def inputcClasses = " boolean"

  override def cell( s:Scope, f:PathField ) = s.rec.b( f.va.name ) |* Glyph.Checkmark
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

  override def ui( s:Scope, f:PathField ) = {
    val input = Input( f.id, s.rec.t( f.va.name ).toDateStr, f.effOpts:_*  )

    if ( f.focus )
      throw new RuntimeException( "TODO:  handle focus on load" )
    else 
      input
  }

  override def extract( s:Scope, f:PathField ) {
    s.rec( f.va.name ) = T.web.req.s( f.id ).toLaxDate
  }

  override def cell( s:Scope, f:PathField ):NodeSeq = {
    val date = s.rec t f.va.name
    Text( if ( date != null ) date.toDateStr else "" )
  }
}

object DbDate extends DbDateLike

object DbDateTime extends DbDateLike {
	override val sqlName = "TIMESTAMP WITHOUT TIME ZONE"

  override def dateOnly = false

  override def ui( s:Scope, f:PathField ) = {
    val input = Input( f.id, s.rec.t( f.va.name ).toDateTimeStr, f.effOpts:_*  )

    if ( f.focus )
      throw new RuntimeException( "TODO:  handle focus on load" )
    else 
      input
  }

  override def extract( s:Scope, f:PathField ) {
    s.rec( f.va.name ) = T.web.req.s( f.id ).toLaxDateTime
  }

  override def cell( s:Scope, f:PathField ):NodeSeq = {
    val date = s.rec t f.va.name
    Unparsed( "<nobr>" + ( if ( date != null ) date.toDateTimeStr else "" ) + "</nobr>" )
  }
}


/*
 * * *   Linking & Embedding
 */

case class DbArray( of:Domain ) extends Domain {
	val sqlName = "invalid"

  /*
  override def ui( s:Scope, f:PathField, f.effOpts:(String,String)* ) =

  draw a field for each array element, and add an "Add" button
   */

  
  override def cell( s:Scope, f:PathField ) = {
    val arr = s.rec a_? f.va.name

    import scala.collection.JavaConversions._
    Unparsed( arr.map( v => of.see( v ) ).sorted.mkString( ",<br/>" ) )
  }


      /*

  lazy val sells:String = obj.a_?( 'sellingCategories ).flatMap( id => Industry.byId( id.coerceInt ) ).map( _ s 'name ).distinct.sorted.mkString( ",<br/>" )

         issues

         - too many categories to show in a dropdown

         - we can't do a search below because a) Search.Custom is an object -and- b) Search is a sealed trait

           + it's ugly to nest it in here anyway, we'd probably rather it be an overridden method on CustomField

         ? should this be a PathField instead ?  "sells" is a DbArray property on Org.  we could add a:  def ui on DbArray that can deal with searching ?

           ? would we need a Search.In ?


        def search( run:Run, f:Field, searchObj:DBObject, value:Any ) = {

          
          searchObj( f.baseName ) = Mobj( $gt -> "" )
        }
      }
      */
}

case class DbLink( toEntity:Entity ) extends Domain {
	lazy val sqlName = toEntity.idType match {
		                case IdType.ID_32      => "INT"
		                case IdType.ID_64      => "BIGINT"
		                case IdType.ID_COMPLEX => throw new ModelException( toEntity.name + " has a complex ID and cannot be linked to." )
										}

  override def ui( s:Scope, f:PathField ) = {
    
    /*
     * a.  model-based linking
     * 
     *     see if there is anything else on the form that links to something in toEntity
     * 
     *     for example:  if toEntity == region, and region has a country -> Country,
     *                   and there is a country -> Country in f.form, restrict the search to that
     * 
     * b.  PathField.filter: ( rec:Record ) => Boolean
     * 
     */

    val idLabels = f.filter.flatten(
      filter => toEntity.records.filter( filter ).map( _.idLabel ),
      toEntity.idLabels )
      
    val values = idLabels.map( v => ( v._1.toString, v._2 ) ).toSeq
    
    Select( f.id, s.rec s f.va,
            ( "" -> "-Please Select-" ) +: values,
            f.effOpts:_* )
  }

  override def extract( s:Scope, f:PathField ) {
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

  override def cell( s:Scope, f:PathField ) = Text( see( s.rec( f.va.name ) ) )
}



