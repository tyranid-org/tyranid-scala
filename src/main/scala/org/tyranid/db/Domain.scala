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
import org.tyranid.db.meta.Tid
import org.tyranid.logic.{ Valid, Invalid }
import org.tyranid.math.Base64
import org.tyranid.time.Time
import org.tyranid.ui.{ Checkbox, Glyph, Input, PathField, Search, Select, TextArea, ToggleLink, UiStyle, Valuable }
import org.tyranid.web.WebContext


/*
 * * *   Domains
 */

trait Domain extends Valid {

  val isSimple = true

  val isLink   = false // DbLink, DbTid
  def hasLinks:Boolean = isLink || ( this.isInstanceOf[DbArray] && this.as[DbArray].of.hasLinks )

  lazy val name = getClass.getSimpleName.replaceAll( "^Db", "" ).replace( "$", "" ).uncapitalize

	val sqlName:String

  def idToRecordTid( v:Any )                = "invalid-" + getClass.getSimpleName
  def recordTidToId( recordTid:String ):Any = throw new UnsupportedOperationException

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

  def compare( v1:Any, v2:Any ) = v1.safeString.compareTo( v2.safeString )
    

  /*
   * This method modifies the data to ensure that it corresponds to restrictions set up by the domain.
   * Examples are translating to upper/lowercase by DbTextlike.
   */
  def transformValue( value:Any ) = value

  // TODO:  this could probably be named more generically
  def needsCaseInsensitiveSearch = false

  def get( s:Scope, f:PathField ) =
    s.get( f ) match {
    case v:Valuable => v.get
    case v          => v
    }
    
  def set( s:Scope, f:PathField, v:Any ) =
    s.get( f ) match {
    case c:Valuable => c.set( v )
    case _          => s.set( f, v )
    }

  def remove( s:Scope, f:PathField ) =
    s.get( f ) match {
    case c:Valuable => c.set( null )
    case _          => s.remove( f )
    }



  /*
   * * *   F o r m s
   */

  protected def commonUi( s:Scope, f:PathField, normal: => NodeSeq ) =
    f.search match {
    case Search.Exists =>
      Checkbox( f.id, get( s, f )._b ) ++ f.labelUi

    case _  =>
      normal
    }

  protected def commonExtract( s:Scope, f:PathField ) =
    f.search match {
    case Search.Exists =>
      val v = T.web.b( f.id )

      if ( v ) set( s, f, true )
      else     remove( s, f )
      true
    case _ =>
      false
    }

  def ui( s:Scope, f:PathField ):NodeSeq =
    commonUi( s, f, {
      val input = Input( f.id, s.s( f ), f.effOpts:_*  )

      if ( f.focus )
        throw new RuntimeException( "TODO:  handle focus on load" )
      else 
        input
    } )

  def fromString( s:String ):Any = s.trim
    
  def extract( s:Scope, f:PathField ) =
    if ( !commonExtract( s, f ) ) {
      val v = T.web.s( f.id )

      if ( v.notBlank ) set( s, f, fromString( v ) )
      else              remove( s, f )
    }

  /**
   * These are the class(es) that should be added to the input container.
   */
  def inputcClasses = ""


  /*
   * * *   G r i d
   */

  def cell( s:Scope, f:PathField ):NodeSeq = Text( get( s, f )._s )
}


/*
 * * *   Numbers
 */

abstract class DbIntish extends Domain {
  override def idToRecordTid( v:Any )                = if ( v != null ) Base64.toString( v._i ) else null
  override def recordTidToId( recordTid:String ):Any = Base64.toInt( recordTid )

  override def fromString( s:String ) = s.trim.toInt

  override def compare( v1:Any, v2:Any ) = v1._i - v2._i
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
  override def idToRecordTid( v:Any )                = if ( v != null ) Base64.toString( v._l ) else null
  override def recordTidToId( recordTid:String ):Any = Base64.toLong( recordTid )

  override def fromString( s:String ) = s.trim.toLong
  override def compare( v1:Any, v2:Any ) = ( v1._l - v2._l ).toInt
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
    val ta = TextArea( f.id, get( s, f )._s, f.effOpts:_*  )
    
    if ( f.focus )
      throw new RuntimeException( "TODO:  handle focus on load" )
    else 
      ta
	}
	
  override def inputcClasses = "large"

  override def cell( s:Scope, f:PathField ) = Unparsed( get( s, f )._s.replace( "\n", "<br/>" ) )
}

trait LimitedText extends DbTextLike {
  val len:Int

	lazy val sqlName = "CHAR(" + len + ")"

  override def validations =
    ( ( scope:Scope ) => scope.s.filter( s => s.notBlank && s.length > len ).map( s => Invalid( scope, "Too long (max " + len + " " + "character".plural( len ) + ")." ) ) ) ::
    super.validations
}

case class DbChar( len:Int ) extends LimitedText

case class DbVarChar( len:Int ) extends LimitedText {
	override lazy val sqlName = "VARCHAR(" + len + ")"
}

/**
 * A string that must be lower case.
 */
case class DbLowerChar( len:Int ) extends LimitedText {
  override val lowercase = true
}

/**
 * A string that must be lower case.
 */
case class DbUpperChar( len:Int ) extends LimitedText {
  override val uppercase = true
}

object DbPassword extends DbVarChar( 80 ) {

  override def ui( s:Scope, f:PathField ) = {
    // Only specify the type of password if it is not specified   
    val input = Input( f.id, get( s, f )._s, ( if ( !f.opts.exists( _._1 == "type" ) ) ( f.effOpts :+ ( "type" -> "password" ) ) else f.effOpts ):_* )
    
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
  
  override def extract( s:Scope, f:PathField ) {
    val existing = s.s( f )
    
    if ( !commonExtract( s, f ) ) {
      val v = fromString( T.web.s( f.id ) )._s

      if ( v.notBlank ) {
        import org.mindrot.jbcrypt.BCrypt
        val salt = T.session.cache.getOrElseUpdate( "pw.salt", BCrypt.gensalt() )._s
        val hashed = BCrypt.hashpw( v, salt )
        
        // Only set the value if it has changed.
        if ( existing.isBlank || !BCrypt.checkpw( v, existing ) )
          set( s, f, hashed )
      } else {
        remove( s, f )
      }
    }
  }  
}

object DbUrl extends DbVarChar( 256 ) {

  override def cell( s:Scope, f:PathField ) = {
    val base = get( s, f )._s

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
    case UiStyle.Toggle => ToggleLink( f.id, get( s, f )._b, f.effOpts:_* )
    case _              => Checkbox( f.id, get( s, f )._b, f.effOpts:_* ) ++ f.labelUi
    }
    
  override def fromString( s:String ):Any = s.trim.toLaxBoolean

  override def inputcClasses = " boolean"

  override def cell( s:Scope, f:PathField ) = get( s, f )._b |* Glyph.Checkmark
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
    val input = Input( f.id, get( s, f )._t.toDateStr, f.effOpts:_*  )

    if ( f.focus )
      throw new RuntimeException( "TODO:  handle focus on load" )
    else 
      input
  }

  override def fromString( s:String ):Any = s.trim.toLaxUserDate

  override def cell( s:Scope, f:PathField ):NodeSeq = {
    val date = get( s, f )._t
    Text( if ( date != null ) date.toDateStr else "" )
  }
}

object DbDate extends DbDateLike

object DbDateTime extends DbDateLike {
	override val sqlName = "TIMESTAMP WITHOUT TIME ZONE"

  override def dateOnly = false

  override def ui( s:Scope, f:PathField ) = {
    val input = Input( f.id, get( s, f )._t.toDateTimeStr, f.effOpts:_*  )

    if ( f.focus )
      throw new RuntimeException( "TODO:  handle focus on load" )
    else 
      input
  }

  override def fromString( s:String ):Any = s.trim.toLaxUserDateTime

  override def cell( s:Scope, f:PathField ):NodeSeq = {
    val date = get( s, f )._t
    Unparsed( "<nobr>" + ( if ( date != null ) date.toDateTimeStr else "" ) + "</nobr>" )
  }
}


/*
 * * *   Linking & Embedding
 */

case class DbArray( of:Domain ) extends Domain {
  override val isSimple = false

	val sqlName = "invalid"

  /*
  override def ui( s:Scope, f:PathField, f.effOpts:(String,String)* ) =

  draw a field for each array element, and add an "Add" button
   */

  
  override def cell( s:Scope, f:PathField ) = {
    if ( f.path.tail.isInstanceOf[ArrayIndex] ) {
      of.cell( s, f )
    } else {
      val arr = s.a_?( f )

      import scala.collection.JavaConversions._
      Unparsed( arr.map( v => of.see( v ) ).sorted.mkString( ",<br/>" ) )
    }
  }


      /*

  lazy val sells:String = obj.a_?( 'sellingCategories ).flatMap( id => Industry.byId( id._i ) ).map( _ s 'name ).distinct.sorted.mkString( ",<br/>" )

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

case class DbTid( of:Entity* ) extends LimitedText {
  val len = 32

  override val isLink = true

	override def see( v:Any ) = {

    if ( v == null ) {
      ""
    } else {
      val ( entity, id ) = Tid.parse( v.toString )

      id match {
      case null => ""
      case n    => entity.labelFor( n )
      }
    }
  }
}

case class DbLink( toEntity:Entity ) extends Domain {
  require( toEntity != null )

  override val isLink = true

	lazy val sqlName = toEntity.idAtt.flatten( _.domain.sqlName, toEntity.problem( "embedded entities don't have IDs" ) )

  override def idToRecordTid( v:Any )                = toEntity.idAtt.flatten( _.domain.idToRecordTid( v ),         toEntity.problem( "embedded entities don't have IDs" ) )
  override def recordTidToId( recordTid:String ):Any = toEntity.idAtt.flatten( _.domain.recordTidToId( recordTid ), toEntity.problem( "embedded entities don't have IDs" ) )

  override def ui( s:Scope, f:PathField ) = {
    if ( f.scopeOpts( s ).find( t => t._1 == "readonly" ).get != None ) {
      Text( see( get( s, f ) ) )
    } else {
      
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
      
      Select( f.id, get( s, f )._s,
              ( "" -> "-Please Select-" ) +: values,
              f.scopeOpts( s ):_* )
    }
  }

  override def fromString( s:String ) = toEntity.idAtt.flatten( _.domain.fromString( s ), toEntity.problem( "embedded entities don't have IDs" ) )

  override def inputcClasses = " select"

	override def see( v:Any ) =
		v match {
		case null => ""
    case s:String if toEntity.idAtt.flatten( !_.domain.isInstanceOf[DbTextLike], toEntity.problem( "embedded entities don't have IDs." ) ) => s
		case n    => toEntity.labelFor( n )
		}

  override def cell( s:Scope, f:PathField ) = Text( see( get( s, f ) ) )
}



