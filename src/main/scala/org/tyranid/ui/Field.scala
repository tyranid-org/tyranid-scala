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

package org.tyranid.ui

import scala.xml.Text

import com.mongodb.DBObject

import scala.collection.mutable
import scala.xml.{ Node, NodeSeq, Unparsed }

import org.tyranid.Imp._
import org.tyranid.db.{ Record, Path, Scope, View, ViewAttribute }
import org.tyranid.db.mongo.Imp._
import org.tyranid.logic.Invalid
import org.tyranid.report.{ Report, Run }
import org.tyranid.web.WebContext


/*
 * * *   S h o w
 */

sealed trait Show

object Show {

  val values = Seq(
    Hidden,
    Readonly,
    Editable
  )

  case object Hidden   extends Show
  case object Readonly extends Show
  case object Editable extends Show
}


/*
 * * *   S e a r c h
 */

sealed trait Search {
  val name:String

  def makeSearchName( baseName:String ) = baseName + "$" + name

  def mongoSearch( run:Run, f:Field, searchObj:DBObject, value:Any ):Unit

  def matchesSearch( run:Run, f:PathField, value:Any, rec:Record ):Boolean
}

object Search {

  val values = Seq(
    Equals,
    Exists,
    Subst,
    Gte,
    Lte,
    Group,
    Custom
  )

  case object Equals extends Search {
    val name = "eq"

    def mongoSearch( run:Run, f:Field, searchObj:DBObject, value:Any ) = searchObj( f.baseName ) = f.transformValue( value )

    def matchesSearch( run:Run, f:PathField, searchValue:Any, rec:Record ) = f.basePath.get( rec ) == searchValue
  }

  case object Exists extends Search {
    val name = "exist"

    def mongoSearch( run:Run, f:Field, searchObj:DBObject, value:Any ) = searchObj( f.baseName ) = Mobj( $gt -> "" )

    def matchesSearch( run:Run, f:PathField, searchValue:Any, rec:Record ) = f.basePath.s( rec ).notBlank
  }

  case object Subst  extends Search {
    val name = "subst"

    def mongoSearch( run:Run, f:Field, searchObj:DBObject, value:Any ) =
      searchObj( f.baseName ) =
        if ( f.needsCaseInsensitiveSearch )
          value.toString.toPatternI
        else
          Mobj( $regex -> f.transformValue( value ) )

    def matchesSearch( run:Run, f:PathField, searchValue:Any, rec:Record ) = {
      val v = f.basePath.s( rec )

      if ( f.needsCaseInsensitiveSearch )
        v.safeString.toUpperCase.indexOf( searchValue.safeString.toUpperCase ) != -1
      else
        v.safeString.indexOf( searchValue.safeString ) != -1
    }
  }

  case object Gte    extends Search {
    val name = "gte"

    def mongoSearch( run:Run, f:Field, searchObj:DBObject, value:Any ) = searchObj( f.baseName ) = Mobj( $gte -> value )

    def matchesSearch( run:Run, f:PathField, searchValue:Any, rec:Record ) =
      f.basePath.leaf.domain.compare( searchValue, f.basePath.get( rec ) ) >= 0
  }

  case object Lte    extends Search {
    val name = "lte"

    def mongoSearch( run:Run, f:Field, searchObj:DBObject, value:Any ) = searchObj( f.baseName ) = Mobj( $lte -> value )

    def matchesSearch( run:Run, f:PathField, searchValue:Any, rec:Record ) =
      f.basePath.leaf.domain.compare( searchValue, f.basePath.get( rec ) ) <= 0
  }

  case object Group  extends Search {
    val name = "grp"

    def mongoSearch( run:Run, f:Field, searchObj:DBObject, value:Any ) = {
      val grouping = run.report.query.grouping
      val group = grouping.selectedGroup( run.report )
      if ( group != null ) {
        val fk = grouping.foreignKey
        searchObj( fk ) = Mobj( $in -> group.a_?( 'ids ) )
      }
    }

    def matchesSearch( run:Run, f:PathField, searchValue:Any, rec:Record ) =
      // TODO:  implement this
      false
  }

  case object Custom extends Search {
    val name = "cst"

    def mongoSearch( run:Run, f:Field, searchObj:DBObject, value:Any )     = {}
    def matchesSearch( run:Run, f:PathField, searchValue:Any, rec:Record ) = false
  }

  def by( name:String ) = values.find( _.name == name )

  def extract( name:String ):(String,Search) = {
    val idx = name.indexOf( "$" )

    if ( idx != -1 ) {
      val searchName = name.substring( idx + 1 )
      val search = by( searchName ) getOrElse { throw new RuntimeException( "Unknown search type: '" + searchName + "' in '" + name + "'" ) }
      
      ( name.substring( 0, idx ), search )
    } else {
      ( name, null )
    }
  }
}


/*
 * * *   F i e l d s
 */

trait Field {
  def baseName:String
  def name:String

  lazy val label = name.camelCaseToSpaceUpper

  def labelUi = <label for={ name }>{ label }</label>

  def section = "Standard"

  def header( run:Run ) = <th id={ name } class={ if ( run.report.selectedColumns( name ) ) "colh hi" else "colh" } style={ headerStyle }><div><span>{ headerCell }</span></div></th>
  def headerStyle = ""
  def headerCell:NodeSeq = Text( label )

  def cellClass:String = null
  def cell( s:Scope ):NodeSeq

  def effCell( s:Scope ) = cell( s )

  def ui( s:Scope ):NodeSeq   = throw new UnsupportedOperationException( "name=" + name )
  def extract( s:Scope ):Unit = throw new UnsupportedOperationException( "name=" + name )


  /*
   * * *   Data Hinting
   */

  // subclasses should convert to lowercase or uppercase as indicated by the domain
  def transformValue( value:Any ) = value

  def needsCaseInsensitiveSearch = false

  val show:Show
  val default:Option[ () => Any ]


  /*
   * * *   Search
   */

  val data:Boolean
  val search:Search

  def mongoSearch( run:Run, searchObj:DBObject, value:Any ) =
    if ( search != null )
      search.mongoSearch( run, this, searchObj, value )

  def matchesSearch( run:Run, value:Any, rec:Record ):Boolean
}


trait CustomField extends Field {

  val baseName = name

  val data = true
  val search:Search = null

  val show = Show.Editable
  val default = None

  def matchesSearch( run:Run, value:Any, rec:Record ) = search == null
}


object PathField {
  implicit def string2Field( name:String ) = PathField( name )
  implicit def symbol2Field( name:Symbol ) = PathField( name.name )

  def apply( path:Path ):PathField = PathField( path.name ).bind( path )
}

case class PathField( baseName:String,
                      l:String = null,
                      opts:Seq[(String,String)] = Nil,
                      sec:String = "Standard",
                      cellCls:String = null,
                      displayExists:Boolean = false,
                      data:Boolean = true,
                      search:Search = null,
                      span:Int = 1,
                      inputOnly:Boolean = false,
                      focus:Boolean = false,
                      filter:Option[ ( Record ) => Boolean ] = None,
                      uiStyle:UiStyle = UiStyle.Default,
                      labelc:Boolean = true,
                      create:Boolean = false,
                      show:Show = Show.Editable,
                      default:Option[ () => Any ] = None ) extends Field with UiObj {

  lazy val ( id, effOpts ) = {
    var _id:String = null

    val _opts = opts.flatMap {
      _ match {
      case ( "id" | "name", v )   =>
        if ( _id != null && v != _id )
          throw new RuntimeException( "Form element being named " + v + " and " + _id )

        _id = v
        None

      case p =>
        Some( p )
      }
    }

    if ( _id == null )
      _id = /* TODO: form id + '_' + */ va.name

    ( _id, _opts )
  }


  val name =
    /* [TAURUS-SIX]

       For convenience we allow a single Field to do double-duty as a search field and a display field in a report.
       In this case, the search fields' name does not have the extra $search stuff appended to it.  Obviously, this
       only works if there is only one data = true, if there are more than one search fields, all but one of them must
       have data = false.
     */
    if ( search != null && data == false ) search.makeSearchName( baseName )
    else                                   baseName

  override lazy val label = if ( l.notBlank ) l else path.leaf.label

  var path:Path = null
  def va = path.leaf

  lazy val basePath = va.view.path( baseName )

  def bind( path:Path ) = {
    this.path = path
    this
  }

  def bind( view:View ) = {
    path = view.path( name, sep = '.' )
    assert( path != null )
    this // TODO:  return an immutable version
  }

  override def ui( s:Scope ) = path.leaf.domain.ui( s, this )

  override def extract( pScope:Scope ) = {
    val scope = pScope.at( path )
    va.att.domain.extract( scope, this )
  }

  def fields = Seq( this )

  private def invalidLines( invalids:Seq[Invalid] ) =
    for ( invalid <- invalids )
      yield <span>{ invalid.message }</span>

  override def draw( pScope:Scope ) =
    if ( inputOnly ) {
      va.att.domain.ui( pScope.at( path ), this )
    } else {
      val scope = pScope.at( path )
      val invalids = va.invalids( scope )
      val invalid = !invalids.isEmpty
      val rec = scope.rec
      rec.invalids( va.index ) = !invalids.isEmpty
      
      va.att.domain.show( scope ) |*
      <div id={ va.name + "_c" } class={ "fieldc" + ( invalid |* " invalid" ) }>
       { if ( labelc ) 
         <div class="labelc">{ va.label( rec, opts:_* ) }{ va.att.required |* <span class="required">*</span> }{ va.att.help != NodeSeq.Empty |* <span class="attHelp">{ va.att.help }</span><span class="helpIcon"></span> }</div>
       }
       <div class={ "inputc" + va.att.domain.inputcClasses }>{ va.att.domain.ui( scope, this ) }</div>
       <div id={ va.name + "_e" } class="notec">{ !invalids.isEmpty |* invalidLines( invalids ) }</div>
      </div>
    }

  override def section = sec
  override def cellClass = cellCls

  def cell( scope:Scope ) = path.leaf.domain.cell( scope, this )

  override def effCell( pScope:Scope ) = {
    if ( displayExists ) {
      pScope.at( path ).rec.s( va.name ).notBlank |* Glyph.Checkmark
    } else {
      cell( pScope )
    }
  }

  override def transformValue( value:Any ) = path.leaf.domain.transformValue( value )
  override def needsCaseInsensitiveSearch  = path.leaf.domain.needsCaseInsensitiveSearch

  def matchesSearch( run:Run, value:Any, rec:Record ) = search == null || search.matchesSearch( run, this, value, rec )
}


case class CustomTextSearchField( baseName:String, l:String = null, opts:Seq[(String,String)] = Nil ) extends Field {

  val name = baseName + "$cst"

  val search = Search.Custom
  override val data = false

  val show = Show.Editable
  val default = None

  def cell( s:Scope ):NodeSeq = throw new UnsupportedOperationException

  override lazy val label = if ( l.notBlank ) l else name.camelCaseToSpaceUpper

  override def ui( s:Scope ) = Input( name, s.rec.s( name ), opts:_* )

  override def extract( s:Scope ) = {
    val v = T.web.req.s( name )

    if ( v.notBlank ) s.rec( name ) = v
    else              s.rec.remove( name )
  }

  def matchesSearch( run:Run, value:Any, rec:Record ) = true
}

