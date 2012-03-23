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


sealed trait Search {
  val name:String
  def search( run:Run, f:Field, searchObj:DBObject, value:Any ):Unit

  def makeSearchName( baseName:String ) = baseName + "$" + name
}

object Search {

  val values = Seq(
    Equals,
    Exists,
    Subst,
    Gte,
    Custom
  )

  case object Equals extends Search {
    val name = "eq"

    def search( run:Run, f:Field, searchObj:DBObject, value:Any ) =
      searchObj( f.baseName ) = f.transformValue( value )
  }

  case object Exists extends Search {
    val name = "exist"

    def search( run:Run, f:Field, searchObj:DBObject, value:Any ) = searchObj( f.baseName ) = Mobj( $gt -> "" )
  }

  case object Subst  extends Search {
    val name = "subst"

    def search( run:Run, f:Field, searchObj:DBObject, value:Any ) =
      searchObj( f.baseName ) =
        if ( f.needsCaseInsensitiveSearch )
          value.toString.toPatternI
        else
          Mobj( $regex -> f.transformValue( value ) )
  }

  case object Gte    extends Search {
    val name = "gte"

    def search( run:Run, f:Field, searchObj:DBObject, value:Any ) = searchObj( f.baseName ) = Mobj( $gte -> value )
  }

  case object Custom extends Search {
    val name = "cst"

    // nothing to do, this is handled in Query subclasses' prepareSearch()
    def search( run:Run, f:Field, searchObj:DBObject, value:Any ) = {}
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
  def cell( run:Run, rec:Record ):NodeSeq

  def effCell( run:Run, rec:Record ) = cell( run, rec )

  def ui( s:Scope ):NodeSeq   = throw new UnsupportedOperationException( "name=" + name )
  def extract( s:Scope ):Unit = throw new UnsupportedOperationException( "name=" + name )


  /*
   * * *   Data Hinting
   */

  // subclasses should convert to lowercase or uppercase as indicated by the domain
  def transformValue( value:Any ) = value

  def needsCaseInsensitiveSearch = false


  /*
   * * *   Search
   */

  def textSearchKeys = Seq( name )

  def textSearch( run:Run ):Seq[DBObject] = {
    val searchValue = run.report.textSearchValue

    // TODO:  handle lookup codes

    // TODO:  multivalue / calculated fields

    if ( searchValue.notBlank )
      textSearchKeys.map( name => Mobj( name -> searchValue.toPatternI ) )
    else
      Nil
  }


  val data:Boolean
  val search:Search

  def prepareSearch( run:Run, searchObj:DBObject, value:Any ) =
    if ( search != null )
      search.search( run, this, searchObj, value )
}


trait CustomField extends Field {

  val baseName = name

  val data = true
  val search:Search = null
}


object PathField {
  implicit def string2Field( name:String ) = PathField( name )
  implicit def symbol2Field( name:Symbol ) = PathField( name.name )
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
                      uiStyle:UiStyle = UiStyle.Default ) extends Field with UiObj {
  var id:String = null

  val name =
    if ( search != null ) search.makeSearchName( baseName )
    else                  baseName

  override lazy val label = if ( l.notBlank ) l else path.leaf.label

  var path:Path = null
  def va = path.leaf

  def bind( view:View ) = {
    path = view.path( name, sep = '.' )
    this // TODO:  return an immutable version
  }

  override def ui( s:Scope )      = path.leaf.domain.ui( s, this )
  override def extract( pScope:Scope ) = {
    val scope = pScope.at( path )
    va.att.domain.extract( scope, this )
  }

  def fields = Seq( this )

  def optsMapper( s:Scope ) = { 
    var id = this.id

    val opts2 = opts.flatMap {
      _ match {
      case ( "id" | "name", v )   =>
        if ( id != null && v != id )
          throw new RuntimeException( "Form element being named " + v + " and " + id )

        id = v
        None

      case p =>
        Some( p )
      }
    }

    if ( id == null ) {
      id = /* TODO: form id + '_' + */ va.name
      this.id = id
    } else if ( id != this.id ) {
      this.id = id
    }

    opts2
  }

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
       <div class="labelc">{ va.label( rec, opts:_* ) }{ va.att.required |* <span class="required">*</span> }</div>
       <div class={ "inputc" + va.att.domain.inputcClasses }>{ va.att.domain.ui( scope, this ) }</div>
       <div id={ va.name + "_e" } class="notec">{ !invalids.isEmpty |* invalidLines( invalids ) }</div>
      </div>
    }

  override def section = sec
  override def cellClass = cellCls

  def cell( run:Run, r:Record ) = path.leaf.domain.cell( this, r )

  override def effCell( run:Run, rec:Record ) = {
    if ( displayExists )
      path.s( rec ).notBlank |* Glyph.Checkmark
    else
      cell( run, rec )
  }

  override def transformValue( value:Any ) = path.leaf.domain.transformValue( value )
  override def needsCaseInsensitiveSearch  = path.leaf.domain.needsCaseInsensitiveSearch
}


case class CustomTextSearchField( baseName:String, l:String = null, opts:Seq[(String,String)] = Nil ) extends Field {

  val name = baseName + "$cst"

  val search = Search.Custom
  override val data = false

  def cell( run:Run, rec:Record ):NodeSeq = throw new UnsupportedOperationException

  override lazy val label = if ( l.notBlank ) l else name.camelCaseToSpaceUpper

  override def ui( s:Scope ) = Input( name, s.rec.s( name ), opts:_* )

  override def extract( s:Scope ) = {
    val v = T.web.req.s( name )

    if ( v.notBlank ) s.rec( name ) = v
    else              s.rec.remove( name )
  }
}

