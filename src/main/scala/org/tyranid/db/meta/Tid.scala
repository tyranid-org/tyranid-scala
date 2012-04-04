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

package org.tyranid.db.meta

import scala.collection.mutable
import scala.xml.{ NodeSeq, Text, Unparsed }

import org.bson.types.ObjectId
import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.db.{ Domain, DbArray, DbLink, DbTid, Entity, MultiPath, PathNode, Record }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ MongoEntity, MongoRecord }
import org.tyranid.math.Base64
import org.tyranid.ui.{ Tab, TabBar }
import org.tyranid.web.{ Weblet, WebContext }


object Tid {

  def split( tid:String ) = tid.splitAt( 4 )

  def parse( tid:String ):(Entity,Any) =
    if ( tid.isBlank ) {
      ( null, null )
    } else {
      val ( entityTid, recordTid ) = split( tid )

      val en = Entity.byTid( entityTid ).get

      if ( recordTid.isBlank )
        ( en, null )
      else
        ( en, en.recordTidToId( recordTid ) )
    }

  def toIds( tids:Seq[String], entity:Entity ) =
    tids.filter( _.startsWith( entity.tid ) ).map( tid => new ObjectId( Base64.toBytes( tid.substring( 4 ) ) ) )

  def refs( tid:String, in:Entity = null ):Seq[Record] = {
    val ( entityTid, recordTid ) = Tid.split( tid )

    val refEn = Entity.byTid( entityTid ).get
    val refId = refEn.recordTidToId( recordTid )

    val matchers = mutable.ArrayBuffer[DBObject]()
    val matches = mutable.ArrayBuffer[Record]()

    def enter( path:List[PathNode], dom:Domain ) {

      dom match {
      case link:DbLink =>
        if ( link.toEntity == refEn ) {
          val p = MultiPath( path:_* )

          matchers += Mobj( p.name -> refId )
        }

      case link:DbTid =>
        if ( link.of.contains( refEn ) ) {
          val p = MultiPath( path:_* )

          matchers += Mobj( p.name -> tid )
        }

      case array:DbArray =>
        enter( path, array.of )

      case en:Entity =>
        val v = en.makeView
        for ( va <- v.vas )
          enter( va :: path, va.domain )

      case _ =>
      }
    }

    def entity( en:Entity ) {
      matchers.clear
      enter( Nil, en )

      val query =
        matchers.size match {
        case 0 => null
        case 1 => matchers( 0 )
        case n => Mobj( $or -> Mlist( matchers:_* ) )
        }

      if ( query != null ) {
        en match {
        case me:MongoEntity =>
          for ( m <- me.db.find( query ) )
            matches += me( m )

        case _ =>
          // not supported yet
        }
      }

      if ( en.is[Versioning] ) {
        // TODO:
        // log( 'user ) = Session().user.id
        //if ( diffs.as.nonEmpty )
          //log( 'removals ) = PathValue.toDbObject( diffs.as )
        
        //if ( diffs.bs.nonEmpty )
          //log( 'adds ) = PathValue.toDbObject( diffs.bs )
        
        //if ( diffs.diffs.nonEmpty )
          //log( 'updates ) = PathDiff.toDbObject( diffs.diffs )
      }
    }

    if ( in != null ) {
      entity( in )
    } else {
      for ( en <- Entity.all )
        entity( en )
    }

    matches  
  }

  /*
   * * *  Delete references

        +.  delete from the entity where the tid is based

            remove from entities where it is foreign ...
            
            (a)  array ... remove from array
            (b)  value ... remove property

        ?.  dangling references after a delete ?

            what if we remove it from the array and now the array is empty, and the record is basically dangling ?

        ?.  what about cascading deletes ?

            this should only come into play when we add in ownership


   * * *  Ownership


   * * *  Versioning

       +.  simple links in version (currently just User)

       +.  complex links in differences

   */
}

object Tidlet extends Weblet {

  def tidLink( tid:String, label:String = null ) =
    if ( tid.endsWith( "not-available" ) )
      Text( tid )
    else
      <a href={ wpath + "/field?tid=" + tid }>{ if ( label.notBlank ) label else  tid }</a>

  def handle( web:WebContext ) = {
    val t = T

    if ( !t.user.isGod )
      _404

    rpath match {
    case "/" | "/field" | "/json" | "/ref" =>
      shell( ui( web.req.s( 'tid ) ) )

    case _ =>
      _404
    }
  }

  val entityTabBar =
    TabBar( this,
      Tab( "/attrib", Text( "Attributes" ), default = true ),
      Tab( "/record", Text( "Records" ) )
    )

  val recordTabBar =
    TabBar( this,
      Tab( "/field", Text( "Fields" ), default = true ),
      Tab( "/json",  Text( "JSON" ) ),
      Tab( "/ref",   Text( "Refs" ) )
    )

  def ui( tid:String ) = {

    val ( entity, id ) = Tid.parse( tid )
    val rec = id != null |* Record.byTid( tid )

    <div class="plainbox">
     <div class="content">
      <form method="post" action={ wpath } style="margin-top:8px;">
       <div style="padding:4px;">
        <label for="tid" style="float:left; width:40px; font-size:16px; line-height:28px; color:#888;">TID</label>
        <input type="text" id="tid" name="tid" value={ tid } style="font-size:20px;"/>
        <input type="submit" class="greenBtn" value="Analyze" style="font-size:16px;"/>
       </div>
      </form>
     </div>
    </div> ++
    { if ( rec.isDefined ) {
        val r = rec.get

        <div class="fieldHeader">
         <label>Type</label><span>Record</span>
         <label style="margin-left:16px;">Label</label><span>{ r.label.summarize() }</span>
         <label style="margin-left:16px;">Entity</label><span><a href={ wpath + "/field?tid=" + entity.tid }>{ entity.name }</a></span>
         <label style="margin-left:16px;">Storage</label><span>{ r.view.entity.storageName }</span>
        </div> ++
        { recordTabBar.draw( qs = "?tid=" + tid ) } ++
        { recordTabBar.choice match {
          case "/field" => fields( r )
          case "/json"  => json( r )
          case "/ref"   => refs( tid )
          }
        }
      } else if ( entity != null ) {
        <div class="fieldHeader">
         <label>Type</label><span>Entity</span>
         <label style="margin-left:16px;">Label</label><span>{ entity.label }</span>
         <label style="margin-left:16px;">Entity</label><span>{ entity.name }</span>
         <label style="margin-left:16px;">Storage</label><span>{ entity.storageName }</span>
        </div> ++
        { entityTabBar.draw( qs = "?tid=" + tid ) } ++
        { entityTabBar.choice match {
          case "/attrib" => attribs( entity )
          case "/record" => records( entity )
          }
        }
      } else {
         tid.notBlank |* Unparsed( """<span style="color:red;">Invalid TID.</span>""" )
      }
    }
  }

  def fields( rec:Record ) = {

    def ui( rec:Record ) = {
      val pathValues = rec.flatten.sorted

      var odd = true

      def divStyle = "padding:2px 0;" + ( odd |* " background:#f8f8f8;" )

      for ( pv <- pathValues ) yield {
        val path = pv.path
        val va = path.leaf
        val id = va.name

        odd = !odd

        val v = path.get( rec )

        val domain =
          va.domain match {
          case array:DbArray => array.of
          case dom           => dom
          }

        domain match {
        case link:DbLink =>
          if ( link.toEntity == null )
            throw new RuntimeException( va.view.entity.name + "." + va.name + " is null!" )

          val recTid = link.idToRecordTid( v )

          recTid != null |* {
            val tid = link.toEntity.tid + recTid

            <div style={ divStyle }>
             <label for={ id } style="float:left;">{ path.label }</label>
             <span id={ id } style="display:block; margin-left:400px;">{ link.see( v ) } - { tidLink( tid ) }</span>
            </div>
          }

        case tlink:DbTid =>

          v != null |* {
            <div style={ divStyle }>
             <label for={ id } style="float:left;">{ path.label }</label>
             <span id={ id } style="display:block; margin-left:400px;">{ tlink.see( v ) } - { tidLink( v.toString ) }</span>
            </div>
          }

        case d =>
          val vStr = v.safeString

          vStr.notBlank |*
          <div style={ divStyle }>
           <label for={ id } style="float:left;">{ path.label }</label>
           <span id={ id } style="display:block; margin-left:400px;">{ vStr }</span>
          </div>
        }
      }
    }

    <div class="plainbox">
     <div class="title">Fields</div>
     <div class="content">{ ui( rec ) }</div>
    </div>
  }

  def json( rec:Record ) = {
    rec match {
    case mr:MongoRecord =>
      <div style="padding:2px 0;">
       { mr.obj.toString }
      </div>

    case _ =>
      <div style="padding:2px 0;">Not a MongoDB object.</div>
    }
  }

  def refs( tid:String ) =
    <table class="dtable">
     <tr><th>Entity</th><th>Label</th><th>TID</th></tr>{
       val matches = Tid.refs( tid )

       matches.map { m =>
         <tr>
          <td>{ m.view.entity.name }</td><td><b>{ m.label }</b></td><td><i>{ tidLink( m.tid ) }</i></td>
         </tr>
       }
    }</table>

  def attribs( en:Entity ) = {

    def describe( d:Domain ):String = {
      d match {
      case    en:Entity  => tidLink( en.tid, en.label ).toString
      case array:DbArray => "array(" + describe( array.of ) + ")"
      case  link:DbLink  => "link(" + describe( link.toEntity ) + ")"
      case  tids:DbTid   => "tid(" + tids.of.map( describe ).mkString( "," ) + ")"
      case     d         => d.name
      }
    }

    <table class="dtable">
     <tr>
      <th>Name</th>
      <th>Label</th>
      <th>Domain</th>
      <th>Help</th>
     </tr>
     { for ( a <- en.attribs ) yield
       <tr>
        <td>{ a.name }</td>
        <td>{ a.label }</td>
        <td>{ Unparsed( describe( a.domain ) ) }</td>
        <td>{ a.help }</td>
       </tr>
     }
    </table>
  }

  def records( en:Entity ) =
    <table class="dtable"/>
}


