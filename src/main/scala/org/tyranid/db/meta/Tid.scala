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
import scala.xml.{ Text, Unparsed }

import org.bson.types.ObjectId
import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.db.{ Domain, DbArray, DbLink, DbTid, Entity, MultiPath, PathNode, Record }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ MongoEntity, MongoRecord }
import org.tyranid.math.Base64
import org.tyranid.web.{ Weblet, WebContext }


object Tid {

  def split( tid:String ) = tid.splitAt( 4 )

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
spam( "me #1, query=" + query )
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

  def tidLink( tid:String ) =
    if ( tid.endsWith( "not-available" ) )
      Text( tid )
    else
      <a href={ wpath + "?tid=" + tid }>{ tid }</a>

  def handle( web:WebContext ) = {
    val t = T

    if ( !t.user.isGod )
      _404

    rpath match {
    case "/" =>
      val tid = web.req.s( 'tid )

      shell(
        <div class="plainbox">
         <div class="title">TID</div>
         <div class="content">
          <form method="post" action={ wpath }>
           <div style="padding:4px;">
            <label for="tid" style="float:left; width:70px;">TID</label>
            <input type="text" id="tid" name="tid" value={ tid }/>
            <input type="submit" class="greenBtn" value="Analyze"/>
           </div>
          </form>
         </div>
        </div> ++
        { tid.notBlank |* {
        <div class="plainbox">
         <div class="title">Information</div>
         <div class="content">{
          Record.byTid( tid ) match {
          case Some( rec ) =>
            <table style="border-collapse:separate; border-spacing:4px;">
             <tr><td><b>Entity</b></td><td>{ rec.view.entity.name }</td></tr>
             <tr><td><b>Label</b></td><td>{ rec.label }</td></tr>
             { rec match {
               case mr:MongoRecord =>
                 <tr><td><b>JSON</b></td><td>{ mr.obj.toString }</td></tr>
               case _ =>
             } }
            </table>

           case None =>
             Unparsed( """<span style="color:red;">Invalid TID.</span>""" )
           }
         }</div>
        </div>
        <div class="plainbox">
         <div class="title">References</div>
         <div class="content">
          <table class="dtable">
           <tr><th>Entity</th><th>Label</th><th>TID</th></tr>{
           val matches = Tid.refs( tid )

           matches.map { m =>
             <tr>
              <td>{ m.view.entity.name }</td><td><b>{ m.label }</b></td><td><i>{ tidLink( m.tid ) }</i></td>
             </tr>
           }
          }</table>
         </div>
        </div> } }
      )

    case _ =>
      _404
    }
  }
}


