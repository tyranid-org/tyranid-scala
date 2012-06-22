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
import org.tyranid.collection.ConcurrentExpireAutoMap
import org.tyranid.db.{ Domain, DbArray, DbLink, DbTid, Entity, ArrayIndex, MultiPath, PathNode, PathValue, Record, Scope }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ MongoEntity, MongoRecord }
import org.tyranid.db.ram.RamEntity
import org.tyranid.math.Base64
import org.tyranid.report.AutoQuery
import org.tyranid.ui.{ PathField, Tab, TabBar }
import org.tyranid.time.Time
import org.tyranid.web.{ Weblet, WebContext }


object TidItem {

  def unknown( tid:String ) = {
    val ( entityTid, recordTid ) = tid.splitAt( 4 )

    Entity.byTid( entityTid ) match {
    case Some( e ) => TidItem( tid = tid, id = e.tidToId( tid ), org = null, name = "Unknown " + e.label, thumbnail = e.defaultIcon )
    case None      => TidItem( tid = tid, id = null, org = null, name = "Unknown", thumbnail = B.Org.defaultIcon )
    }
  }

  def by( tid:String ) = itemFor( tid )


  private lazy val itemFor = new ConcurrentExpireAutoMap( Time.HalfHourMs, ( tid:String ) => {
    Record.byTid( tid ) match {
    case Some( rec ) => TidItem( tid = tid, id = rec.id, org = B.Org.orgIdFor( rec ), name = rec.label, thumbnail = rec.icon )
    case None        => log( Event.RefInt, "m" -> ( "ERROR:  could not locate tid " + tid ), "ex" -> new RuntimeException() )
                        unknown( tid )
    }
  } )
}

case class TidItem( tid:String, id:Any, org:ObjectId, name:String, thumbnail:String )


object TidCache {
}

class TidCache {

  val byTid = new mutable.HashMap[String,Record]()

  def size  = byTid.size
  def clear = byTid.clear
  def has( tid:String ) = byTid.contains( tid )
}


case class DeleteResults( ramReferences:Seq[Record], cascadeFailures:Seq[Record], updates:Seq[Record], deletes:Seq[Record] ) {

  def success = ramReferences.isEmpty && cascadeFailures.isEmpty
}

object Tid {
  def eye( tid:String ) = T.isEye |* <a href={ "/admin/tid?tid=" + tid } class="eyeBtn">T</a>

  def isRecordTid( tid:String ) = tid.length > 4

  def split( tid:String ) = tid.splitAt( 4 )

  def tidToId( tid:String ) = {

    val ( entityTid, recordTid ) = Tid.split( tid )

    Entity.byTid( entityTid ).get.recordTidToId( recordTid )
  }

  def parse( tid:String ):(Entity,Any) =
    if ( tid.isBlank ) {
      ( null, null )
    } else {
      val ( entityTid, recordTid ) = split( tid )

      Entity.byTid( entityTid ) match {
      case Some( en ) =>
        if ( recordTid.isBlank )
          ( en, null )
        else
          ( en, en.recordTidToId( recordTid ) )

      case _ =>
        ( null, null )
      }
    }

  def toIds( tids:Seq[String], entity:Entity ) =
    tids.filter( _.startsWith( entity.tid ) ).map( tid => new ObjectId( Base64.toBytes( tid.substring( 4 ) ) ) )

  def references( tid:String, in:Entity = null ):Seq[Record] = {
    val ( refEn, refId ) = Tid.parse( tid )

    val matchers = mutable.ArrayBuffer[DBObject]()
    val matches = mutable.ArrayBuffer[Record]()

    def enter( path:List[PathNode], dom:Domain ) {

      dom match {
      case link:DbLink =>
        if ( link.toEntity == refEn ) {
          val p = MultiPath( path.reverse:_* )

          matchers += Mobj( p.name -> refId )
        }

      case link:DbTid =>
        if ( link.of.contains( refEn ) ) {
          val p = MultiPath( path.reverse:_* )

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
      for ( en <- Entity.all if !en.embedded )
        entity( en )
    }

    matches  
  }

  /**
   * Returns true if the record is now dangling (i.e. an owned property was removed).
   */
  def extractTid( rec:Record, refTid:String ):Boolean = {
    val ( refEn, refId ) = Tid.parse( refTid )
    var dangling = false

    val recEn = rec.view.entity

    def enter( path:List[PathNode] ):Boolean = {
      val p = MultiPath( path.reverse:_* )

      val domain =
        if ( path != Nil ) p.leafDomain
        else               recEn

      domain match {
      case link:DbLink =>
        if ( link.toEntity == refEn && p.get( rec ) == refId )
          return true

      case link:DbTid =>
        if ( link.of.contains( refEn ) && p.get( rec ) == refTid )
          return true

      case array:DbArray =>
        val a = p.a_?( rec )

        if ( a.size > 0 ) {
          var i = 0
          while ( i < a.size )
            if ( enter( ArrayIndex( i ) :: path ) )
              a.remove( i )
            else
              i += 1

          if ( a.size == 0 && p.leaf.att.owner )
            dangling = true
        }

      case en:Entity =>
        for ( va <- en.makeView.vas )
          if ( enter( va :: path ) ) {
            val r = if ( p != Nil ) p.get( rec ).as[DBObject] else rec.as[DBObject]
            r.remove( va.name )

            if ( va.att.owner )
              dangling = true
          }

      case _ =>
      }

      false
    }

    enter( Nil )

    if ( recEn.is[Versioning] ) {
      // TODO:
      // log( 'user ) = Session().user.id
      //if ( diffs.as.nonEmpty )
        //log( 'removals ) = PathValue.toDbObject( diffs.as )
        
      //if ( diffs.bs.nonEmpty )
        //log( 'adds ) = PathValue.toDbObject( diffs.bs )
        
      //if ( diffs.diffs.nonEmpty )
        //log( 'updates ) = PathDiff.toDbObject( diffs.diffs )
    }

    dangling
  }
  
  // WARNING!  This will perform a blind cascade delete
  def deleteCascade( tid:String, deletions:mutable.Buffer[Record] = null ) {
    val delStat = delete( tid, true )
    val cFailures = delStat.cascadeFailures
    
    if ( cFailures.nonEmpty ) {
      cFailures.foreach( f => deleteCascade( f.tid ) )
      deleteCascade( tid )
    } else {
      if ( deletions != null )
        deletions ++= delStat.deletes
    }
  }
  
  def delete( tid:String, performDeletion:Boolean ) = {

    val refs = references( tid )

    val ramReferences   = mutable.ArrayBuffer[Record]()
    val cascadeFailures = mutable.ArrayBuffer[Record]()
    val updates         = mutable.ArrayBuffer[Record]()
    val deletes         = mutable.ArrayBuffer[Record]()

    for ( ref <- refs ) {
      if ( ref.view.entity.isInstanceOf[RamEntity] ) {
        ramReferences += ref
      } else if ( extractTid( ref, tid ) ) {
        val refRefs = references( ref.tid ).filter( rr => !refs.exists( _.tid == rr.tid ) && rr.tid != tid )

        if ( refRefs.nonEmpty )
          cascadeFailures ++= refRefs
        else
          deletes += ref
      } else {
        updates += ref
      }
    }

    val rec = Record.byTid( tid ).get
    if ( rec.view.entity.isInstanceOf[RamEntity] )
      ramReferences += rec
    else
      deletes += rec

    val results = DeleteResults( ramReferences = ramReferences, cascadeFailures = cascadeFailures, updates = updates, deletes = deletes )

    if ( results.success && performDeletion ) {
      updates foreach { _.save }
      deletes foreach { _.delete }
    }

    results
  }
}

object Tidlet extends Weblet {

  def tidLink( tid:String, label:String = null ) =
    if ( tid.endsWith( "not-available" ) )
      Text( tid )
    else
      <a href={ wpath + "/field?tid=" + tid }>{ if ( label.notBlank ) label else tid }</a>

  def handle( web:WebContext ) = {
    val t = T

    if ( !t.user.isGod )
      _404

    var tid = web.req.s( 'tid )
    if ( tid.isBlank )
      tid = t.session.cache.s( 'lastTid )
    else
      t.session.cache( 'lastTid ) = tid

    rpath match {
    case "/delete" =>
      shell( delete( tid ) )

    case _ =>
      if ( "/" == rpath || entityTabBar.has( rpath ) || recordTabBar.has( rpath ) )
        shell( ui( tid ) )
      else
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
      Tab( "/field",   Text( "Fields" ), default = true ),
      Tab( "/json",    Text( "JSON" ) ),
      Tab( "/ref",     Text( "Refs" ) ),
      Tab( "/version", Text( "Versions" ) )
    )

  def ui( tid:String ) = {

    val ( entity, id ) = Tid.parse( tid )
    val rec = id != null |* Record.byTid( tid )

    <div class="plainbox">
     <div class="content">
      <form method="post" action={ wpath } style="margin-top:8px;">
       <div style="padding:4px;">
        <label for="tid" style="float:left; width:40px; font-size:16px; line-height:28px; color:#888;">TID</label>
        <input type="text" id="tid" name="tid" value={ tid } style="font-size:20px; width:300px;"/>
        <input type="submit" class="go btn" value="Analyze" style="font-size:16px;"/>
       </div>
      </form>
     </div>
    </div> ++
    { if ( rec.isDefined ) {
        val r = rec.get

        <div class="fieldHeader">
         <label>Type</label><span>Record</span>
         <label style="margin-left:16px;">Label</label><span>{ r.label.summarize().encUnicode }</span>
         <label style="margin-left:16px;">Entity</label><span><a href={ wpath + "/field?tid=" + entity.tid }>{ entity.name }</a></span>
         <label style="margin-left:16px;">Storage</label><span>{ entity.storageName + ( entity.embedded |* "-Embedded" ) }</span>
         { entity.isInstanceOf[MongoEntity] |* <a href={ wpath + "/delete?tid=" + tid } class="stop btn" style="float:right; margin:2px 4px;">Delete</a> }
        </div> ++
        { recordTabBar.draw(
            qs = "?tid=" + tid,
            except = Seq(
              !entity.isInstanceOf[MongoEntity] |* Some( "/json" ),
              !entity.isInstanceOf[Versioning]  |* Some( "/version" )
            ).flatten
          ) } ++
        { recordTabBar.choice match {
          case "/field"   => fields( r )
          case "/json"    => json( r )
          case "/ref"     => refs( tid )
          case "/version" => <div style="overflow:scroll;">{ Versioning.ui( this, tid ) }</div>
          }
        }
      } else if ( entity != null ) {
        <div class="fieldHeader">
         <label>Type</label><span>{ if ( Tid.isRecordTid( tid ) ) "Record" else "Entity" }</span>
         <label style="margin-left:16px;">Label</label><span>{ entity.label }</span>
         <label style="margin-left:16px;">Entity</label><span>{ entity.name }</span>
         <label style="margin-left:16px;">Storage</label><span>{ entity.storageName + ( entity.embedded |* "-Embedded" ) }</span>
        </div> ++
        { if ( Tid.isRecordTid( tid ) )
            <div style="color:red;">Invalid TID.</div>
          else
            entityTabBar.draw(
              qs = "?tid=" + tid,
              except = if ( entity.embedded ) Seq( "/record" ) else Nil ) ++
            { entityTabBar.choice match {
              case "/attrib" => attribs( entity )
              case "/record" => AutoQuery.byEntity( entity ).draw
              }
            }
        }
      } else {
        tid.notBlank |* Unparsed( """<span style="color:red;">Invalid TID.</span>""" )
      }
    }
  }

  def fields( rec:Record ) = {

    def ui( rec:Record ) = {
      val pathValues = rec.flatten.sorted( PathValue.orderByLabel )
      val scope = new Scope( rec )

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
             <span id={ id } style="display:block; margin-left:400px;">{ tidLink( tid, link.see( v ) ) }</span>
            </div>
          }

        case tlink:DbTid =>
          v != null |* {
            <div style={ divStyle }>
             <label for={ id } style="float:left;">{ path.label }</label>
             <span id={ id } style="display:block; margin-left:400px;">{ tidLink( v.toString, tlink.see( v ) ) }</span>
            </div>
          }

        case d =>
          v.safeString.notBlank |*
          <div style={ divStyle }>
           <label for={ id } style="float:left;">{ path.label }</label>
           <span id={ id } style="display:block; margin-left:400px;">{ val ns = PathField( pv.path ).cell( scope ); if ( ns == NodeSeq.Empty ) Unparsed("&nbsp;") else ns }</span>
          </div>
        }
      }
    }

    ui( rec ).flatten
  }

  def json( rec:Record ) = {
    rec match {
    case mr:MongoRecord =>
      <pre style="padding:2px 0;">{ Unparsed( mr.obj.toPretty( markup = true ) ) }</pre>

    case _ =>
      <div style="padding:2px 0;">Not a MongoDB object.</div>
    }
  }

  def refs( tid:String ) = displayTable( Tid.references( tid ) )

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
     <thead>
      <tr>
       <th>Name</th>
       <th>Label</th>
       <th>Domain</th>
       <th style="width:300px;">Help</th>
      </tr>
     </thead>
     { for ( a <- en.attribs.clone.sortBy( _.name ) ) yield
       <tr>
        <td>{ a.name }</td>
        <td>{ a.label }</td>
        <td>{ Unparsed( describe( a.domain ) ) }</td>
        <td>{ a.help }</td>
       </tr>
     }
    </table>
  }

  def delete( tid:String ) = {
    val deleting = T.web.req.b( 'deleting )
    val results = Tid.delete( tid, deleting )

    <div class="plainBox">
     <div class="title">Deletion {
       if ( results.success ) ( if ( deleting ) "Success" else "is Possible" )
       else                   ( if ( deleting ) "Failed"  else "is NOT Possible" )
     }.</div>
     <div class="content">
      { results.ramReferences.nonEmpty |*
      <h3>RAM References:  { results.ramReferences.size } (cannot be present to delete)</h3> ++ displayTable( results.ramReferences ) }
      { results.cascadeFailures.nonEmpty |*
      <h3>Cascading References:  { results.cascadeFailures.size } (cannot be present to delete)</h3> ++ displayTable( results.cascadeFailures ) }
      { results.deletes.nonEmpty |*
      <h3>Deletions:  { results.deletes.size }</h3> ++ displayTable( results.deletes ) }
      { results.updates.nonEmpty |*
      <h3>Updates:  { results.updates.size }</h3> ++ displayTable( results.updates ) }
     </div>
    </div> ++
    { results.success && !deleting |* <a href={ wpath + "/delete?tid=" + tid + "&deleting=true" } class="stop btn">Actually Delete</a> } ++
    <a href={ wpath + "?tid=" + tid } class="btn">Cancel</a>
  }

  private def displayTable( recs:Seq[Record] ) =
    <table class="dtable">
     <tr><th style="width:200px;">Entity</th><th>Label</th><th style="width:200px;">TID</th></tr>{
       recs.map { r =>
         <tr>
          <td>{ r.view.entity.name }</td><td><b>{ r.label }</b></td><td><i>{ tidLink( r.tid ) }</i></td>
         </tr>
       }
    }</table>
}


