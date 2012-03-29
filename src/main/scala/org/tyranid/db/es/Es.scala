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

package org.tyranid.db.es

import java.util.Date

import com.mongodb.DBObject

import akka.actor.Actor
import akka.actor.Actor.actorOf

import org.tyranid.Imp._
import org.tyranid.db.{ Entity, Record, View }
import org.tyranid.db.mongo.MongoEntity
import org.tyranid.db.mongo.Imp._


/*
 * * *   Searchable
 */

trait Searchable {

  val text:Boolean
}

case class Search( text:Boolean = true ) extends Searchable {
}

case object NoSearch extends Searchable {
  val text = false
}


/*
 * * *   Indexing
 */

object Indexer {

  // TODO:  add fault tolerance / load balancing / etc.
  lazy val actor = actorOf[Indexer].start()
}

class Indexer extends Actor {

  def receive = {
  case IndexMsg( index, typ, id, json ) =>

    try {
      if ( json != "{}" )
        spam( "indexing:  " + ( "http://localhost:9200/" + index + "/" + typ + "/" + id ).POST( content = json ) )
    } catch {
    case e:org.apache.http.conn.HttpHostConnectException =>
      spam( "Cannot index in elastic search-- it does not appear to be running: " + e.getMessage )
    }
  }

}

case class IndexMsg( index:String, typ:String, id:String, json:String )


/**
 * ES = ElasticSearch
 *
 * NOTE: decide between the native Java API vs. the REST/JSON API
 *
 *       native:  the REST/JSON API is implemented in terms of the native API internally, so the native API should be faster
 *       native:  the native API allows you to join the cluster as a data-less node, this means that routing is done in 1-hop instead of 2-hops
 *       rest:    less JARs to link in ( added ~10MB to WAR size )
 *       rest:    JAR is hosted at Sonatype, not at scala-tools or maven, so another repo to bring in
 *       rest:    less configuration?  (not sure about this)
 *       
 *       *** going with REST/JSON for now ***
 */
object Es {

  def search( text:String ) = "http://localhost:9200/_search".GET( Map( "q" -> text ) )


  def jsonFor( rec:Record ) = {
    val sb = new StringBuilder

    def enter( rec:Record ) {
      val view = rec.view

      sb += '{'
      var first = true

      for ( va <- view.vas;
            if va.att.search.text;
            v = rec( va );
            if v != null ) {

        if ( first ) first = false
        else         sb += ','

        sb ++= va.att.dbName += ':'
        v match {
        case crec:Record  => enter( crec )
        case dbo:DBObject => enter( rec.rec( va ) )
        case v:Number     => sb ++= v.toString
        case t:Date       => sb ++= t.getTime.toString
        case v            => sb ++= ( '"' + v.toString.encJson + '"' )
        }
      }

      sb += '}'
    }

    enter( rec )

    sb.toString
  }

  def hasSearchData( v:View ) = v.vas.exists( _.att.search.text )

  def index( rec:Record ) =
    Indexer.actor ! IndexMsg( rec.view.entity.searchIndex, rec.view.entity.dbName, rec.tid, jsonFor( rec ) )

  def indexAll = {
    for ( e <- Entity.all ) {

      e match {
      case e:MongoEntity =>
        val v = e.makeView
        if ( hasSearchData( v ) )
          for ( obj <- e.db.find() )
            index( e.make( obj ) )

      case _ =>
      }
    }
  }
}

