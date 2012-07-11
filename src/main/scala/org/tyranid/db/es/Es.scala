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

package org.tyranid.db.es

import java.util.Date

import com.mongodb.{ DBObject, BasicDBList }

import akka.actor.Actor
import akka.actor.Actor.actorOf

import org.tyranid.Imp._
import org.tyranid.db.{ Entity, Record, View }
import org.tyranid.db.mongo.MongoEntity
import org.tyranid.db.mongo.Imp._
import org.tyranid.profile.User


/*
 * * *   Searchable
 */

sealed trait Searchable {
  val auth:Boolean
  val text:Boolean
}

case object SearchText extends Searchable {
  val auth = false
  val text = true
}

case object SearchAuth extends Searchable {
  val auth = true
  val text = false
}

case object NoSearch extends Searchable {
  val auth = false
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
      if ( json != "{}" ) {
        spam( "posting index=" + index + "  type=" + typ )
        spam( "json=" + json )
        ( Es.host + "/" + index + "/" + typ + "/" + id ).POST( content = json )
      }
    } catch {
    case e:org.apache.http.conn.HttpHostConnectException =>
      e.logWith( "m" -> "Cannot index in elastic search-- it does not appear to be running" )
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

  val host = "http://localhost:9200"

  def search( text:String, user:User ) = {
    val query =
      Map(
        "query" -> Map(
          "filtered" -> Map(
            "query" -> Map(
              "query_string" -> Map(
                "query" -> text
              )
            ),
            "filter" -> Map(
              "terms" -> Map(
                "auth" -> ( "yes" +: user.allowProfileTids ) //.filter( _ == "a01vTzHoIeSwMnKLiSlN" ) )
              )
            )
          )
        )
      ).toJsonStr
    spam( "query=" + query )
    ( Es.host + "/_search" ).POST( content = query ).s
  }

  def jsonFor( rec:Record ) = {
    val sb = new StringBuilder

    def enter( rec:Record, root:Boolean = false ) {
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
        case v            => sb += '"' ++= v.toString.encJson += '"'
        }
      }

      if ( root ) {
        if ( !first ) sb += ','
        sb ++= "auth:" ++=
          ( view.vas.find( _.att.search.auth ) match {
            case Some( va ) =>
              rec( va ) match {
              case tids:BasicDBList => tids.toJsonStr
              case tid:String       => tid.toJsonStr
              case _                => "\"no\""
              }

            case None =>
              "\"yes\""
            } )
      }

      sb += '}'
    }

    enter( rec, root = true )

    sb.toString
  }

  def hasSearchData( v:View ) = v.vas.exists( _.att.search.text )

  def index( rec:Record ) =
    Indexer.actor ! IndexMsg( rec.view.entity.searchIndex, rec.view.entity.dbName, rec.tid, jsonFor( rec ) )

  def indexAll {
    for ( index <- Entity.all.filter( e => hasSearchData( e.makeView ) ).map( _.searchIndex ).toSeq.distinct )
      ( Es.host + "/" + index ).DELETE()

    for ( e <- Entity.all ) {

      /*
       * TODO:  finish put mapping
      ( Es.host + "/" ).PUT( content =
        Map(



        ).toJsonStr )
      */


      e match {
      case e:MongoEntity =>
        val v = e.makeView
        if ( hasSearchData( v ) )
          for ( obj <- e.db.find();
                r = e.make( obj );
                if e.searchIndexable( r ) )
            index( r )

      case _ =>
      }
    }
  }
}

