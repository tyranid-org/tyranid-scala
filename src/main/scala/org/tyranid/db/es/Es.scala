/**
 * Copyright (c) 2008-2013 Tyranid <http://tyranid.org>
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

import scala.collection.mutable

import com.mongodb.{ DBObject, BasicDBList }

import akka.actor.{ Actor, ActorSystem, Props }

import org.tyranid.Imp._
import org.tyranid.db.{ DbArray, DbDateLike, DbLink, DbNumber, DbTextLike, Domain, Entity, Record, View, ViewAttribute }
import org.tyranid.db.mongo.Imp._
import org.tyranid.json.Json
import org.tyranid.profile.User


/*
 * * *   Searchable
 */

sealed trait Searchable {
  val auth:Boolean
  val text:Boolean
  val analyze:Boolean

  def extract( rec:Record, va:ViewAttribute ) = rec( va )
}

trait SearchTextLike extends Searchable {
  val auth    = false
  val text    = true
  val analyze = true
}

case object SearchText extends SearchTextLike

case object SearchToken extends Searchable {
  val auth    = false
  val text    = true
  val analyze = false
}

case object SearchAuth extends Searchable {
  val auth    = true
  val text    = false
  val analyze = false
}

case object NoSearch extends Searchable {
  val auth    = false
  val text    = false
  val analyze = false
}


/*
 * * *   Indexing
 */

case class IndexMsg( index:String, typ:String, id:String, json:String )

object Indexer {
  val system = ActorSystem( "MySystem" )
  // TODO:  add fault tolerance / load balancing / etc.
  lazy val actor = system.actorOf( Props[Indexer], name = "indexer" )//.start()
}

class Indexer extends Actor {

  def receive = {
  case IndexMsg( index, typ, id, json ) =>

    try {
      if ( json != "{}" ) {
//sp am( "posting index=" + index + "  type=" + typ )
//sp am( "url=" + Es.host + "/" + index + "/" + typ + "/" + id )
//sp am( "json=" + json )

        val response  = ( Es.host + "/" + index + "/" + typ + "/" + id ).POST( content = json )
        val responseJson = Json.parse( response._s )
        
        val error = responseJson.s( 'error )
        
        if ( error.notBlank ) {
          println( "ES Indexing Failure on ID " + id + ", error=" + responseJson + "\n\nIndex JSON:\n\n" + json )
          Log.log( Event.Search, "m" -> ( "Failed to index id " + id + ", type=" + typ + ", err=" + error ) ) 
        }
      }
    } catch {
    case e:org.apache.http.conn.HttpHostConnectException =>
      e.logWith( "m" -> "Cannot index in elastic search-- it does not appear to be running" )
    }
  }

}

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
  val ElasticSearchDefaultPageSize = 10 // this needs to map to elasticsearch's actual default page size

  val UTF8_CHARSET = java.nio.charset.Charset.forName("UTF-8")
  val host = "http://localhost:9200"

  def search( text:String, user:User, offset:Int = 0, pageSize:Int = ElasticSearchDefaultPageSize ):ObjectMap =
    search(
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
      ),
      user,
      offset = offset,
      pageSize = pageSize
    )

  def search( query:Map[String,Any], user:User, offset:Int, pageSize:Int ):ObjectMap = {
//sp am( "query=" + query.toJsonStr )

    var params =
      if ( offset != 0 && pageSize != ElasticSearchDefaultPageSize )
        "?from=" + offset + "&size=" + pageSize
      else if ( offset != 0 )
        "?from=" + offset
      else if ( pageSize != ElasticSearchDefaultPageSize )
        "?size=" + pageSize
      else
        ""

    val s = ( Es.host + "/_search" + params ).POST( content = query.toJsonStr( false ) ).s
//sp_am( "results=[\n\n" + results + "\n\n]" )

    val json = s.parseJsonObject

    val error = json.s( 'error )
    
    if ( error.notBlank ) {
      println( "ES Search Failure.  Query=\n\n" + query.toJsonStr( false ) + "\n\nError:\n\n" + error )
      Log.log( Event.Search, "m" -> ( "ElasticSearch search failure=" + error ) ) 
    }

    json
  }

  def jsonFor( rec:Record ) = {
    val sb = new StringBuilder

    def array( va:ViewAttribute, arr:BasicDBList ) {
      sb += '[' ++= arr.toSeq.map( v => va.domain.as[DbArray].of.see( v ).toJsonStr( false ) ).mkString( "," ) += ']'
    }

    def enter( rec:Record, root:Boolean = false ) {
      val view = rec.view
      

      sb += '{'
      var first = true

      for ( va <- view.vas;
            search = va.att.search;
            if search.text;
            v = search.extract( rec, va );
            if v != null ) {

        if ( first ) first = false
        else         sb += ','

        sb ++= va.att.dbName += ':'
        va.domain match {
        case link:DbLink =>
          sb += '"' ++= link.idToTid( v ) += '"'

        case _ =>
          v match {
          case crec:Record     => enter( crec )
          case arr:BasicDBList => array( va, arr )
          case dbo:DBObject    => enter( rec.rec( va ) )
          case v:Number        => sb ++= v.toString
          case t:Date          => sb += '"' ++= t.toIso8601Str += '"'
          //case v               => sb += '"' ++= new String( v.toString.getBytes(UTF8_CHARSET), UTF8_CHARSET ).encJson += '"'
          case v               => sb += '"' ++= v.toString.encJson += '"'
          }
        }
      }

      if ( root ) {
        if ( !first ) sb += ','
        sb ++= "auth:" ++=
          ( view.vas.find( _.att.search.auth ) match {
            case Some( va ) =>
              rec( va ) match {
              case tids:BasicDBList => tids.toJsonStr( false )
              case tid:String       => tid.toJsonStr( false )
              case _                => "\"no\""
              }

            case None =>
              "\"yes\""
            } )

        if ( rec.entity.searchText ) {
          var t = rec.searchText

          if ( t.notBlank )
            sb ++= ",st:\"" ++= t.encJson += '"'
        }
      }

      sb += '}'
    }

    enter( rec, root = true )

    //sp am( "index json=\n\n" + sb.toString + "\n\n" )
    sb.toString
  }

  def mappingFor( rootEn:Entity ) = {

    def enter( props:mutable.Map[String,Any], en:Entity ) {

      for ( att <- en.attribs;
            if att.search.text ) {

        def domain( dom:Domain ) {
          dom match {
          case link:DbLink =>
            props( att.dbName ) = Map( "type" -> "string", "index" -> "not_analyzed" )

          case text:DbTextLike =>
            props( att.dbName ) = if ( att.search.analyze ) Map( "type" -> "string" )
                                  else                      Map( "type" -> "string", "analyzer" -> "string_lowercase" /* "index" -> "not_analyzed" */ )

          case number:DbNumber =>
            props( att.dbName ) = Map( "type" -> "number" )

          case date:DbDateLike =>
            props( att.dbName ) = Map( "type" -> "date" )

          case ce:Entity =>
            if ( ce.isSearchable ) {
              val cprops = mutable.Map[String,Any]()
              enter( cprops, ce )
              props( att.dbName ) = Map( "type" -> "object", "properties" -> cprops )
            }

          case array:DbArray =>
            domain( array.of )

          case v =>
            // unmapped
          }
        }

        domain( att.domain )
      }
    }

    val props = mutable.Map[String,Any]()

    props( "auth" ) = Map(
      "type" -> "string",
      "index" -> "not_analyzed"
    )

    enter( props, rootEn )

    if ( rootEn.searchText )
      props( "st" ) = Map( "type" -> "string" )

    Map(
      "properties" -> props
    )
  }

  def index( rec:Record ) = {
    try {
      if ( B.DEV ) println( "ES:  indexing " + rec.tid )
//sp am( "getting JSON" )
      val json = jsonFor( rec )
//sp am( "queuing to index" )
      Indexer.actor ! IndexMsg( rec.view.entity.searchIndex, rec.view.entity.dbName, rec.tid, json )
//sp am( "done" )

      //Indexer.actor ! IndexMsg( rec.view.entity.searchIndex, rec.view.entity.dbName, rec.tid, jsonFor( rec ) )
    } catch {
      case e:Exception =>
        Log.log( Event.Search, "m" -> ( "Failed to index, err=" + e ), "ex" -> e )
    }
  }

  def deleteAll =
    for ( index <- Entity.all.filter( e => e.isSearchable && !e.embedded ).map( _.searchIndex ).toSeq.distinct )
      ( Es.host + "/" + index ).DELETE()

  def mapAll {
    println( "ElasticSearch Mapping STARTED" )

    for ( index <- Entity.all.filter( e => e.isSearchable && !e.embedded ).map( _.searchIndex ).toSeq.distinct ) {
      val mappings = mutable.Map[String,Any]()

      for ( e <- Entity.all;
            if e.searchIndex == index && !e.embedded && e.isSearchable )
        mappings( e.dbName ) = mappingFor( e )

        val content = Map(
          "settings" -> Map(
            "index" -> Map(
              "number_of_shards" -> 3,
              "number_of_replicas" -> 1,
              "analysis" -> Map(
                "analyzer" -> Map(
                  "string_lowercase" -> Map(
                    "type"      -> "custom",
                    "tokenizer" -> "keyword",
                    "filter"    -> Seq( "lowercase", "trim" )
                  )
                )
              )
            )
          ),
          "mappings" -> mappings
        ).toJsonStr( false )

      if ( true )
        println( "Using Map:\n\n" + content )

      ( Es.host + "/" + index + "/" ).PUT( content = content )
    }

    println( "ElasticSearch Mapping COMPLETED" )
  }

  def indexAll {
    //if ( true ) {
      //val broken = Record.getByTid( "a09vUcjPbeSw48-4weNm" )
      //index( broken )
      //return
    //}

    deleteAll
    mapAll

    var count = 0
    for ( e <- Entity.all;
          if !e.embedded && e.isSearchable ) {
      // TODO:  we're reading all the records in memory since the Tika extraction code can take so long on a handful of large documents that the mongodb cursor times out
      //        need to implement some sort of paging mechanism to avoid reading all (searchable) records in memory or utilize MongoDB's no query timeout code (which is scary in different ways)
      //        another option would be to just read in all the TIDs and then read each record individually.
      val recs = e.records.filter( e.searchIndexable ).toSeq
      for ( r <- recs )
        r.tid // force it to get read in, toSeq alone wasn't doing it

      for ( r <- recs )
        index( r )

      count += 1
    }
      
    println( "ES: indexAll completed, " + count + " documents indexed." )
  }
}

