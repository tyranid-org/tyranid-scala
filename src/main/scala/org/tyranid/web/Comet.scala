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

package org.tyranid.web

import scala.collection.JavaConversions._
import scala.collection.mutable

import com.mongodb.{ Bytes, DBObject }

import org.cometd.bayeux.server.{ BayeuxServer, ServerSession }
import org.cometd.server.AbstractService

import org.tyranid.Imp._
import org.tyranid.db.{ DbChar, DbLink }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity, MongoRecord }
import org.tyranid.json.JsCmd
import org.tyranid.net.Ip
import org.tyranid.session.{ Session, SessionData, WebSession }


case class CometService( name:String, create: ( BayeuxServer ) => AbstractService ) {

  var service:AbstractService = null

  def init( bayeux:BayeuxServer ) {
    service = create( bayeux )
  }
}


case class Comet( session:SessionData ) {

  var output:collection.Map[String,AnyRef] = null

  def user = session.user

  def send( output:collection.Map[String,Any], cmds:JsCmd* ) {
    val o =
      if ( cmds != null && cmds.nonEmpty )
        output + ( "cmds" -> cmds.filter( _ != null ).map( _.toMap ).toJsonStr( client = true ) )
      else
        output

    this.output = o.asInstanceOf[collection.Map[String,AnyRef]];
  }

  def send( act:String, data:collection.Map[String,Any], cmds:JsCmd* ) {
    send(
      Map(
        "act"  -> act,
        "data" -> data.toJsonStr( client = true )
      ),
      cmds:_*
    )
  }
}

object Comet {

  def remove( serverSessionId:String ) {
    val bSessions = new java.util.ArrayList( B.bayeux.getSessions )
    
    for ( session <- bSessions ) {
      val httpSessionId = session.getAttribute( WebSession.CometHttpSessionIdKey )

      if ( httpSessionId != null && serverSessionId == httpSessionId )
        B.bayeux.getSessions().remove( session )
    }
  }
    
  def visit( visitor: ( Comet ) => Unit ) = {

    for ( sd <- B.SessionData.records;
          u = sd( 'u );
          if u != null ) {

      val comet = Comet( sd )

spam( "visiting " + sd.user.label )

      visitor( comet )

spam( "results " + comet.output )
      if ( comet.output != null ) {

        val sv = sd.s( 'sv )

        if ( true || // DEBUG:  force everything remote so we can test the queue
             sv != Ip.Host ) {
          CometQueue.dbFor( sv ).save(
            Mobj(
              "h"  -> false, // this can't left undefined, because you can't update a document in a capped mongo collection to be larger
              "ss" -> sd.s( 'ss ),
              "m"  -> comet.output.toDBObject
            )
          )
        } else {
          send( sd.s( 'ss ), comet.output )
        }
      }
    }
  }


  def send( httpSessionId:String, m:java.util.Map[String,AnyRef] ) = {
    val serverSession = B.comets.find( _.name == "message" ).get.service.getServerSession

    for ( session <- B.bayeux.getSessions ) {
      if ( session.getAttribute( WebSession.CometHttpSessionIdKey ) == httpSessionId ) {
        session.deliver( serverSession, "/message", m, null )
      }
    }
  }
}


/*
 * * *  CometQueue
 */

object CometQueue {

  //"_id"      is DbMongoId         is 'id;

  //"m"        is DbObject          as "Comet Message";
  //"ss"       is DbChar(32)        as "HTTP Session ID";


  def dbNameFor( ip:String ) = "comet_" + ip.replace( ".", "_" ).replace( ":", "_" )

  def dbFor( ip:String ) = Mongo.connect.db( B.profileDbName )( dbNameFor( ip ) )

  def createCollectionFor( ip:String ) = {

    val db = Mongo.connect.db( B.profileDbName )

    val colName = dbNameFor( ip )

    if ( !db.collectionExists( colName ) ) {
      db.createCollection(
        colName,
        Mobj(
          "capped" -> true,
          "size"   -> ( 2 * 1024 * 1024 )
        )
      )

      // place a dummy "already-handled" object into the capped collection so that the query will "await data" properly ... if the collection is empty the query will return immediately
      db( colName ).save( Mobj( "h" -> true ) )
    }
  }

  def process = {

    val db = dbFor( localName )

    def query = db.find( Mobj( "h" -> false ) ).addOption( Bytes.QUERYOPTION_TAILABLE ).addOption( Bytes.QUERYOPTION_AWAITDATA )

    while ( true ) {

      try {

        var cursor = query

        while ( true ) {
          if ( !cursor.hasNext ) {

            if ( cursor.getCursorId == 0 ) { // a.k.a. cursor.isDead ?
              Thread.sleep( 1000 )
              cursor = query
            }
          } else {
            val obj = cursor.next
spam( "cometqueue processing " + obj )
            Comet.send( obj.s( 'ss ), obj.o( 'm ).toMap.asInstanceOf[java.util.Map[String,AnyRef]] )

            db.update( Mobj( "_id" -> obj( '_id ) ), Mobj( $set -> Mobj( "h" -> true ) ) )
          }
        }
      }
    }
  }

  lazy val localName = org.tyranid.net.Ip.Host

  def init = {

    createCollectionFor( localName )

    background {
      process
    }
  }
}

