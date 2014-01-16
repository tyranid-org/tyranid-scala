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
import org.tyranid.json.{ JsCmd, JsCmds, JsModel }
import org.tyranid.net.Ip
import org.tyranid.session.{ Session, SessionData, WebSession }




/*

   websockets

   X. install script to install node.js server

       X. copy direct

   X. change push.js to find security keys correctly

   X. change push.js to find mongodb url correctly

   /. multiple servers


 */

case class CometService( name:String, create: ( BayeuxServer ) => AbstractService ) {

  var service:AbstractService = null

  def init( bayeux:BayeuxServer ) {
    service = create( bayeux )
  }
}


case class Comet( session:SessionData ) {

  var output:collection.Map[String,AnyRef] = null

  def user = session.user

  def send( cmds:JsCmd* ) {
    send( null.asInstanceOf[collection.Map[String,Any]], cmds:_* )
  }
  
  def send( output:collection.Map[String,Any], cmds:JsCmd* ) {
    
    val flatCmds = mutable.Buffer[JsCmd]()
    
    def addCmds( cmds:Seq[JsCmd] ) {
      for ( cmd <- cmds )
        cmd match {
        case cmds:JsCmds =>
          addCmds( cmds.cmds )
  
        case _ =>
          flatCmds += cmd
        }
    }
    
    addCmds( cmds )
    
    val outMap = ( output == null ) ? Map[String,Any]() | output 
    
    val o =
      if ( flatCmds != null && flatCmds.nonEmpty )
        outMap + ( "cmds" -> flatCmds.filter( _ != null ).map( _.toMap ).toJsonStr( client = true ) )
      else
        outMap

    this.output = o.asInstanceOf[collection.Map[String,AnyRef]]
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

//sp am( "visiting " + sd.user.label )

      visitor( comet )

//sp am( "results " + comet.output )
      if ( comet.output != null ) {

        val sv = sd.s( 'sv )

        if ( PushQueue.active ) {
          PushQueue.db.save(
            Mobj(
              "h"  -> false, // this can't left undefined, because you can't update a document in a capped mongo collection to be larger
              "ss" -> sd.s( 'ss ),
              "m"  -> comet.output.toDBObject
            )
          )

        } else if ( true || // DEBUG:  force everything remote so we can test the queue
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
    B.comets.find( _.name == "message" ) foreach { comet =>
      val service = comet.service

      if ( service != null ) {
        val serverSession = comet.service.getServerSession

        for ( session <- B.bayeux.getSessions ) {
          if ( session.getAttribute( WebSession.CometHttpSessionIdKey ) == httpSessionId ) {
            session.deliver( serverSession, "/message", m, null )
          }
        }
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
//sp am( "cometqueue processing " + obj )
            Comet.send( obj.s( 'ss ), obj.o( 'm ).toMap.asInstanceOf[java.util.Map[String,AnyRef]] )

            db.update( Mobj( "_id" -> obj( '_id ) ), Mobj( $set -> Mobj( "h" -> true ) ) )
          }
        }
      } catch {
        case t:Throwable =>
          t.fillInStackTrace
          log( Event.Alert, "m" -> t.getMessage, "ex" -> t )
      }
    }
  }

  lazy val localName = org.tyranid.net.Ip.Host

  def init {

    createCollectionFor( localName )

    background {
      process
    }
  }
}


/*
 * * *  PushQueue
 */

object PushQueue {

  val active = true

  //"_id"      is DbMongoId         is 'id;

  //"m"        is DbObject          as "Comet Message";
  //"ss"       is DbChar(32)        as "HTTP Session ID";
  //"h"        is DbBoolean         as "Handled";

  lazy val db = {
    val mongo = Mongo.connect.db( B.profileDbName )

    val collName = "pushing"

    if ( !mongo.collectionExists( collName ) ) {
      mongo.createCollection(
        collName,
        Mobj(
          "capped" -> true,
          "size"   -> ( 8 * 1024 * 1024 )
        )
      )

      // place a dummy "already-handled" object into the capped collection so that the query will "await data" properly ... if the collection is empty the query will return immediately
      mongo( collName ).save( Mobj( "h" -> true ) )
    }

    Mongo.connect.db( B.profileDbName )( collName )
  }

  def init {

    db

    background {

      // give the server 3 seconds to get settled before we start pulverise it

      Thread.sleep( 3000 )

      PushQueue.db.save(
        Mobj(
          "h"       -> false, // this can't left undefined, because you can't update a document in a capped mongo collection to be larger
          "restart" -> true
        )
      )
    }
  }
}

object Cometlet extends Weblet {
  
  def handle( web:WebContext ) = {
    rpath match {
    case "/sync" =>
      web.jsRes(
        Map(
          "session" -> T.session.httpSessionId
        )
      )

    case _ =>
      _404
    }
  }
}

