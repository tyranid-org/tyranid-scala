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

import org.cometd.bayeux.server.{ BayeuxServer, ServerSession }
import org.cometd.server.AbstractService

import org.tyranid.Imp._
import org.tyranid.json.JsCmd
import org.tyranid.session.{ Session, WebSession }


case class CometService( name:String, create: ( BayeuxServer ) => AbstractService ) {

  var service:AbstractService = null

  def init( bayeux:BayeuxServer ) {
    service = create( bayeux )
  }
}


case class Comet( serviceSession:ServerSession, fromSession:ServerSession, session:Session ) {

  def send( output:collection.Map[String,Any], cmds:JsCmd* ) {
    val data =
      if ( cmds != null && cmds.nonEmpty )
        output + ( "cmds" -> cmds.filter( _ != null ).map( _.toMap ).toJsonStr( client = true ) )
      else
        output

    val jOutput:java.util.Map[String,Object] = data.asInstanceOf[collection.Map[String,AnyRef]]

    fromSession.deliver( serviceSession, "/message", jOutput, null )
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
    println( "remove: "+ serverSessionId )
    
    val bSessions = new java.util.ArrayList( B.bayeux.getSessions )
    
    for ( session <- bSessions ) {
      val httpSessionId = session.getAttribute( WebSession.CometHttpSessionIdKey )

      println( "found: " + httpSessionId )
      if ( httpSessionId != null && serverSessionId == httpSessionId ) {
        println( "Removed!: " + httpSessionId )
        B.bayeux.getSessions().remove( session )
      }
    }
  }
    
  def visit( visitor: ( Comet ) => Unit ) = {
    val serverSession = B.comets.find( _.name == "message" ).get.service.getServerSession
    //val seen = mutable.Set[String]()

    for ( session <- B.bayeux.getSessions ) {
      val httpSessionId = session.getAttribute( WebSession.CometHttpSessionIdKey )

      //println( "send to : " + httpSessionId )
      if ( httpSessionId != null ) {
        //println( "visiting: " + httpSessionId )
        val httpSessionIdStr = httpSessionId.as[String]
        
        //if ( !seen( httpSessionIdStr ) ) {
          //seen += httpSessionIdStr
          
          val tyrSession = Session.byHttpSessionId( httpSessionIdStr ).as[Session] // as a Volerro session
          visitor( Comet( serverSession, session, tyrSession ) )
       // }
      }
    }
  }
}

