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

package org.tyranid.session

import java.util.Date

import javax.servlet.http.HttpSession

import scala.collection.mutable
import scala.xml.{ Node, NodeSeq, Unparsed }

import org.tyranid.Imp._
import org.tyranid.db.{ DbChar, DbDateTime, Record }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity }
import org.tyranid.profile.User
import org.tyranid.report.{ Field, Run, MongoQuery }
import org.tyranid.web.WebContext


object AccessLog extends MongoEntity( tid = "a0Bu" ) {
  "id"                  is DbMongoId      is 'key;
  "sid"                 is DbChar(64)     as "Session";
  "uid"                 is DbMongoId      as "User";
  "ua"                  is DbChar(256)    as "User Agent";
  "on"                  is DbDateTime;

  def log( ctx:WebContext, httpSession:HttpSession, session:Session ) {

    if ( !session.loggedUser ) {
      val user = session.user

      if ( user.loggedIn ) {
        db.save( Mobj(
          "uid" -> user.id,
          "sid" -> httpSession.getId,
          "ua" -> ctx.req.getHeader("User-Agent"),
          "on" -> new Date ) )

        session.loggedUser = true
        session.loggedEntry = true
      }
    }

    if ( !session.loggedEntry && httpSession != null ) {
      db.save( Mobj(
        "sid" -> httpSession.getId,
        "ua" -> ctx.req.getHeader("User-Agent"),
        "on" -> new Date ) )

      session.loggedEntry = true
    }
  }
}

object AccessLogQuery extends MongoQuery {

  //def connections( run:Run ) =
    //run.cache.getOrElseUpdate( "connections", Connection.db.find( Mobj( "from" -> Session().user.org.id ) ).toSeq ).asInstanceOf[Seq[DBObject]]

  val entity = AccessLog
  val name = "accessLog"

  override def newReport = {
    var r = super.newReport
    r.sort = Mobj( "on" -> -1 )
    r
  }

  val allFields = Seq(
    dateTime( "on" ),
    string( "sid" ),
    new Field {
      def name = "uid"
      override def label = "User"
      def cell( run:Run, r:Record ) = {
        r.oid( 'uid ) match {
        case null => Unparsed( "" )
        case uid  => Unparsed( Tyr.userMeta.nameFor( uid ) )
        }
      }
    },
    string( "ua" )
  )

  val defaultFields = allFields
}

