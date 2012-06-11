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

package org.tyranid.session

import javax.servlet.http.HttpSession

import scala.collection.mutable
import scala.xml.NodeSeq

import org.tyranid.Imp._
import org.tyranid.db.Scope
import org.tyranid.db.meta.TidItem
import org.tyranid.db.mongo.Imp._
import org.tyranid.http.UserAgent
import org.tyranid.log.Log
import org.tyranid.report.{ Query, Report }
import org.tyranid.ui.{ Checkbox, CustomField, Search }
import org.tyranid.web.{ Weblet, WebContext }


case class Milestone( name:String, satisfies:( Log ) => Boolean )

object AccessLog {

  def log( web:WebContext, thread:ThreadData ) {
    val session = thread.session

    if ( B.accessLogs ) {
      web.path match {
      case "/cometd" => // ignore
      case p if p.endsWith( ".png" ) || p.endsWith( ".js" ) || p.endsWith( ".gif" ) || p.endsWith( ".css" ) || p.endsWith( ".ico" ) || p.endsWith( ".html" ) => // ignore
      case p         =>
        Log.log( Event.Access,
                 "p"   -> p, 
                 "bid" -> TrackingCookie.get,
                 "ua"  -> web.req.getHeader( "User-Agent" ) )
      }

    } else {
      // still do "digest" access logs

      if ( !session.loggedUser ) {
        val user = session.user

        if ( user.loggedIn ) {
          Log.log( Event.Access, "ua" -> web.req.getHeader( "User-Agent" ) )
          session.loggedUser = true
          session.loggedEntry = true
        }
      }

      if ( !session.loggedEntry && thread.http != null ) {
        Log.log( Event.Access, "ua" -> web.req.getHeader( "User-Agent" ) )
        session.loggedEntry = true
      }
    }
  }
}

object TrackingCookie {

  def get = {
    val t = T

    var token = T.web.req.cookieValue( B.trackingCookieName )
    if ( token.isBlank ) {
      token = org.tyranid.math.Base62.make( 10 )

      val cookie = new javax.servlet.http.Cookie( B.trackingCookieName, token )
      cookie.setMaxAge(60 * 60 * 24 * 365) // one year
      cookie.setPath("/")
      t.web.res.addCookie( cookie )
    }

    val u = t.user
    val tokens = u.a_?( 'bids ).toSeq.of[String]

    if ( !tokens.contains( token ) ) {
      val list = ( tokens :+ token ).toMlist
      u( 'bids ) = list
      B.User.db.update( Mobj( "_id" -> u.id ), Mobj( $set -> Mobj( "bids" -> list ) ) )
    }

    token
  }

  def remove = T.web.res.deleteCookie( B.trackingCookieName )
}


object ActivityQuery extends Query {

  //def connections( run:Run ) =
    //run.cache.getOrElseUpdate( "connections", Connection.db.find( Mobj( "from" -> Session().user.org.id ) ).toSeq ).asInstanceOf[Seq[DBObject]]

  val entity = Log
  val name = "activity"

  override def newReport = {
    var r = super.newReport
    r.sort = Log.defaultSort
    r
  }

  val fields = Seq(
    new CustomField {
      val name = "hideOperators$cst"
      override val search = Search.Custom
      override lazy val label = "Hide " + B.applicationName + " Users"
      override def ui( s:Scope ) = Checkbox( id, s.rec.b( name ) )
      override def extract( s:Scope ) = s.rec( name ) = T.web.b( id )
    }
  )

  val defaultFields = dataFields.take( 5 )

  override val searchForm =
    ( r:Report ) => {
      val s = Scope( r.searchRec )

      <form method="post" action={ T.web.path } id="rSearchForm" style="padding-top:8px;">
       <div class="fieldsc" style="margin-top:8px; padding:4px;">
        <h3>Search By</h3>
        { searchFields.map { f =>
            <div class="fieldc">
             <div class="labelc">{ f.labelUi }</div>
             <div class="inputc">{ f.ui( s ) }</div>
            </div>
          }
        }
       </div>
       <div class="btns">
        <input type="submit" value="Search" class="go btn" name="saving"/>
       </div>
      </form>
    }
}

case class Browser( bid:String, ua:UserAgent, milestones:mutable.Set[Milestone] = mutable.Set[Milestone](), var skip:Boolean = false )

object Accesslet extends Weblet {

  def report:NodeSeq = {

    ActivityQuery.init
    val report = T.session.reportFor( ActivityQuery.name )

    if ( T.web.s( 'saving ).notBlank )
      report.extractSearchRec

    val hideOperators = report.searchRec.b( 'hideOperators$cst )

    val browsers        = mutable.Map[String,Browser]()
    val milestoneCounts = mutable.Map[Milestone,Int]( B.milestones.map( milestone => milestone -> 0 ):_* )

    def skip( l:Log ) =
      (   l.ua.orNull == null
       || (   hideOperators
           && B.operatorIps.contains( l.s( 'ip ) ) ) )

    for ( al <- Log.db.find( Mobj( "e" -> Event.Access.id, "bid" -> Mobj( $exists -> true ) ) ).sort( Mobj( "on" -> -1 ) ).map( Log.apply );
          if !skip( al ) ) {

      val bid = al.s( 'bid )


      val browser = browsers.getOrElseUpdate( bid, Browser( bid, al.ua.get ) )

      val uid = al.oid( 'uid )
      if ( hideOperators && uid != null && TidItem.by( B.User.idToTid( uid ) ).org == B.appOrgId ) {
        browser.skip = true
      } else {
        for ( milestone <- B.milestones )
          if ( !browser.milestones( milestone ) && milestone.satisfies( al ) )
            browser.milestones += milestone
      }
    }

    val userAgents = mutable.Map[UserAgent,Int]()

    for ( b <- browsers.values if !b.skip ) {
      for ( milestone <- b.milestones )
        milestoneCounts( milestone ) += 1

      b.ua.updateIfNeeded
      if ( !userAgents.contains( b.ua ) )
        userAgents( b.ua ) = 1
      else
        userAgents( b.ua ) += 1
    }

    val total = milestoneCounts( B.milestones( 0 ) )

    { ActivityQuery.searchForm( report ) } +:
    <div class="fieldhc">
     Milestones
    </div>
    <table class="dtable">
     <thead>
      <tr>
       <th>Milestone</th><th style="width:110px;"># Distinct Users</th><th style="width:50px;">%</th>
      </tr>
     </thead>
     { for ( milestone <- B.milestones ) yield {
         val count = milestoneCounts( milestone )

        <tr>
         <td>{ milestone.name }</td>
         <td>{ count }</td>
         <td>{ "%.0f%%".format( count._d * 100 / total ) }</td>
        </tr>
      }
     }
    </table>
    <div class="fieldhc">
     Browsers
    </div>
    <table class="dtable">
     <thead>
      <tr>
       <th style="width:26px; padding-left:0;"/><th>Agent</th><th>OS</th><th style="width:110px;"># Distinct Users</th><th style="width:50px;">%</th>
      </tr>
     </thead>
     { for ( ua <- userAgents.keys.filter( !_.bot ).toSeq.sortBy( _.s( 'agentName ) ) ) yield {
         val count = userAgents( ua )

        <tr>
         <td style="padding-left:0;">{ ua.eye }</td>
         <td>{ ua.agent }</td>
         <td>{ ua.os }</td>
         <td>{ count }</td>
         <td>{ "%.0f%%".format( count._d * 100 / total ) }</td>
        </tr>
      }
     }
    </table>
    <div class="fieldhc">
     Bots
    </div>
    <table class="dtable">
     <thead>
      <tr>
       <th style="width:26px; padding-left:0;"/><th>Agent</th><th>OS</th><th style="width:110px;"># Distinct Users</th><th style="width:50px;">%</th>
      </tr>
     </thead>
     { for ( ua <- userAgents.keys.filter( _.bot ).toSeq.sortBy( _.s( 'agentName ) ) ) yield {
         val count = userAgents( ua )

        <tr>
         <td style="padding-left:0;">{ ua.eye }</td>
         <td>{ ua.agent }</td>
         <td>{ ua.os }</td>
         <td>{ count }</td>
         <td>{ "%.0f%%".format( count._d * 100 / total ) }</td>
        </tr>
      }
     }
    </table>
  }

  def handle( web:WebContext ) = {

    if ( !T.user.isGod )
      _404

    rpath match {
    case "/" =>
      shell( report )

    case _ =>
      _404
    }
  }
}

