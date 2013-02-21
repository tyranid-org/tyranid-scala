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

import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.db.Scope
import org.tyranid.db.meta.TidItem
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.MongoRecord
import org.tyranid.http.UserAgent
import org.tyranid.log.Log
import org.tyranid.math.Base62
import org.tyranid.net.DnsDomain
import org.tyranid.profile.User
import org.tyranid.report.{ Query, Report }
import org.tyranid.ui.{ Checkbox, CustomField, PathField, Search }
import org.tyranid.web.{ Weblet, WebContext, WebFilter, WebPath }


object Milestone {

  def apply( id:String ) = B.milestones.find( _.id == id )

}

case class Milestone( name:String, satisfies:( Log ) => Boolean ) {
  lazy val id = Base62.make( 4 )
}



object AccessLog {

  lazy val paths = {
    val map = mutable.Map[String,WebPath]()

    for ( path <- B.paths )
      map( path.path ) = path

    map
  }

  def qsMobjFor( web:WebContext ):DBObject = {

    paths.get( web.path ) foreach { path =>
      val params = Mobj()

      for ( param <- path.logParams ) {
        // TODO:  handle arrays ?
        params( param ) = web.s( param )
      }

      return params
    }

    null
  }

  def log( web:WebContext, thread:ThreadData, durationMs:Long ) {
    val session = thread.session

    if ( B.accessLogs ) {
      web.path match {
      case "/cometd" => // ignore
      case p if !WebFilter.notAsset( p ) => // ignore
      case p         =>
        Log.log( Event.Access,
                 "p"   -> p, 
                 "bid" -> TrackingCookie.get,
                 "ua"  -> web.req.getHeader( "User-Agent" ),
                 "du"  -> durationMs,
                 "d"   -> DnsDomain.idFor( web.req.getServerName.stripPrefix( "www." ).stripXss ),
                 "qs"  -> qsMobjFor( web )
               )
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
      cookie.setSecure( true )
      t.web.res.addCookie( cookie )
    }

    val u = t.user
    val tokens = u.a_?( 'bids ).toSeq.of[String]

    if ( !tokens.contains( token ) ) {
      val list = ( tokens :+ token ).toMlist
      u( 'bids ) = list
      B.User.db.update( Mobj( "_id" -> u.id ), Mobj( $set -> Mobj( "bids" -> list ) ) )
    }

    //if ( tokens.size > 12 )
      //consolidate( u )

    token
  }

  def remove = T.web.res.deleteCookie( B.trackingCookieName )

  def consolidate( user:User ) {

    // this isn't right ... this will consolidate multiple real users into the same browser id if two different people ever log into the same account
    // 
    // taking IP into account would help a little, but still not solve it since people from the same location share the same IP

    val bids = user.a_?( 'bids )
    if ( bids.size <= 1 )
      return

    val main = bids( 0 )

    for ( bid <- bids if bid != main ) {

      Log.db.update(
        Mobj( "bid" -> bid ),
        Mobj( "$set" -> Mobj( "bid" -> main ) ),
        false,
        true )

      for ( u <- B.User.db.find( Mobj( "bids" -> bid ) ).map( B.User.apply );
            if u.tid != user.tid ) {
        val ubids = u.a_!( 'bids )
        ubids.remove( bid )
        if ( !ubids.contains( main ) )
          ubids.add( main )
        u.save
      }
    }

    user( 'bids ) = Mlist( main )
    user.save
  }
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
    },
    PathField( "d",                                 search = Search.Equals ),
    PathField( "on", l = "From Date", data = false, search = Search.Gte,   default = Some( () => "last week".toLaxUserDateTime ) ),
    PathField( "on", l =   "To Date", data = false, search = Search.Lte    )
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
        <input type="submit" value="Search" class="btn-success btn" name="saving"/>
       </div>
      </form>
    }
}

case class Browser( bid:String,
                    ua:UserAgent,
                    milestones:mutable.Map[Milestone,Int] = mutable.Map[Milestone,Int](),
                    users:mutable.Set[TidItem]            = mutable.Set[TidItem](),
                    var skip:Boolean                      = false,
                    var domainFound:Boolean               = false ) {

  def hasMilestone( milestone:Milestone )   = milestoneCount( milestone ) > 0
  def milestoneCount( milestone:Milestone ) = milestones.getOrElse( milestone, 0 )
  def addMilestone( milestone:Milestone )   = milestones( milestone ) = milestoneCount( milestone ) + 1
}

case class Path( path:String,
                 var requests:Int = 0,
                 var ms:Long      = 0 )

case class MilestoneCounts( var distinct:Int = 0, var total:Int = 0 )

object Accesslet extends Weblet {

  def report:NodeSeq = {
    val web = T.web

    ActivityQuery.init
    val report = T.session.reportFor( ActivityQuery.name )

    if ( web.s( 'saving ).notBlank )
      report.extractSearchRec

    val hideOperators = report.searchRec.b( 'hideOperators$cst )
    val domain        = report.searchRec( 'd )
    val dateGte       = report.searchRec( 'on$gte )
    val dateLte       = report.searchRec( 'on$lte )

    val browsers        = mutable.Map[String,Browser]()
    val paths           = mutable.Map[String,Path]()
    val milestoneCounts = mutable.Map[Milestone,MilestoneCounts]( B.milestones.map( _ -> MilestoneCounts() ):_* )

    val onlyMilestone = web.sOpt( "milestone" ).flatMap( Milestone.apply )
    val milestones    = onlyMilestone.flatten( Seq(_), B.milestones )


    def skipLog( l:Log ) =
      (   (   l.ua.orNull == null
           && l.e != Event.NewInvite )
       || (   hideOperators
           && B.operatorIps.contains( l.s( 'ip ) ) ) )

    def skipBrowser( b:Browser ) =
      b.skip ||
      ( domain != null && !b.domainFound ) ||
      ( onlyMilestone.isDefined && !b.hasMilestone( onlyMilestone.get ) )


    val query = Mobj( "e" -> Mobj( $in -> Mlist( Event.Access.id, Event.NewInvite.id ) ) )
    if ( dateGte != null || dateLte != null ) {
      val q = Mobj()
      if ( dateGte != null )
        q( $gte ) = dateGte
      if ( dateLte != null )
        q( $lte ) = dateLte
      query( "on" ) = q
    }

    for ( al <- Log.db.find( query ).sort( Mobj( "on" -> -1 ) ).map( Log.apply );
          if !skipLog( al ) ) {

      var bid = al.s( 'bid )
      if ( bid.isBlank )
        bid = B.User.idToTid( al.s( 'uid ) )

      val ua = al.ua.getOrElse( UserAgent.system )
      val browser = browsers.getOrElseUpdate( bid, Browser( bid, ua ) )

      val uid = al.oid( 'uid )
      val user = if ( uid != null ) TidItem.by( B.User.idToTid( uid ) ) else null

      if ( hideOperators && user != null && user.org == B.appOrgId ) {
        browser.skip = true
      } else {
        for ( milestone <- B.milestones )
          if ( !browser.ua.bot && milestone.satisfies( al ) )
            browser.addMilestone( milestone )

        if ( user != null )
          browser.users += user

        if ( domain != null && al( 'd ) == domain )
          browser.domainFound = true
      }

      val p = al.s( 'p )
      val path = paths.getOrElseUpdate( p, new Path( p ) )
      path.requests += 1
      path.ms += al.l( 'du )
    }

    val userAgents = mutable.Map[UserAgent,Int]()

    for ( b <- browsers.values if !skipBrowser( b ) ) {

      for ( milestonePair <- b.milestones;
            mc = milestoneCounts( milestonePair._1 ) ) {
        mc.distinct += 1
        mc.total += milestonePair._2
      }

      val ua = b.ua
      
      if ( ua != null ) {
        ua.updateIfNeeded
        
        if ( !userAgents.contains( ua ) )
          userAgents( ua ) = 1
        else
          userAgents( ua ) += 1
      }
    }

    val totalUsers  = milestoneCounts( B.milestones( 0 ) ).distinct
    val totalTotals = milestoneCounts.map( _._2.total ).sum
    val totalBots   = browsers.values.count( _.ua.bot )


    { ActivityQuery.searchForm( report ) } ++
    { if ( onlyMilestone.isDefined )
    <div class="fieldhc">
     Milestone: { onlyMilestone.get.name }
    </div>
    <div class="fieldhc">
     Users
    </div>
    <table class="dtable">
     <thead>
      <tr>
       <th style="width:120px;">Browser ID</th><th>User</th>
      </tr>
     </thead>
     { for ( b <- browsers.values if !skipBrowser( b ) ) yield {

        <tr>
         <td><a href={ "/admin/log?bid=" + b.bid }>{ b.bid }</a></td>
         <td>{ b.users.map( _.name ).toSeq.sorted.mkString( ", " ) }</td>
        </tr>
      }
     }
    </table>
      else
    <div class="fieldhc">
     Milestones
    </div>
    <table class="dtable">
     <thead>
      <tr>
       <th>Milestone</th>
       <th style="width:110px;"># Distinct Users</th>
       <th style="width:90px;">% Distinct</th>
       <th style="width:80px;">Total</th>
       <th style="width:60px;">% Total</th>
      </tr>
     </thead>
     { for ( milestone <- milestones ) yield {
         val count = milestoneCounts( milestone )

        <tr>
         <td><a href={ "?milestone=" + milestone.id }>{ milestone.name }</a></td>
         <td>{ count.distinct }</td>
         <td>{ "%.0f%%".format( count.distinct._d * 100 / totalUsers ) }</td>
         <td>{ count.total }</td>
         <td>{ "%.0f%%".format( count.total._d * 100 / totalTotals ) }</td>
        </tr>
      }
     }
    </table>
    } ++
    <div class="fieldhc">
     Browsers
    </div>
    <table class="dtable">
     <thead>
      <tr>
       <th style="width:26px; padding-left:0;"/><th>Agent</th><th>OS</th><th style="width:110px;"># Distinct Users</th><th style="width:90px;">% Distinct</th>
      </tr>
     </thead>
     { for ( ua <- userAgents.keys.filter( !_.bot ).toSeq.sortBy( _.s( 'agentName ) ) ) yield {
         val count = userAgents( ua )

        <tr>
         <td style="padding-left:0;">{ ua.eye }</td>
         <td>{ ua.agent }</td>
         <td>{ ua.os }</td>
         <td>{ count }</td>
         <td>{ "%.0f%%".format( count._d * 100 / totalUsers ) }</td>
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
       <th style="width:26px; padding-left:0;"/><th>Agent</th><th style="width:110px;"># Distinct Bots</th><th style="width:50px;">%</th>
      </tr>
     </thead>
     { for ( ua <- userAgents.keys.filter( _.bot ).toSeq.sortBy( _.s( 'agentName ) ) ) yield {
         val count = userAgents( ua )

        <tr>
         <td style="padding-left:0;">{ ua.eye }</td>
         <td>{ ua.agent }</td>
         <td>{ count }</td>
         <td>{ "%.0f%%".format( count._d * 100 / totalBots ) }</td>
        </tr>
      }
     }
    </table>
    <div class="fieldhc">
     Paths
    </div>
    <table class="dtable">
     <thead>
      <tr>
       <th>Path</th><th style="width:100px;"># Requests</th><th style="width:100px;">Total Time (ms)</th><th style="width:100px;">Avg Time (ms)</th>
      </tr>
     </thead>
     { for ( path <- paths.values.toSeq.sortBy( -_.ms ) ) yield {

        <tr>
         <td>{ path.path }</td>
         <td>{ path.requests }</td>
         <td>{ path.ms }</td>
         <td>{ path.ms / path.requests }</td>
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

