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
import scala.xml.{ NodeSeq, Unparsed }

import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.db.Scope
import org.tyranid.db.meta.TidItem
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.MongoRecord
import org.tyranid.http.UserAgent
import org.tyranid.log.Log
import org.tyranid.json.JqHtml
import org.tyranid.math.Base62
import org.tyranid.net.DnsDomain
import org.tyranid.profile.User
import org.tyranid.report.{ Query, Report }
import org.tyranid.ui.{ Checkbox, CustomField, PathField, Search }
import org.tyranid.web.{ Weblet, WebContext, WebFilter, WebPath }


object Milestone {

  def apply( id:String ) = B.milestones.find( _.id == id )

  def size = nextIdx

  @volatile private var nextIdx = 0
}

case class Milestone( name:String, satisfies:( Log ) => Boolean ) {
  lazy val id = Base62.make( 4 )

  val idx = {
    val i = Milestone.nextIdx
    Milestone.nextIdx += 1
    i
  }
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
        val s = web.s( param )

        // TODO:  handle arrays ?

        if ( s.notBlank )
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

/**
 * This manages Browser IDs.
 */
object TrackingCookie {

  def get:String = {
    val t = T

    if ( t.web == null )
      return null;

    var token = t.web.req.cookieValue( B.trackingCookieName )
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

  //def remove = T.web.res.deleteCookie( B.trackingCookieName )

  def mergeUser( user:User ) {

    def isOperator( user:User ) = user.email.endsWith( B.domain )

    val userOperator = isOperator( user )

    case class BidInfo( bid:String, users:Seq[User], operator:Boolean ) {

      lazy val hasOperatorIpLog = {
        val log = Log( Log.db.findOne( Mobj( "bid" -> bid ), null, Mobj( "on" -> -1 ) ) )
        log != null && B.operatorIps.contains( log.s( 'ip ) )
      }

      lazy val canCollapse = users.size == 1 && ( userOperator || ( !operator && !hasOperatorIpLog ) )
    }


    val bids =  user.a_?( 'bids )
    if ( bids.size < 2 )
      return

    val bis = user.a_?( 'bids ).map { rBid =>
      val bid = rBid._s
      val users = B.User.db.find( Mobj( "bids" -> bid ) ).map( B.User.apply ).toSeq

      BidInfo( bid, users, operator = users.exists( isOperator ) )
    }


    /*

        I.  try to collapse multi-user BIDs (might not be an issue)

            issues when dealing with collapsing BIDs that have more than one user:

              this will consolidate multiple real users into the same browser id if two different people ever log into the same account
              taking IP into account would help a little, but still not solve it since people from the same location share the same IP
              remove all browser ids that are also volerro users

     */

    println( "BID Merge:  Analyzing " + user.label + " (" + bis.size + " bids) ..." )
    bis.find( _.canCollapse ) foreach { main =>
      val collapsing = bis.filter( bi => ( bi ne main ) && bi.canCollapse )

      if ( collapsing.nonEmpty ) {
        print( "BID Merge:  Collapsing " + collapsing.size + " bids into " + main.bid + " for " + user.label + "..." )

        Log.db.update(
          Mobj( "bid" -> Mobj( $in -> collapsing.map( _.bid ).toMlist ) ),
          Mobj( "$set" -> Mobj( "bid" -> main.bid ) ),
          false,
          true )

        /*

          We're only collapsing bids that are used by a single user, so this isn't needed yet

        for ( u <- B.User.db.find( Mobj( "bids" -> bid ) ).map( B.User.apply );
              if u.tid != user.tid ) {
          val ubids = u.a_!( 'bids )
          ubids.remove( bid )
          if ( !ubids.contains( main ) )
            ubids.add( main )
          u.save
        }
         */


        for ( bi <- collapsing )
          user.a_?( 'bids ).remove( bi.bid )

        B.User.db.update( Mobj( "_id" -> user.id ), Mobj( $set -> Mobj( "bids" -> user.a_?( 'bids ) ) ) )

        println( "done." )
      }
    }
  }

  def merge {

    for ( user <- B.User.records )
      TrackingCookie.mergeUser( user )

    println( "BID Merge:  Completed." )
  }
}


object ActivityQuery extends Query {

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
      override def ui( user:User, s:Scope ) = Checkbox( id, s.rec.b( name ) )
      override def extract( s:Scope ) = s.rec( name ) = T.web.b( id )
      override val default = Some( () => true )
    },
    PathField( "on", l = "From Date", data = false, search = Search.Gte,   default = Some( () => "last week".toLaxUserDateTime ) ),
    PathField( "on", l =   "To Date", data = false, search = Search.Lte    )
  )

  val defaultFields = dataFields.take( 5 )

  override def searchForm( user:User, r:Report ) = {
    val s = Scope( r.searchRec )

    <form method="post" action="" id="rSearchForm" style="padding-top:8px;" class="handling">
     <div class="fieldsc" style="margin-top:8px; padding:4px;">
      <h3>Search By</h3>
      { searchFields.map { f =>
        <div class="fieldc">
         <div class="labelc">{ f.labelUi }</div>
         <div class="inputc">{ f.ui( user, s ) }</div>
        </div>
      } }
     </div>
     <div class="btns">
      <input type="button" value="Search" class="btn-success btn" name="saving"/>
     </div>
    </form>
  }
}


trait MilestoneCounter {
  val milestoneCounts:Array[Int] = new Array( Milestone.size )

  def hasMilestone( milestone:Milestone )   = milestoneCounts( milestone.idx ) > 0
  def milestoneCount( milestone:Milestone ) = milestoneCounts( milestone.idx )
  def addMilestone( milestone:Milestone )   = milestoneCounts( milestone.idx ) += 1

  lazy val totalMilestones = milestoneCounts.sum
}

case class Browser( bid:String,
                    ua:UserAgent,
                    users:mutable.Set[TidItem] = mutable.Set[TidItem](),
                    var skip:Boolean           = false )
  extends MilestoneCounter

case class UserData( user:TidItem )
  extends MilestoneCounter

case class Path( path:String,
                 var requests:Int = 0,
                 var ms:Long      = 0 )

case class MilestoneCounts( var distinct:Int = 0, var total:Int = 0 )

object Accesslet extends Weblet {

  def report:NodeSeq = {
    val web = T.web
    val user = T.user

    ActivityQuery.init
    val report = T.session.reportFor( ActivityQuery.name )

    if ( web.s( 'saving ).notBlank )
      report.extractSearchRec

    val hideOperators = report.searchRec.b( 'hideOperators$cst )
    val dateGte       = report.searchRec( 'on$gte )
    val dateLte       = report.searchRec( 'on$lte )

    var browsers        = mutable.Map[String,Browser]()
    var users           = mutable.Map[String,UserData]() // the string is a user tid
    val paths           = mutable.Map[String,Path]()
    val milestoneCounts = mutable.Map[Milestone,MilestoneCounts]( B.milestones.map( _ -> MilestoneCounts() ):_* )

    val onlyMilestone = web.sOpt( "milestone" ).flatMap( Milestone.apply )
    val milestones    = onlyMilestone.flatten( Seq(_), B.milestones )


    //
    // Query Raw Log Data, generating Browsers and performing basic counts
    //

    val query = Mobj( "e" -> Mobj( $in -> Mlist( Event.Access.id, Event.NewInvite.id, Event.Login.id ) ) )
    if ( dateGte != null || dateLte != null ) {
      val q = Mobj()
      if ( dateGte != null )
        q( $gte ) = dateGte
      if ( dateLte != null )
        q( $lte ) = dateLte
      query( "on" ) = q

      if ( hideOperators )
        query( "ip" ) = Mobj( $nin -> B.operatorIps.toMlist )
    }

    def skipLog( l:Log ) =
      (   l.ua.orNull == null
       && l.e == Event.Access )

    def bidFor( l:Log ):String = {
      val bid = l.s( 'bid )
      if ( bid.notBlank )
        return bid

      val uid = l.s( 'uid )
      if ( uid.notBlank )
        return B.User.idToTid( uid )

      null 
    }

    for ( al <- Log.db.find( query ).sort( Mobj( "on" -> -1 ) ).map( Log.apply );
          if !skipLog( al );
          bid = bidFor( al );
          if bid != null ) {

      val ua = al.ua.getOrElse( UserAgent.system )
      val browser = browsers.getOrElseUpdate( bid, Browser( bid, ua ) )

      val uid = al.oid( 'uid )
      val user = if ( uid != null ) TidItem.by( B.User.idToTid( uid ) ) else null

      if ( hideOperators && user != null && user.org == B.appOrgId ) {
        browser.skip = true
      } else {
        for ( milestone <- B.milestones ) {
          if ( !browser.ua.bot && milestone.satisfies( al ) ) {
            browser.addMilestone( milestone )

            if ( user != null )
              users.getOrElseUpdate( user.tid, UserData( user ) ).addMilestone( milestone )
          }
        }

        if ( user != null )
          browser.users += user
      }

      val p = al.s( 'p )
      val path = paths.getOrElseUpdate( p, new Path( p ) )
      path.requests += 1
      path.ms += al.l( 'du )
    }


    //
    // Filter out Browsers we don't care about
    //

    browsers = browsers.filter { entry =>
      val b = entry._2
     
      (   !b.skip
       && ( !onlyMilestone.isDefined || b.hasMilestone( onlyMilestone.get ) ) )
    }


    //
    // Calculate Milestone Counts
    //

    for ( m <- milestones ) {
      val mc = milestoneCounts( m )

      val users = mutable.Set[TidItem]()
      var userlessBrowsers:Int = 0

      for ( b <- browsers.values;
            mCount = b.milestoneCount( m );
            if mCount > 0 ) {
        val bUsers = b.users

        if ( bUsers.size > 0 )
          users ++= bUsers
        else
          userlessBrowsers += 1

        mc.total += mCount
      }

      mc.distinct = users.size + userlessBrowsers
    }


    //
    // Calculate User Agent Counts
    //

    val userAgents = mutable.Map[UserAgent,Int]()

    for ( b <- browsers.values ) {

      val ua = b.ua
      
      if ( ua != null ) {
        ua.updateIfNeeded
        
        if ( !userAgents.contains( ua ) )
          userAgents( ua ) = 1
        else
          userAgents( ua ) += 1
      }
    }


    //
    // Calculate Totals
    //

    val totalUsers        = B.User.db.count()

    val totalActiveUsers  = milestoneCounts( B.milestones( 0 ) ).distinct
    val totalTotals       = milestoneCounts.map( _._2.total ).sum
    val totalBots         = browsers.values.count( _.ua.bot )


    //
    // Report the Data
    //

    { ActivityQuery.searchForm( user, report ) } ++
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
       <th style="width:120px;">Company</th>
       <th style="width:120px;">User</th>
       <th>Browsers / Sessions</th>
      </tr>
     </thead>
     {
       val browsersByUsers = browsers.values.groupBy { b =>

         val users = b.users

         val sb = new StringBuilder

         val singleOrg = users.nonEmpty && users.head.org != null && users.forall( _.org == users.head.org )
         if ( singleOrg )
           sb ++= users.head.orgLabel
         else
           sb ++= "aaaaa"

         sb += '|'

         sb ++= b.users.toSeq.sortBy( _.name.toLowerCase ).map( _.name ).mkString( ", " )

         sb.toString
       }

       for ( key <- browsersByUsers.keys.toSeq.sortBy( _.toLowerCase ) ) yield {
         val browsers = browsersByUsers( key )

         val users = browsers.head.users.toSeq // they're all the same, so we can grab the first one

         val singleOrg = users.nonEmpty && users.head.org != null && users.forall( _.org == users.head.org )


         <tr>
          <td>{
            singleOrg |* <a href={ "#admin/tid/" + users.head.orgTid }>{ users.head.orgLabel }</a>
          }</td>
          <td>{ Unparsed( users.sortBy( _.name.toLowerCase ).map( u => "<a href=\"#admin/tid/" + u.tid + "\">" + u.name + "</a>" ).mkString( ", " ) ) }</td>
          <td>{ Unparsed( browsers.map( b => "<a href=\"/admin/log?bid=" + b.bid + "\">" + b.bid + "</a>" ).mkString( ", " ) ) }</td>
         </tr>
       }
     }
    </table>
      else
    <div class="fieldhc">
     Total { B.applicationName } Users: { totalUsers }
    </div>
    <div class="fieldhc">
     Milestones
    </div>
    <table class="dtable">
     <thead>
      <tr>
       <th/>
       <th colspan="3">Distinct Users</th>
       <th colspan="2">Events</th>
      </tr>
      <tr>
       <th>Milestone</th>
       <th style="width:110px;">#</th>
       <th style="width:90px;">% Active</th>
       <th style="width:90px;">% Total</th>
       <th style="width:80px;">Total</th>
       <th style="width:60px;">% Total</th>
      </tr>
     </thead>
     { for ( milestone <- milestones ) yield {
         val count = milestoneCounts( milestone )

        <tr>
         <td><a href={ "#admin/activity/" + milestone.id /*"?milestone=" + milestone.id */ }>{ milestone.name }</a></td>
         <td>{ count.distinct }</td>
         <td>{ "%.0f%%".format( count.distinct._d * 100 / totalActiveUsers ) }</td>
         <td>{ "%.0f%%".format( count.distinct._d * 100 / totalUsers ) }</td>
         <td>{ count.total }</td>
         <td>{ "%.0f%%".format( count.total._d * 100 / totalTotals ) }</td>
        </tr>
      }
     }
    </table>
    } ++
    <div class="fieldhc">
     Users
    </div>
    <table class="dtable">
     <thead>
      <tr>
       <th style="width:26px; padding-left:0;">Org</th>
       <th style="width:26px; padding-left:0;">User</th>
       { for ( m <- milestones ) yield
         <th>{ m.name }</th> }
      </tr>
     </thead>
     { 
       for ( userData <- users.values.toSeq.sortBy( -_.totalMilestones );
             u = userData.user ) yield
         <tr>
          <td>{ u.org != null |* <a href={ "#admin/tid/" + u.orgTid }>{ u.orgLabel }</a> }</td>
          <td><a href={ "#admin/tid/" + u.tid }>{ u.label }</a></td>
          { for ( milestone <- milestones ) yield
              <td>{ userData.milestoneCount( milestone ) }</td> }
         </tr>
     }
    </table>
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
          <td>{ "%.0f%%".format( count._d * 100 / totalActiveUsers ) }</td>
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
      web.jsRes( JqHtml( "#adminContent", report ) )

    case _ =>
      _404
    }
  }
}

