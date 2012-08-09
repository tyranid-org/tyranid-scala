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

package org.tyranid.boot

import java.io.File
import java.net.InetAddress

import scala.xml.NodeSeq

import org.cometd.bayeux.server.BayeuxServer
import org.clapper.classutil.ClassFinder

import com.braintreegateway.BraintreeGateway

import org.bson.types.ObjectId

import org.tyranid.Imp._
import org.tyranid.db.Entity
import org.tyranid.db.mongo.Imp._
import org.tyranid.document.crocodoc.CrocApp
import org.tyranid.document.issuu.IssuuApp
import org.tyranid.document.scribd.ScribdApp
import org.tyranid.db.mongo.MongoEntity
import org.tyranid.email.EmailTemplate
import org.tyranid.profile.{ Group, OrgMeta, User, UserMeta }
import org.tyranid.secure.{ AccessType, Multipass }
import org.tyranid.session.{ Milestone, Session, ThreadData }
import org.tyranid.sms.NexmoApp
import org.tyranid.social.{ TrackurApp, TwApp }
import org.tyranid.social.basecamp.BcApp
import org.tyranid.social.facebook.FbApp
import org.tyranid.social.google.GoApp
import org.tyranid.social.linkedin.LiApp
import org.tyranid.web.{ Weblet, Webloc, CometService }

object Boot {

  @volatile var instance:Bootable = _

  def boot = synchronized {

    if ( instance == null ) {
      try {
        val cls = Class.forName( "bootstrap.tyranid.Boot" )
        if ( cls == null )
          throw new RuntimeException( "Could not locate bootstrap.tyranid.Boot" )

        val boot = cls.newInstance
        if ( boot == null )
          throw new RuntimeException( "Could not instantiate bootstrap.tyranid.Boot" )

        instance = boot.asInstanceOf[Bootable]

        instance.boot

        println( "*** " + instance.applicationName + " booted ... mode: " + instance.mode.toUpperCase + ", build: " + instance.build + " ***" )

      } catch {
      case e:ClassCastException =>
        throw new RuntimeException( "bootstrap.tyranid.Boot does not extend org.tyranid.boot.Boot" )

      case e =>
        e.printStackTrace
        throw new RuntimeException( "could not instantiate bootstrap.tyranid.Boot" )
      }
    }
  }
}


/**
 * This is the tyranid bootstrap/configuration file.  Your instance should be:
 *
 * 1)  located in bootstrap.tyranid.Boot,
 * 2)  be an object named "Boot",
 * 3)  extend this trait.
 */
trait Bootable {

  val applicationName:String

  val domain:String
  
  val operatorIps = Seq[String]()

  // DEV assumes the DNS is in your hosts file
  
  lazy val fullDomain = 
    if ( DEV )        "dev." + domain
    else if ( STAGE ) "stage." + domain
    else              "app." + domain
    
  lazy val domainPort = 
    if ( DEV )        fullDomain + ":8443"
    else              fullDomain
    

  lazy val nonsecureWebsite = "http://" + domainPort
  
  val bounceEmail = "support@" + domain 
  val systemEmail:String
  val alertEmail:String

  val weblocs:Seq[Webloc]

  val applicationWebloc:Webloc
  lazy val loginWebloc = weblocs.find( _.path == "/log" ).get

  val milestones:Seq[Milestone]

  val templates:List[(String, ( NodeSeq ) => NodeSeq )]
  val emailTemplates:EmailTemplate

  val comets:Seq[CometService]

  @volatile var bayeux:BayeuxServer = null

  def boot:Unit


  /*
   * * *   S e c u r i t y
   */

  val requireSsl = false

  def requireReCaptcha = TyranidConfig().b( 'recaptcha )
  def accessLogs       = TyranidConfig().b( 'accessLogs )

  def access( thread:ThreadData, accessType:AccessType, ref:AnyRef )

  def secureBypass( url:String ) = false


  @volatile var newUser:() => User = null
  val userMeta:UserMeta
  @volatile var User:UserMeta = null
  @volatile var Org:OrgMeta = null
  @volatile var Location:MongoEntity = null
  @volatile var newSession:() => Session = null

  lazy val appOrgId = Org.db.findOne( Mobj( "name" -> applicationName ) ).oid
  lazy val appOrgTid = Org.idToTid( appOrgId )

  val loginCookieName:String    = null
  val trackingCookieName:String = null

  lazy val hostName = InetAddress.getLocalHost.getHostName

  lazy val DEV = hostName.indexOf( "macbook" ) != -1 || hostName.indexOf( "iMac" ) != -1 || hostName.indexOf( "imac" ) != -1 || hostName.indexOf( "-mac-" ) != -1 || hostName.indexOf( ".local" ) != -1
  lazy val STAGE = !DEV && hostName.indexOf( "-x" ) != -1
  lazy val PRODUCTION = !( DEV || STAGE )

  def mode =
    if ( DEV )        "development"
    else if ( STAGE ) "stage"
    else              "production"

  
  val build:Int

  lazy val buildPrefix = "/v" + build

  // Environment
  var envSuffix = "" // "-x" or "-dev"

  val serverTimeZone = java.util.TimeZone.getTimeZone( "CDT" )

  // DB
  val profileDbName:String

  @volatile var mongoHost:String = null

  // SQL
  @volatile var dbUrl:String  = ""
  @volatile var dbUser:String = ""
  @volatile var dbPw:String   = ""
  @volatile var dbDriver      = "org.postgresql.Driver"

  // SMS
  val sms:NexmoApp = null

  // Social
  val facebook:FbApp      = null
  val linkedIn:LiApp      = null
  val google:GoApp        = null
  val twitter:TwApp       = null
  val trackur:TrackurApp  = null

  val basecamp:BcApp      = null
  
  // Document services
  val crocodoc:CrocApp = null
  val scribd:ScribdApp = null
  val issuu:IssuuApp = null
  
  // ReCaptcha
  val reCaptchaPublicKey  = ""
  val reCaptchaPrivateKey = ""

  // Assistly
  val assistly:Multipass  = null

  // Braintree
  @volatile var braintreeGateway:BraintreeGateway = null
  @volatile var braintreeMerchantId:String = null

  // PDF Crowd
  val pdfCrowdName:String = ""
  val pdfCrowdKey:String = ""
  
  // AWS
  import org.tyranid.cloud.aws.S3Bucket

  val awsCredentials:com.amazonaws.auth.AWSCredentials = null
  val bucketSuffix:String = ""

  private val s3Buckets = scala.collection.mutable.Map[String,S3Bucket]()

  def getS3Bucket( prefix:String ): S3Bucket = s3Buckets( prefix + envSuffix ) 
  
  def bucket( buckets:S3Bucket* ) =
    for ( bucket <- buckets )
      s3Buckets( bucket.prefix ) = bucket

  // Groups
  lazy val commonGroupField:org.tyranid.profile.GroupField = null
  def defaultFavorites:Seq[ObjectId] = Seq()
  def availableFavorites:Seq[Group]  = Seq()

  def groupMemberAdded( group:Group, tid:String )   {}
  def groupMemberRemoved( group:Group, tid:String ) {}
 

  def initEntities {
    val cl = Thread.currentThread.getContextClassLoader
    val urls = cl.as[java.net.URLClassLoader].getURLs.map( _.getFile ).filter( !_.endsWith( ".jar" ) )

    val finder = ClassFinder( urls.map( new File(_) ) )
    val infoMap = ClassFinder.classInfoMap( finder.getClasses )

    ClassFinder.concreteSubclasses( "org.tyranid.db.Entity", infoMap ).foreach { c =>
      if ( !c.name.contains( ".test." ) ) Class.forName( c.name )
    }

    org.tyranid.db.Entity.init
  }
}
