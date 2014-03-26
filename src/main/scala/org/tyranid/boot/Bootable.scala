/**
 * Copyright (c) 2008-2014 Tyranid <http://tyranid.org>
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

import javax.servlet.http.HttpServletRequest

import scala.xml.NodeSeq

//import org.clapper.classutil.ClassFinder

import com.braintreegateway.BraintreeGateway

import org.bson.types.ObjectId

import org.tyranid.Imp._
import org.tyranid.cloud.aws.{ Aws, S3Bucket }
import org.tyranid.cloud.stackdriver.StackDriverApp
import org.tyranid.content.Content
import org.tyranid.db.mongo.Imp._
import org.tyranid.document.DocApp
import org.tyranid.document.CloudConvertApp
import org.tyranid.document.crocodoc.CrocApp
import org.tyranid.document.dropbox.DropboxApp
import org.tyranid.document.issuu.IssuuApp
import org.tyranid.document.scribd.ScribdApp
import org.tyranid.document.zencoder.ZencoderApp
import org.tyranid.document.saaspose.SaasposeApp
import org.tyranid.db.mongo.MongoEntity
import org.tyranid.email.EmailTemplate
import org.tyranid.profile.{ Group, OrgMeta, User, UserMeta }
import org.tyranid.secure.{ AccessType, Multipass }
import org.tyranid.session.{ Milestone, Session, SessionData, ThreadData, SessionDataMeta }
import org.tyranid.sms.NexmoApp
import org.tyranid.social.TwApp
import org.tyranid.social.basecamp.BcApp
import org.tyranid.social.facebook.FbApp
import org.tyranid.social.google.GoApp
import org.tyranid.social.linkedin.LiApp
import org.tyranid.telco.TwilioApp
import org.tyranid.web.{ Weblet, Webloc, WebPath, WebContext }

object Boot {
  var TEST = false

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

        println( "Temp dir: " + System.getProperty( "java.io.tmpdir" ) )
        println( "*** " + instance.applicationName + " booted ... mode: " + instance.mode.toUpperCase + ", build: " + instance.build + " ***" )
      } catch {
      case e:ClassCastException =>
        throw new RuntimeException( "bootstrap.tyranid.Boot does not extend org.tyranid.boot.Boot" )

      case e:Throwable =>
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

  val liteAppName = "ReVu.Me"
  
  @volatile var SHUTTINGDOWN = false
  
  val liteDomainPart = "revu"
  val liteDomainName = "revu.me"
 
  def fullDomain = { 
    if ( DEV )        "rb-dev." + domain
    else if ( STAGE ) "rb-stage." + domain
    else if ( BETA )  "rb-beta." + domain
    else              "rb." + domain
  }
  
  def liteFullDomain = { 
    if ( DEV )        liteDomainPart + "-dev." + domain
    else if ( STAGE ) liteDomainPart + "-stage." + domain
    else if ( BETA )  liteDomainPart + "-beta." + domain
    else              liteDomainPart + "." + domain
  }
    
  def port = 
    if ( DEV )        ":8443"
    else              ""
      
  def domainPort = fullDomain + port 
    
  def nonsecureWebsite = "http://" + domainPort
    
  val bounceEmail = "support@" + domain 
  val alertEmail:String

  val systemEmail:String
  val systemUser:User
  val demoUser:User
  val publicGroup:Group

  val weblocs:Seq[Webloc]

  lazy val loginWebloc = weblocs.find( _.path == "/log" ).get

  val paths:Seq[WebPath]
  val milestones:Seq[Milestone]

  val emailTemplates:EmailTemplate

  def boot:Unit

  def loginListeners:Seq[ User => Unit ] = Nil
  def logoutListeners:Seq[ User => Unit ] = Nil
  def logoutJs = ""


   /*
   * * *   S e c u r i t y
   */

  val requireSsl = false
  
  var maintenanceMode = false

  def requireReCaptcha = TyranidConfig().b( 'recaptcha )
  def accessLogs       = TyranidConfig().b( 'accessLogs )
  def onePagePdf       = TyranidConfig().b( 'onePagePdf )
  def convIndd         = TyranidConfig().b( 'convIndd )
  def allTmpl          = TyranidConfig().b( 'allTmpl )
  def debugSso         = TyranidConfig().b( 'debugSso )
  def actOn            = TyranidConfig().b( 'actOn )
  def debugChat        = TyranidConfig().b( 'debugChat )
  def profile          = TyranidConfig().b( 'profile )
  def syncWebDav       = TyranidConfig().b( 'syncWebDav )
  def hideUpgradeBtn   = TyranidConfig().b( 'hideUpgradeBtn )
  def enableLite       = TyranidConfig().b( 'lite )

  def access( thread:ThreadData, accessType:AccessType, ref:AnyRef )

  def secureBypass( url:String ) = false

  def canAddUser( o:org.tyranid.profile.Org ):Boolean
  @volatile var onLogin: ( Session ) => Unit = null

  @volatile var newUser:() => User = null
  val userMeta:UserMeta
  val orgMeta:OrgMeta
  val sessionDataMeta:SessionDataMeta
  @volatile var User:UserMeta = null
  @volatile var Org:OrgMeta = null
  @volatile var SessionData:SessionDataMeta = null
  @volatile var Location:MongoEntity = null
  @volatile var newSession:()     => Session     = null
  @volatile var newSessionData:() => SessionData = null
  
  def welcomeUserEvent

  def ContentEntities:Seq[MongoEntity] = Nil
  def DocEntity:MongoEntity = null

  def finishConversion( content:Content )
  def addInvolvedPresentationData( req:HttpServletRequest )
  
  def registerUser( user:User, companyName:String ) 
  def sendMessage( msg:String, toUserTid:String, fromUserTid:String = null ) 

  lazy val appOrgId = Org.db.findOne( Mobj( "name" -> applicationName ) ).oid
  lazy val appOrgTid = Org.idToTid( appOrgId )

  val loginCookieName:String    = null
  val trackingCookieName:String = null

  lazy val hostName = InetAddress.getLocalHost.getHostName
  
  println( "hostname: " + hostName )
  lazy val DEV = hostName.indexOf( "macbook" ) != -1 || hostName.indexOf( "iMac" ) != -1 || hostName.indexOf( "imac" ) != -1 || hostName.indexOf( "-mac-" ) != -1 || hostName.indexOf( ".local" ) != -1 || hostName.indexOf( "dannys-mbp" ) != -1

  lazy val securityGroup =
    if ( DEV ) ""
    else       ( Aws.instanceQueryUrl + "/latest/meta-data/security-groups" ).GET().s

  lazy val BETA = false //!DEV && !STAGE && hostName.indexOf( "-beta" ) != -1

  lazy val PRODUCTION = !DEV && securityGroup == "mdb"

  lazy val STAGE = !DEV && !PRODUCTION

  lazy val ec2InstanceId = {
    if ( DEV ) ""
    else       ( Aws.instanceQueryUrl + "/latest/meta-data/instance-id" ).GET().s
  }
  
  def mode =
    if ( DEV )        "development"
    else if ( STAGE ) "stage"
    else if ( BETA )  "beta"
    else              "production"

  
  val build:Int

  lazy val buildPrefix = "/v" + build

  // Environment
  var envSuffix = "" // "-x" or "-dev"

  val serverTimeZone = java.util.TimeZone.getTimeZone( "CDT" )

  // DB
  val profileDbName:String
  val productName:String
  
  def mongoHost:String = null

  // SQL
  @volatile var dbUrl:String  = ""
  @volatile var dbUser:String = ""
  @volatile var dbPw:String   = ""
  @volatile var dbDriver      = "org.postgresql.Driver"

  // ElasticSearch
  def elasticSearchHost:String = "http://localhost:9200"
  
  // socket.io
  def socketIoHost:String = null

  // SMS
  val sms:NexmoApp = null

  // SSO stuff
  val saasId:String = null
  val pingIdentityUsername:String = null
  val pingIdentityPassword:String = null

  // CreatePDF
  val createPDFUsername:String = null
  val createPDFPassword:String = null

  // Social
  val facebook:FbApp      = null
  val linkedIn:LiApp      = null
  val google:GoApp        = null
  val twitter:TwApp       = null

  val twilio:TwilioApp    = null
  
  val basecamp:BcApp      = null
  
  // Document services
  val dropbox:DropboxApp = null
  val zencoder:ZencoderApp = null
  val crocodoc:CrocApp = null; val CROCODOC_SCRIPT:String = null; val CROC_JS_V2 = true 
  val scribd:ScribdApp = null
  val issuu:IssuuApp = null
  val saaspose:SaasposeApp = null
  val cloudConvert:CloudConvertApp = null
  
  // ReCaptcha
  val reCaptchaPublicKey  = ""
  val reCaptchaPrivateKey = ""

  val turboBridgeAccountId = ""
  val turboBridgeEmail = ""
  val turboBridgePassword = ""

  val stackdriver:StackDriverApp = null

  // Assistly
  val assistly:Multipass  = null

  // Braintree
  @volatile var braintreeGateway:BraintreeGateway = null
  @volatile var braintreeMerchantId:String = null

  // PDF Crowd
  val pdfCrowdName:String = ""
  val pdfCrowdKey:String = ""
    
  val convertApiKey:String = ""
  
  // AWS
  val awsCredentials:com.amazonaws.auth.AWSCredentials = null
  val bucketSuffix:String = ""
  val documentEntity:MongoEntity   
    
  val domainRoute53ZoneId = ""

  val filesBucket:S3Bucket
  
  val docPreviewApp:DocApp
  
  private val s3Buckets = scala.collection.mutable.Map[String,S3Bucket]()

  def getS3BucketByFullName( name:String ): S3Bucket = s3Buckets.values.find( bucket => bucket.name == name ).getOrElse( null )
  
  def getS3Bucket( prefix:String ): S3Bucket = s3Buckets( prefix + envSuffix ) 

  def bucketByUrl( url:String ) = s3Buckets.values.find( bucket => {
      val cfUrl = bucket.url( "" )
      val nonCfUrl = bucket.url( "", true )
      url.startsWith( cfUrl ) || url.startsWith( nonCfUrl )
    }
  )
  
  def bucket( buckets:S3Bucket* ) =
    for ( bucket <- buckets )
      s3Buckets( bucket.prefix ) = bucket

  def groupContents( group:Group ):Seq[Content] = Nil
 
  //def initEntities {
    //val cl = Thread.currentThread.getContextClassLoader
    //val urls = cl.as[java.net.URLClassLoader].getURLs.map( _.getFile ).filter( !_.endsWith( ".jar" ) )

    //val finder = ClassFinder( urls.map( new File(_) ) )
    //val infoMap = ClassFinder.classInfoMap( finder.getClasses )

    //ClassFinder.concreteSubclasses( "org.tyranid.db.Entity", infoMap ).foreach { c =>
      //if ( !c.name.contains( ".test." ) ) Class.forName( c.name )
    //}

    //org.tyranid.db.Entity.init
  //}
  
  // Security
  java.security.Security.addProvider( new org.bouncycastle.jce.provider.BouncyCastleProvider() )
}
