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

package org.tyranid.social.linkedin

import javax.servlet.http.Cookie

import scala.annotation.tailrec
import scala.collection.mutable
import scala.xml.Unparsed

import org.bson.types.ObjectId
import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.cloud.aws.S3
import org.tyranid.company.Industry
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.MongoEntity
import org.tyranid.email.Email
import org.tyranid.http.Http403Exception
import org.tyranid.io.File
import org.tyranid.locale.{ Country, LocationType, Region }
import org.tyranid.net.Uri
import org.tyranid.oauth.{ OAuth, Token }
import org.tyranid.profile.{ Org, User }
import org.tyranid.session.Session
import org.tyranid.social.SoApp
import org.tyranid.ui.Form
import org.tyranid.web.{ WebContext, Weblet }

case class LiApp( apiKey:String, secret:String ) extends SoApp {

  val networkCode = "li"
  val networkName = "LinkedIn"

  val logo = "/images/linkedin_logo.png"

  lazy val oauth = OAuth( key = apiKey, secret = secret )

  def copyAttributes( from:User, to:User ) = {
    to( 'liid ) = from.s( 'liid )
    to( 'lit )  = from.s( 'lit )
    to( 'lits ) = from.s( 'lits )
  }

  def saveAttributes( user:User ) {
    B.User.db.update(
      Mobj( "_id" -> user.id ),
      Mobj( $set -> Mobj(
        "liid" -> user.s( 'liid ),
        "lit"  -> user.s( 'lit ),
        "lits" -> user.s( 'lits ) )
      )
    )
  }

  def removeAttributes( user:DBObject ) {
    user.remove( 'liid )
    user.remove( 'lit )
    user.remove( 'lits )
    B.User.db.update( Mobj( "_id" -> user.id ), Mobj( $unset -> Mobj( "liid" -> 1, "lit" -> 1, "lits" -> 1 ) ) )
  }

  def loginButton( weblet:Weblet ) = {
    val loggingOut = T.web.req.s( 'lo ).notBlank
    
    <head>
     <script type="text/javascript" src="//platform.linkedin.com/in.js">
        api_key:{ apiKey }
        authorize: true
        credentials_cookie: true
        onLoad: onLinkedInLoad
     </script>
     <script type="text/javascript">{ Unparsed("""

""" + ( loggingOut |* "window.liLogOut = true;" ) + """

function onLinkedInAuth() {
  if ( !window.liLogOut )
    window.location.assign( '""" + weblet.wpath + """/inli' );
  else
    delete window.liLogOut
}

function onLinkedInLoad() {
""" + ( loggingOut |* """
  IN.User.logout();
  setTimeout( "delete window.liLogOut;", 1200 );
""" ) + """
}
""") }</script>
     </head>
     <script type="IN/Login" data-onAuth="onLinkedInAuth"/>
   }

  def linkButton = {
    <head>
     <script src="//platform.linkedin.com/in.js">
       api_key: { B.linkedIn.apiKey }
       authorize: true
       credentials_cookie: true
     </script>
     <script>{ Unparsed( """
       function onLinkedInAuth() {
         $.post('/linkedin/exchange', function(data) {
           window.location.reload( true );
         });
       }
     """ ) }</script>
    </head>
    <script type="IN/Login" data-onAuth="onLinkedInAuth"/>
  }

  def linkPreview( user:User ) = {
    val uid = user.s( 'liid )
    val profile = B.linkedIn.GET( "/people/id=" + uid + ":(id,first-name,last-name,picture-url)", user ).parseJsonObject

    { Form.text( "First Name", profile.s( 'firstName ) ) } ++
    { Form.text( "Last Name", profile.s( 'lastName ) ) } ++
    { profile.contains( 'pictureUrl ) |*
      Form.thumbnail( "Profile Image", profile.s( 'pictureUrl ), href = "/linkedin/useThumbnail", hrefLabel = "Use Picture" ) }
  }


  private val cookieName = "linkedin_oauth_" + apiKey

  def isActive = {
    val cv = T.web.req.cookieValue( cookieName )
    cv.notBlank && cv != "null"
  }

  def exchangeToken:Boolean = {

    val cookie =
      T.web.req.cookie( cookieName ).getOrElse {
        log( Event.LinkedIn, "m" -> ( "Linkedin exchange missing " + cookieName + " cookie.  Cannot exchange linked in bearer token for a server token." ) )
        return false
      }

    val json = cookie.getValue.decUrl.parseJsonObject

    val uid         = json.s( 'member_id )
    val accessToken = json.s( 'access_token )
    val signature   = json.s( 'signature )

    val text = new StringBuilder
    for ( fieldName <- json( 'signature_order ).as[Array[String]] )
      text ++= json( fieldName ).toString

    val calcSignature = OAuth.hmacSha1( text.toString, secret )

    if ( calcSignature != signature )
      throw new RuntimeException( "Failed signature match." )

    val exchangeUrl = "https://api.linkedin.com/uas/oauth/accessToken"

    val params = mutable.Map(
      "oauth_consumer_key"         -> oauth.key,
      "xoauth_oauth2_access_token" -> accessToken,
      "oauth_signature_method"     -> OAuth.signatureMethod )

    oauth.sign( "POST", exchangeUrl, params )

    val str = exchangeUrl.POST( content = OAuth.encParams( params ), contentType = "application/x-www-form-urlencoded" )

    val session = Session()
    val user = session.user

    user( 'liid ) = uid
    for ( rslt <- str.splitAmp;
          ( key, value ) = rslt.splitFirst( '=' ) ) {
      key match {
      case "oauth_token"                    => user( 'lit )  = value
      case "oauth_token_secret"             => user( 'lits ) = value
      case "oauth_expires_in"               => // should be 0
      case "oauth_authorization_expires_in" => // should be 0
      case _ =>
      }
    }

    exchangeAttributes( user )
    true
  }

  def tokenFor( user:User ) = Token( key = user.s( 'lit ), secret = user.s( 'lits ) )

  def GET( url:String, user:User ) = oauth.GET( "https://api.linkedin.com/v1" + url, tokenFor( user ), headers = Map( "x-li-format" -> "json" ) )


  /*
   * * *   People
   */

  def importUser( user:User, uid:String ) = {
    val profile = GET( "/people/id=" + uid + ":(id,first-name,last-name,picture-url,headline,location:(country:(code)))", user ).parseJsonObject

    user( 'firstName ) = profile( 'firstName )
    user( 'lastName )  = profile( 'lastName )
    user( 'thumbnail ) =
      if ( profile.contains( 'pictureUrl ) ) profile( 'pictureUrl )
      else                                   "/icon_individual.png"
    user( 'title )     = profile( 'headline )

    val country = profile.o( 'location ).o( 'country ).s( 'code )
    if ( country.notBlank )
      user( 'country ) = Country.idByIso3166_2( country )
  }


  /*
   * * *   Companies
   */

  val companyFields =
    "(id,name,website-url,industry,specialties,logo-url,square-logo-url,employee-count-range,description,twitter-id,blog-rss-url," +
    "founded-year,locations:(is-headquarters,description,address:(street1,street2,city,state,postal-code,country-code,region-code),contact-info))"

  def companiesById( user:User, ids:String* ):Seq[ObjectMap] =
    GET( "/companies::(" + ids.mkString( "," ) + "):" + companyFields, user ).parseJsonObject.a_?( 'values ).of[ObjectMap]

  def loadCompanies( user:User, domain:String = null, positions:Boolean = false, multi:Boolean = false, bestMatch:Boolean = false ):Seq[ObjectMap] = {
    val domainPart = domain != null |* Uri.domainPart( domain ).toLowerCase

    var companies = mutable.ArrayBuffer[ObjectMap]()
    var found = false
val strictMatch = true

    if ( domain.notBlank ) {
      companies ++= GET( "/companies:" + companyFields + "?email-domain=" + domain.encUrl, user ).parseJsonObject.a_?( 'values ).of[ObjectMap]
      if ( companies.size > 0 )
        found = true
    }

    if ( bestMatch && strictMatch ) {
      if ( companies.size != 1 )
        return Nil

      return companies
    }

    if ( positions && ( multi || companies.size == 0 ) ) {
      val ids = GET( "/people/~:(positions:(company:(id)))", user ).parseJsonObject.o( 'positions ).a_?( 'values ).of[ObjectMap].map( _.o_?( 'company ).s( 'id ) ).
        filter( id => id.notBlank && !companies.exists( _.s( 'id ) == id ) )

      if ( ids.nonEmpty )
        companies ++= companiesById( user, ids:_* )
    }

    def domainPartPresent = companies.exists( c => Uri.lowerDomainChars( c.s( 'name ) ).contains( domainPart ) )

    if ( domain.notBlank && ( companies.size == 0 || !domainPartPresent ) ) {
      var terms = Uri.nameForDomain( domain )
      if ( terms.notBlank )
        companies ++= GET( "/company-search:(companies:" + companyFields + ")?keywords=" + terms.encUrl, user ).parseJsonObject.o_?( 'companies ).a_?( 'values ).of[ObjectMap]

      if ( companies.size == 0 || !domainPartPresent ) {
        terms = domainPart.toLowerCase
        companies ++= GET( "/company-search:(companies:" + companyFields + ")?keywords=" + domain.encUrl, user ).parseJsonObject.o_?( 'companies ).a_?( 'values ).of[ObjectMap]
      }

      // An additional/alternative approach would be to split up the word into its component strings ... i.e. "mygreatdomain" becomes "my great domain"
      //    see http://stackoverflow.com/questions/195010/how-can-i-split-multiple-joined-words
    }

    if ( bestMatch ) {

      // exact name match
      var candidates = companies.filter( c => Uri.lowerDomainChars( c.s( 'name ) ).contains( domainPart ) )
      if ( candidates.size > 0 ) {
        companies = candidates
        found = true
      }

      // website match
      val websiteUrl = ( "//www." + domain ).toLowerCase
      candidates = companies.filter( _.s( 'websiteUrl ).toLowerCase.endsWith( websiteUrl ) )
      if ( candidates.size > 0 ) {
        companies = candidates
        found = true
      }

      if ( !found )
        return Nil

      // smallest-name ... i.e. "AT&T" subsumes "AT&T Mobility" because the former exists as a substring of the latter
      var candidate = companies.minBy( _.s( 'name ).size ) 
      val shortestName = candidate.s( 'name ).toLowerCase

      if ( companies.forall( _.s( 'name ).toLowerCase.contains( shortestName ) ) )
        return Seq( candidate )
    }

    companies
  }

  def importCompany( c:ObjectMap, org:DBObject ) = {

    def string( liName:String, name:String ) = {
      val v = c.s( liName )
      if ( v.notBlank )
        org( name ) = v
    }

    string( 'id,            'liid )
    string( 'websiteUrl,    'website )

    val logo = c.s( 'squareLogoUrl ) or c.s( 'logoUrl )
    var storeInS3 = false
    
    if ( logo.notBlank ) {
      org( 'thumbnail ) = logo
      storeInS3 = true
    }

    if ( org.s( 'thumbnail ).isBlank )
      org( 'thumbnail ) = "/icon_company.png"

    if ( org.s( 'name ).isBlank )
      string( 'name,          'name )

    val numEmployees = c.o_?( 'employeeCountRange ).s( 'name )
    if ( numEmployees.notBlank )
      org( 'numEmployees ) = numEmployees

    string( 'description,    'desc )
    string( 'twitterId,      'twitter )
    string( 'blogRssUrl,     'blog )

    val foundedYear = c.i( 'foundedYear )
    if ( foundedYear != 0 )
      org( 'foundedYear ) = foundedYear

    val industryCode = Industry.lookupLinkedIn( c.s( 'industry ) )
    if ( industryCode > 0 )
      org.a_!( 'sellingCategories ).add( industryCode.box )

    val spec = c.o_?( 'specialties ).a_?( 'values )
    if ( spec.nonEmpty )
      org( 'specialties ) = Mlist( spec:_* )

    val isNew = org.isNew
    B.Org( org ).save

    val ls = c.o( 'locations )
    if ( ls != null &&
         ( // only import locations if we don't have existing locations already
           isNew || B.Location.db.find( Mobj( "org" -> org.id ) ).toSeq.isEmpty ) ) {
      for ( l <- ls.a_?( 'values ).of[ObjectMap] ) {

        val loc = Mobj()
        loc( 'thumbnail ) = "/icon_company.png"
        loc( 'org )       = org.id

        val desc = l.s( 'description )
        if ( desc.notBlank )
          loc( 'additionalInformation ) = desc

        val ph = l.o_?( 'contactInfo ).s( 'phone1 )
        if ( ph.notBlank )
          loc( 'phone ) = ph

        val a = l.o( 'address )

        if ( a != null ) {
          val addr = Mobj()
          addr( 'street1 )    = a.s( 'street1 )
          addr( 'street2 )    = a.s( 'street2 )
          addr( 'city )       = a.s( 'city )
          addr( 'state )      = Region.idForAbbr( a.s( 'state ) )
          addr( 'postalCode ) = a.s( 'postalCode )

          // linkedin also provides "regionCode" ... we're not using it (yet)

          addr( 'country )    = Country.idForCode( a.s( 'countryCode ) )
          loc( 'address ) = addr
        }

        val hq = l.b( 'isHeadquarters ) || ( desc.notBlank && desc.toLowerCase.contains( "headquarter" ) )
        if ( hq )
          loc( 'type ) = LocationType.HeadquartersId

        B.Location.db.save( loc )

        if ( hq ) {
          org( 'hq ) = loc.id
          B.Org( org ).save
        }
      }
    }

    if ( storeInS3 ) {
      val url = org.s( 'thumbnail )
        
      if ( url notBlank ) {
        val bucket = B.getS3Bucket( "public" )
        org( 'thumbnail ) = S3.storeUrl( bucket, url, File.pathFor( B.Org.tid, B.Org.idToRecordTid( org( "_id" ) ), "thumbnail", url ) )
        B.Org( org ).save
      }
    }
  }

  def createCompany( user:User, domain:String ):Org = {

    loadCompanies( user, domain, bestMatch = true ) foreach { company =>

      val org = Mobj()
      org( 'domain ) = domain
      org( 'vname ) = Uri.domainPart( domain )
      importCompany( company, org )
      B.Org.db.save( org )
      return B.Org( org )
    }

    return null
  }

  def createCompanies( fromOrgId:ObjectId, domains:Seq[String] ):Seq[Org] = {

    val appUsers = B.User.db.find( Mobj( "org" -> B.appOrgId, "liid" -> Mobj( $exists -> true ) ) ).toSeq

    val users = B.User.db.find( Mobj( "org" -> fromOrgId, "liid" -> Mobj( $exists -> true ) ) ).toSeq ++ appUsers

    if ( users.length == 0 ) {
      // TODO: Do something here if the org does not have any users
      return Vector()
    }
    
    var uIdx = 0

    domains.
      filter( domain => !B.Org.db.exists( Mobj( "domain" -> domain ) ) ).
      flatMap { domain =>

        def attemptCreate:Option[Org] =
          try {
            Option( createCompany( B.User( users( uIdx ) ), domain ) )
          } catch {
          case e:Http403Exception =>
            e.printStackTrace()
              println( "I think we reached our LinkedIn limit for a user: " + e.getMessage )
            // this is probably a throttle exception ?

            uIdx = uIdx + 1
            
            if ( uIdx > users.size )
              throw e // we're totally exhausted

            attemptCreate
          }
        
        attemptCreate
      }
  }
}

object LinkedInlet extends Weblet {

  def handle( web:WebContext ) {
    val s = Session()
    val u = s.user

    rpath match {
    case "/exchange" =>
      B.linkedIn.exchangeToken
      web.res.ok

    case "/useThumbnail" =>
      val uid = u.s( 'liid )
      val profile = B.linkedIn.GET( "/people/id=" + uid + ":(id,picture-url)", u ).parseJsonObject
      
      if ( profile.contains( 'pictureUrl ) ) {
        val url = profile.s( 'pictureUrl )
        val bucket = B.getS3Bucket( "public" )
        u( 'thumbnail ) = S3.storeUrl( bucket, url, File.pathFor( B.User.tid, B.User.idToRecordTid( u( "_id" ) ), "thumbnail", url ) )
        s.notice( "Your " + B.applicationName + " profile image has been set to your LinkedIn profile image." )
        B.User.db.update( Mobj( "_id" -> u.id ), Mobj( $set -> Mobj( "thumbnail" -> u.s( 'thumbnail ) ) ) )
      }

      T.web.redirect( "/user/edit" )

    case _ => _404
    }
  }
}

