
package org.tyranid.social.linkedin

import javax.servlet.http.Cookie

import scala.annotation.tailrec
import scala.collection.mutable
import scala.xml.Unparsed

import org.bson.types.ObjectId
import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.company.Industry
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.MongoEntity
import org.tyranid.email.Email
import org.tyranid.http.Http403Exception
import org.tyranid.locale.{ Country, LocationType, Region }
import org.tyranid.oauth.{ OAuth, Token }
import org.tyranid.profile.{ Org, User }
import org.tyranid.session.Session
import org.tyranid.web.{ WebContext, Weblet }


object LinkedIn {
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
}


case class LiApp( apiKey:String, secret:String ) {

  lazy val oauth = OAuth( key = apiKey, secret = secret )

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
    window.location.assign( '""" + weblet.wpath + """/linkedin' );
  else
    delete window.liLogOut
}

function onLinkedInLoad() {
""" + ( loggingOut |* """
  IN.User.logout();
  setTimeout( "delete window.liLogOut;", 1200 );
""" ) + """
}

$(document).ready(function() {
  $('#forgot').click(function(e) {
    window.location.assign( '""" + weblet.wpath + """/forgot?un=' + encodeURIComponent( $("#un").val() ) );
  });
});
""") }</script>
     </head>
     <script type="IN/Login" data-onAuth="onLinkedInAuth"/>
   }

  def exchangeToken:Boolean = {

    val cookieName = "linkedin_oauth_" + apiKey
    val cookie =
      T.web.req.cookie( cookieName ).getOrElse {
        log( Log.LinkedIn, "m" -> ( "Linkedin exchange missing " + cookieName + " cookie.  Cannot exchange linked in bearer token for a server token." ) )
        return false
      }

    val json = cookie.getValue.decUrl.parseJsonObject

    val memberId    = json.s( 'member_id )
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

    user( 'liid ) = memberId
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

    val existing = B.User.db.findOne( Mobj( "liid" -> memberId ) )
    if ( existing != null && user.id != null && existing.id != user.id )
      LinkedIn.removeAttributes( existing )

    if ( !user.isNew )
      LinkedIn.saveAttributes( user )

    true
  }

  def tokenFor( user:User ) = Token( key = user.s( 'lit ), secret = user.s( 'lits ) )

  def GET( url:String, user:User ) = oauth.GET( "https://api.linkedin.com/v1" + url, tokenFor( user ), headers = Map( "x-li-format" -> "json" ) )



  /*
   * * *   Companies
   */

  val companyFields =
    "(id,name,website-url,industry,specialties,logo-url,square-logo-url,employee-count-range,description,twitter-id,blog-rss-url," +
    "founded-year,locations:(is-headquarters,description,address:(street1,street2,city,state,postal-code,country-code,region-code),contact-info))"

  def companiesById( user:User, ids:String* ):Seq[ObjectMap] =
    GET( "/companies::(" + ids.mkString( "," ) + "):" + companyFields, user ).parseJsonObject.a_?( 'values ).of[ObjectMap]

  def loadCompanies( user:User, domain:String = null, positions:Boolean = false, multi:Boolean = false ):Seq[ObjectMap] = {

    val companies = mutable.ArrayBuffer[ObjectMap]()

    if ( domain.notBlank )
      companies ++= GET( "/companies:" + companyFields + "?email-domain=" + domain.encUrl, user ).parseJsonObject.a_?( 'values ).of[ObjectMap]

    if ( positions && ( multi || companies.size == 0 ) ) {
      val ids = GET( "/people/~:(positions:(company:(id)))", user ).parseJsonObject.o( 'positions ).a_?( 'values ).of[ObjectMap].map( _.o( 'company ).s( 'id ) ).
        filter( id => id.notBlank && !companies.exists( _.s( 'id ) == id ) )

      if ( ids.nonEmpty )
        companies ++= companiesById( user, ids:_* )
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
    if ( logo.notBlank )
      org( 'thumbnail ) = logo

    if ( org.s( 'name ).isBlank )
      string( 'name,          'name )

    val numEmployees = c.o( 'employeeCountRange ).s( 'name )
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

    val spec = c.o( 'specialties )
    if ( spec != null ) {
      val arr = spec.a_?( 'values )
      if ( arr.nonEmpty )
        org( 'specialties ) = Mlist( arr:_* )
    }

    val ls = c.o( 'locations )
    if ( ls != null ) {
      val existingLocations = B.Location.db.find( Mobj( "org" -> org.id ) ).toSeq

      // only import locations if we don't have existing locations already
      if ( existingLocations.size == 0 ) {
        for ( l <- ls.a_?( 'values ).of[ObjectMap] ) {

          val loc = Mobj()
          loc( 'thumbnail ) = "/icon_company.png"
          loc( 'org )       = org.id

          val desc = l.s( 'description )
          if ( desc.notBlank )
            loc( 'additionalInformation ) = desc

          val ci = l.o( 'contactInfo )
          if ( ci != null ) {
            val ph = ci.s( 'phone1 )
            if ( ph.notBlank )
              loc( 'phone ) = ph
          }

          val a = l.o( 'address )

          val addr = Mobj()
          addr( 'street1 )    = a.s( 'street1 )
          addr( 'street2 )    = a.s( 'street2 )
          addr( 'city )       = a.s( 'city )
          addr( 'state )      = Region.idForAbbr( a.s( 'state ) )
          addr( 'postalCode ) = a.s( 'postalCode )

          // linkedin also provides "regionCode" ... we're not using it (yet)

          addr( 'country )    = Country.idForCode( a.s( 'countryCode ) )
          loc( 'address ) = addr

          if ( l.b( 'isHeadquarters ) )
            loc( 'type ) = LocationType.HeadquartersId

          B.Location.db.save( loc )

          if ( l.b( 'isHeadquarters ) ) {
            org( 'hq ) = loc.id
          }
        }
      }
    }

    B.Org.db.save( org )
  }

  def createCompany( user:User, domain:String ):Org = {

    loadCompanies( user, domain ) foreach { company =>

      val org = Mobj()
      org( 'domain ) = domain
      org( 'vname ) = Email.domainPart( "junk@" + domain )
      importCompany( company, org )
      B.Org.db.save( org )
      return B.Org( org )
    }

    return null
  }

  def createCompanies( fromOrgId:ObjectId, domains:Seq[String] ):Seq[Org] = {

    val users = B.User.db.find( Mobj( "org" -> fromOrgId, "liid" -> Mobj( $exists -> true ) ) ).toSeq

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
            // this is probably a throttle exception ?

            uIdx += 1
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

    case _ =>
      web.res.ok
    }
  }
}

