
package bootstrap.tyranid

import scala.xml.NodeSeq

import org.tyranid.Imp._
import org.tyranid.cloud.aws.S3Bucket
import org.tyranid.db.mongo.MongoEntity
import org.tyranid.document.DocApp
import org.tyranid.secure.AccessType
import org.tyranid.session.ThreadData
import org.tyranid.test.db.{ Session, User }


class Boot extends org.tyranid.boot.Bootable {
  val applicationName = "Tyranid Test Suite"
  val domain          = "localhost"
  val profileDbName   = "test"
  val productName     = "TEST"
  val systemEmail     = "info@tyranid.org"
  val systemUser = null
  val demoUser = null
  val publicGroup = null
  val sessionDataMeta = null
  val filesBucket:S3Bucket = null
  val documentEntity:MongoEntity = null
  val docPreviewApp:DocApp = null

  def finishConversion( content:org.tyranid.content.Content ) {}
  val alertEmail      = "info@tyranid.org"

  val applicationWebloc = null
  val weblocs = Nil

  val paths = Seq()
  
  val templates =
    "shell"  -> ( ( xml:NodeSeq ) => <html><head></head><body><tyr:header/><tyr:content/></body></html> ) ::
    "header" -> ( ( xml:NodeSeq ) => <h1>hi there</h1> ) ::
    "sample" -> ( ( xml:NodeSeq ) => <p>Sample</p> ) ::
    Nil

  val milestones = Seq()
  
  // AWS
  override val awsCredentials = new com.amazonaws.auth.BasicAWSCredentials( "TODO", "TODO" )
  override val bucketSuffix   = ".volerro.com"

  def canAddUser( o:org.tyranid.profile.Org ) = true
  
  val emailTemplates = null

  val comets = Nil

  lazy val userMeta = User
  lazy val orgMeta = Org

  override def sendMessage( msg:String, toUserTid:String, fromUserTid:String = null ) { } 

  override def registerUser( user:org.tyranid.profile.User, companyName:String ) { }
    
  val build = 1

  def access( thread:ThreadData, accessType:AccessType, ref:AnyRef ) {}

  def boot = {
    bucket( S3Bucket( prefix = "public", cfDistributionId = "TODO", cfDomain = "TODO" ) )

    User = org.tyranid.test.db.User

    newUser    = () => new User
    newSession = () => new Session
  }
  
  override def welcomeUserEvent = {}

}

