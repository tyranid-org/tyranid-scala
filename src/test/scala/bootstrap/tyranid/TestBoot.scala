
package bootstrap.tyranid

import scala.xml.NodeSeq

import org.tyranid.Imp._
import org.tyranid.test.db.{ Session, User }


class Boot extends org.tyranid.boot.Bootable {
  val applicationName = "Tyranid Test Suite"

  val weblets = Nil

  val templates =
    "shell"  -> ( ( xml:NodeSeq ) => <html><head></head><body><tyr:header/><tyr:content/></body></html> ) ::
    "header" -> ( ( xml:NodeSeq ) => <h1>hi there</h1> ) ::
    "sample" -> ( ( xml:NodeSeq ) => <p>Sample</p> ) ::
    Nil

  val comets = Nil

  lazy val userMeta = User

  def boot = {

    // Mongo
    profileDbName = "test"

    newUser    = () => new User
    newSession = () => new Session
  }
}

