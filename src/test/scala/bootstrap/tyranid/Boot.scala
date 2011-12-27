
package bootstrap.tyranid

import scala.xml.NodeSeq

import org.tyranid.Imp._
import org.tyranid.Bind
import org.tyranid.test.db.{ Session, User }


class Boot extends org.tyranid.boot.Bootable {

  val weblets = Nil

  val templates =
    "shell" -> ( xml:NodeSeq ) => <html><head></head><body><tyr:content/></body></html> ) ::
    Nil

  def boot = {

    // Mongo
    Bind.ProfileDbName = "test"

    Bind.NewUser    = () => new User
    Bind.NewSession = () => new Session
  }
}

