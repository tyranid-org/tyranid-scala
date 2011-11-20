
package org.tyranid.test

import org.tyranid.Imp._
import org.tyranid.Bind
import org.tyranid.test.db.{ Session, User }


object TestBoot {

  lazy val boot = {

    // Mongo
    Bind.ProfileDbName = "test"

    Bind.NewUser    = () => new User
    Bind.NewSession = () => new Session
  }
}

