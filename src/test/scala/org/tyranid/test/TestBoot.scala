
package org.tyranid.test

import org.tyranid.Imp._
import org.tyranid.Bind


object TestBoot {

  def boot {

    // Mongo
    Bind.ProfileDbName = "test"
  }
}

