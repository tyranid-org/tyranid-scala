/**
 * Copyright (c) 2008-2011 Tyranid <http://tyranid.org>
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

import scala.xml.NodeSeq

import org.tyranid.web.Weblet

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

  val weblets:List[(String,Weblet)]

  val templates:List[(String, ( NodeSeq ) => NodeSeq )]

  val comets:List[ ( org.cometd.bayeux.server.BayeuxServer ) => org.cometd.server.AbstractService ]

  def boot:Unit
}

