
package org.tyranid.web

import org.cometd.bayeux.server.BayeuxServer
import org.cometd.server.AbstractService

import org.tyranid.Imp._


case class CometService( name:String, create: ( BayeuxServer ) => AbstractService ) {

  var service:AbstractService = null

  def init( bayeux:BayeuxServer ) {
    service = create( bayeux )
  }
}

