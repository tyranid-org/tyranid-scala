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

import org.cometd.bayeux.server.BayeuxServer

import org.tyranid.db.Entity
import org.tyranid.profile.{ User, UserMeta }
import org.tyranid.session.Session
import org.tyranid.web.{ Weblet, CometService }


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

  val applicationName:String

  val weblets:List[(String,Weblet)]

  val templates:List[(String, ( NodeSeq ) => NodeSeq )]

  val comets:Seq[CometService]

  @volatile var bayeux:BayeuxServer = null

  def boot:Unit


  @volatile var newUser:() => User = null
  val userMeta:UserMeta
  @volatile var userEntity:Entity = null
  @volatile var newSession:() => Session = null
  @volatile var loginCookieName:String = null

  // Environment
  val envSuffix = "" // "-x" or "-dx"

  // DB
  @volatile var profileDbName:String = null

  @volatile var mongoHost:String = null

  // SQL
  @volatile var dbUrl:String  = ""
  @volatile var dbUser:String = ""
  @volatile var dbPw:String   = ""
  @volatile var dbDriver      = "org.postgresql.Driver"

  // LinkedIn
  val linkedInApiKey    = ""
  val linkedInSecretKey = ""

  // ReCaptcha
  val reCaptchaPublicKey      = ""
  val reCaptchaPrivateKey     = ""

  // Assistly
  val assistlySiteKey = ""
  val assistlyMultipassKey = ""

  // AWS
  import org.tyranid.cloud.aws.S3Bucket

  @volatile var awsCredentials:com.amazonaws.auth.AWSCredentials = null

  @volatile var bucketSuffix:String = ""

  val s3Buckets = scala.collection.mutable.Map[String,S3Bucket]()

  def apply( bucket:S3Bucket ) = s3Buckets( bucket.prefix ) = bucket
}

