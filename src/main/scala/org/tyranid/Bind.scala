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

package org.tyranid

import scala.collection.mutable
import scala.xml.NodeSeq

import org.tyranid.profile.User
import org.tyranid.ui.Session


object Debug {
  def check( xml: => NodeSeq ):NodeSeq = {
    try {
      xml
    } catch {
      case e:_root_.net.liftweb.http.ResponseShortcutException => throw e
      case t:Throwable => t.printStackTrace; <div class="error">Internal site problem, please try again later.</div>
    }
  }
}

object Bind {

  // Environment
  @volatile var EnvSuffix = "" // "-x" or "-dx"

  @volatile var NewUser:() => User = null
  @volatile var NewSession:() => Session = null

  // DB
  @volatile var ProfileDbName:String = "default"

  @volatile var MongoHost:String = "localhost"

  // SQL
  @volatile var DbUrl:String  = ""
  @volatile var DbUser:String = ""
  @volatile var DbPw:String   = ""
  @volatile var DbDriver      = "org.postgresql.Driver"

  // ReCaptcha
  @volatile var ReCaptchaPublicKey      = ""
  @volatile var ReCaptchaPrivateKey     = ""
  
  // AWS
  import org.tyranid.cloud.aws.S3Bucket

  @volatile var AwsCredentials:com.amazonaws.auth.AWSCredentials = null

  @volatile var BucketSuffix:String = ""

  val S3Buckets = mutable.Map[String,S3Bucket]()

  def apply( bucket:S3Bucket ) = S3Buckets( bucket.prefix ) = bucket

  // Assistly
  @volatile var AssistlySiteKey = ""
  @volatile var AssistlyMultipassKey = ""

}

