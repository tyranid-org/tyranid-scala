/**
 * Copyright (c) 2008-2013 Tyranid <http://tyranid.org>
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

package org.tyranid.secure

import org.tyranid.db.mongo.MongoEntity;
import org.tyranid.db._;
import org.tyranid.Imp._;



// maybe this shouldn't be a ControlThrowable ?
case class SecureException() extends scala.util.control.ControlThrowable


sealed trait AccessType
case object Viewing extends AccessType
case object Editing extends AccessType


object Secure extends MongoEntity( tid = "a0Bt" ) {
  "_id"                 is DbInt is 'id;
  "recaptchaPublicKey"  is DbChar(40);
  "recaptchaPrivateKey" is DbChar(40);
  
  override lazy val dbName = name
}
