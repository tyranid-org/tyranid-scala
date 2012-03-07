
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
  "id"                  is DbInt is 'key;
  "recaptchaPublicKey"  is DbChar(40);
  "recaptchaPrivateKey" is DbChar(40);
  
  override lazy val dbName = name
}
