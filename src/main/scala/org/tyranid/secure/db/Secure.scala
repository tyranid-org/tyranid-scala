package org.tyranid.secure.db

import org.tyranid.db.mongo.MongoEntity;
import org.tyranid.db._;

object Secure extends MongoEntity( tid = "a0Bt" ) {
  "id"                  is DbInt is 'key;
  "recaptchaPublicKey"  is DbChar(40);
  "recaptchaPrivateKey" is DbChar(40);
  
  override lazy val dbName = name
}
