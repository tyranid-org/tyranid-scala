package org.tyranid.secure.db

import org.tyranid.db.mongo.MongoEntity;
import org.tyranid.db._;

object Secure extends MongoEntity( tid = "a0Bt" ) {
  "publicKey"          is DbChar(40);
  "privateKey"         is DbChar(40);
  
  override lazy val dbName = name
}
