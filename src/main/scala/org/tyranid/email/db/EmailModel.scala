package org.tyranid.email.db

import org.tyranid.db.mongo.MongoEntity;
import org.tyranid.db._;
import org.tyranid.Imp._;

object EmailConfig extends MongoEntity( tid = "a0At" ) {
  "id"           is DbInt is 'key;
  "host"         is DbChar(40);
  "port"         is DbInt;
  "authUser"     is DbChar(40);
  "authPassword" is DbPassword;
  
  override lazy val dbName = name
}
