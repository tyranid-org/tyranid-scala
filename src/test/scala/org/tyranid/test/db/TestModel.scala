
package org.tyranid.test.db

import org.bson.types.ObjectId
import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.collection.ConcurrentExpireAutoMap
import org.tyranid.db._
import org.tyranid.db.es.Search
import org.tyranid.db.tuple.{ Tuple, TupleView }
import org.tyranid.db.ram.RamEntity
import org.tyranid.db.meta.{ UiMap, UiMapping }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ DbMongoId, MongoEntity, MongoView, MongoRecord }
import org.tyranid.email.Email
import org.tyranid.image.DbImage
import org.tyranid.io.{ DbFile, DbLocalFile }
import org.tyranid.secure.DbReCaptcha
import org.tyranid.time.Time



object Widget extends MongoEntity( tid = "test0" ) {
  "id"           is DbMongoId           is 'key;
  "name"         is DbChar(32)          is 'label;
  "dims"         is Dimensions          ;
  "tags"         is DbArray(DbChar(32)) ;
  "level"        is DbInt               ;
}

object Dimensions extends MongoEntity( tid = "test1" ) {
  "height"       is DbInt       ;
  "weight"       is DbInt       ;
}

