
package org.tyranid.test.db

import org.bson.types.ObjectId
import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.collection.ConcurrentExpireAutoMap
import org.tyranid.db._
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


object User extends org.tyranid.profile.UserMeta {
  "firstName"      is DbChar(32)          is 'required;
  "lastName"       is DbChar(32)          is 'required;
}

class User( override val obj:DBObject = Mobj() ) extends MongoRecord( User.makeView, obj ) with org.tyranid.profile.User {
  
  override def fullName = {
    val sb = new StringBuilder

    if ( s( 'firstName ).notBlank ) sb ++= s( 'firstName ).capitalize
    if ( s( 'lastName ).notBlank )  sb += ' ' ++= s( 'lastName ).capitalize
      
    sb.toString
  }
}



class Session extends org.tyranid.session.Session {

  override def user:User = super.user.asInstanceOf[User]
}

object Session extends org.tyranid.session.SessionMeta {

  override def apply:Session = super.apply.asInstanceOf[Session]
}



object Widget extends MongoEntity( tid = "ts0t" ) {
  "_id"        is DbMongoId                 is 'id;
  "name"       is DbChar(32)                is 'label;
  "dims"       is Dimensions                ;
  "tags"       is DbArray(DbChar(32))       ;
  "level"      is DbInt                     ;
  "categories" is DbLink(Category)          ;
  "prices"     is DbArray(Pricing)          ;
  "cert"       is DbLink(Certification)     ;
  "tids"       is DbTid(Category,PriceType) ;
}

object Dimensions extends MongoEntity( tid = "ts1t" ) {
  "height"       is DbInt               ;
  "weight"       is DbInt               ;
}

object Category extends MongoEntity( tid = "ts2t" ) {
  "_id"          is DbMongoId           is 'id;
  "name"         is DbChar(128)         is 'label;
}

object Pricing extends MongoEntity( tid = "ts3t" ) {
  "type"         is DbLink(PriceType)   ;
  "price"        is DbDouble            ;
}

object PriceType extends MongoEntity( tid = "ts4t" ) {
  "_id"          is DbMongoId           is 'id;
  "name"         is DbChar(128)         ;
  "quantity"     is DbInt               ;
}

object Certification extends RamEntity( tid = "ts6t" ) {
  "_id"    is DbInt      is 'id;
  "name"   is DbChar(64) is 'label;

  val CertifiedId = 1

  static { s =>
  s(       "_id", "name"        )
  s( CertifiedId, "Certified"   )
  s(           2, "Uncertified" )
  s(           3, "Pending"     )
  }
}



