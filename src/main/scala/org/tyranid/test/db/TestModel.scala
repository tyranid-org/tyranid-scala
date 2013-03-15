
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
  
  override def toClientCommonMap:Map[String,Any] = null
}

class Session extends org.tyranid.session.Session {

  override def user:User = super.user.asInstanceOf[User]
}

object Session extends org.tyranid.session.SessionMeta {

  override def apply:Session = super.apply.asInstanceOf[Session]
}



object Widget extends MongoEntity( tid = "ts0t" ) {
  "_id"        is DbMongoId                 is 'id;
  "name"       is DbChar(32)                is 'client is 'label;
  "dims"       is Dimensions                is 'client;
  "tags"       is DbArray(DbChar(32))       is 'client;
  "level"      is DbInt                     is 'client;
  "categories" is DbLink(Category)          ;
  "prices"     is DbArray(Pricing)          is 'client;
  "cert"       is DbLink(Certification)     ;
  "tids"       is DbTid(Category,PriceType) ;

  "level2"     is DbInt                     is 'client is 'temporary computed( _.i( 'level ) * 2 )
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
  "type"         is DbLink(PriceType)   is 'client;
  "price"        is DbDouble            is 'client;
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



