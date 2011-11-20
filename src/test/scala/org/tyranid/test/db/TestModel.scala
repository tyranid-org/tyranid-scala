
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


object User extends MongoEntity( tid = "test5" ) {
  "id"             is DbMongoId           is 'key;
  "email"          is DbEmail             is 'label is 'required;
  "password"       is DbPassword          is 'required;
  "password2"      is DbPassword          is 'required is 'temporary as "Repeat Password";
  "stayLoggedIn"   is DbBoolean           as "Keep me logged in for two weeks";
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



object Widget extends MongoEntity( tid = "test0" ) {
  "id"           is DbMongoId           is 'key;
  "name"         is DbChar(32)          is 'label;
  "dims"         is Dimensions          ;
  "tags"         is DbArray(DbChar(32)) ;
  "level"        is DbInt               ;
  "categories"   is DbLink(Category)    ;
  "prices"       is DbArray(Pricing)    ;
}

object Dimensions extends MongoEntity( tid = "test1" ) {
  "height"       is DbInt               ;
  "weight"       is DbInt               ;
}

object Category extends MongoEntity( tid = "test2" ) {
  "id"           is DbMongoId           is 'key;
  "name"         is DbChar(128)         is 'label;
}

object Pricing extends MongoEntity( tid = "test3" ) {
  "type"         is DbLink(PriceType)   ;
  "price"        is DbDouble            ;
}

object PriceType extends MongoEntity( tid = "test4" ) {
  "id"           is DbMongoId           is 'key;
  "name"         is DbChar(128)         ;
  "quantity"     is DbInt               ;
}


