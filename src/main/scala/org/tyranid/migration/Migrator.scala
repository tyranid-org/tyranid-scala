/**
 * Copyright (c) 2008-2012 Tyranid <http://tyranid.org>
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

package org.tyranid.migration

import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.db.DbChar
import org.tyranid.db.mongo.{ MongoEntity, MongoRecord }
import org.tyranid.db.mongo.Imp._

object MigrationStatus extends MongoEntity( tid = "a0Mt" ) {
  "_id"            is DbChar(50) is 'id;
}

class MigrationStatus( override val obj:DBObject = Mobj() ) extends MongoRecord( MigrationStatus.makeView, obj )
  

case class Migrator( migrators:Migrates* ) {
  if ( migrators != null ) {
    println( "*** Beginning Migration" )
    
    for ( migrator <- migrators ) {
      if ( !MigrationStatus.db.exists( Mobj( "_id" -> migrator.name ) ) ) {
        println( "[" + migrator.name + ": START]" )
        migrator.migrate
         
        val m = new MigrationStatus()
        m( '_id ) = migrator.name
        m.save
        
        println( "[" + migrator.name + ": COMPLETE]" )
      } else {
        println( "[" + migrator.name + ": ALREADY APPLIED]" )
      }
    }
    
    println( "*** End Migration" )
  }
}
  
trait Migrates {
  val name:String
  def migrate
}