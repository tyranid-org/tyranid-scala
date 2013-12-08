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
  if ( migrators != null && migrators.length > 0 ) {
    var printedStart = false
    
    for ( migrator <- migrators ) {
      if ( !printedStart ) {
        println( "*** Beginning Migration" )
        printedStart = true
      }
      
      if ( !MigrationStatus.db.exists( Mobj( "_id" -> migrator.name ) ) ) {
        val m = new MigrationStatus()
        m( '_id ) = migrator.name
        m.save
        
        println( "[" + migrator.name + ": START]" )
        migrator.desc.lines.foreach( println )
        
        try {
          migrator.migrate
        } catch {
          case t:Throwable =>
            t.printStackTrace()
        }

        if ( migrator.commit ) {          
          println( "[" + migrator.name + ": COMPLETE]" )
        } else {
          MigrationStatus.delete( m.id )
          println( "\n*\n*\n*\n*\n*\n[" + migrator.name + " is COMPLETE but it is NOT committed (migrator will run again on next startup)]\n*\n*\n*\n*\n*\n" )
        }
      } else {
        println( "[" + migrator.name + ": ALREADY APPLIED]" )
      }
    }
    
    if ( printedStart )
      println( "*** End Migration" ) 
  }
}
  
trait Migrates {
  val name:String
  def migrate
  val commit = true
  val desc:String = ""
}
