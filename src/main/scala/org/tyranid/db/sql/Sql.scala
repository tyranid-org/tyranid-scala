/**
 * Copyright (c) 2008-2011 Tyranid <http://tyranid.org>
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

package org.tyranid.db.sql

import java.sql.{ Connection, DriverManager, SQLException }

import net.liftweb.common.{ Box, Empty, Full }
import net.liftweb.mapper.{ DefaultConnectionIdentifier, ConnectionIdentifier, ConnectionManager }

import org.tyranid.Bind
import org.tyranid.db.Entity


/**
 * IMPlicit IMPorts.
 */
object Imp {
}


object SqlVendor extends ConnectionManager {
  private var pool:List[Connection] = Nil
  private var poolSize = 0
  private val maxPoolSize = 4

  	
  private def createOne:Box[Connection] =
    try {
	    Class.forName( Bind.DbDriver )

			Full( DriverManager.getConnection( Bind.DbUrl, Bind.DbUser, Bind.DbPw ) )
	  } catch {
	    case e: Exception => e.printStackTrace; Empty
	  }

  def newConnection( name:ConnectionIdentifier ):Box[Connection] =
    synchronized {
      pool match {
			case Nil if poolSize < maxPoolSize =>
	  		val ret = createOne
        poolSize += 1
        ret.foreach(c => pool = c :: pool)
        ret

			case Nil => wait(1000L); newConnection(name)
			case x :: xs => try {
	        x.setAutoCommit(false)
	        Full(x)
	      } catch {
          case e => try {
            pool = xs
            poolSize = poolSize - 1
            x.close
            newConnection(name)
          } catch {
            case e => newConnection(name)
          }
        }
      }
    }

  def releaseConnection( conn:Connection ):Unit = synchronized {
    pool = conn :: pool
    notify
  }
}


object Sql {

	def literal( sb: StringBuilder, v:AnyRef ) =
		v match {
		case null      => sb ++= "NULL"
		case s: String => sb += '\'' ++= pgEscapeStr( s ) += '\''
		case o         => sb ++= o.toString
		}

	def pgEscapeStr( str:String ): String = {
		var pos = str.indexOf( '\'' )

		if ( pos == -1 )
			return str

		val sb = new StringBuilder
		for ( ch <- str ) {
			if ( ch == '\'' )
				sb += '\''

			sb += ch
		}

		sb.toString
	}

	def connect[ T ]( block: ( Connection ) => T ): T = {
		SqlVendor.newConnection( DefaultConnectionIdentifier ) match {
		case Full( conn ) =>
			try {
				block( conn )
			} finally {
				SqlVendor.releaseConnection( conn )
			}

		case _ =>
			throw new RuntimeException( "ERROR:  Could not allocate database connection!" )
		}
	}

	def commitUpdate( sql:String ) = {
		connect { conn =>
			val stmt = conn.createStatement

			val oldAc = conn.getAutoCommit
			conn.setAutoCommit( true )

			try {
				stmt.executeUpdate( sql )
			} finally {
				stmt.close
				conn.setAutoCommit( oldAc )
			}
		}
	}
}

