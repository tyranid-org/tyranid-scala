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

import scala.collection.mutable.{ ArrayBuffer, HashMap }

import org.tyranid.Imp.{ string, symbol }
import org.tyranid.db.{ Attribute, DbIntSerial, DbChar, Entity, ModelException, Record, Schema, ViewAttribute }
import org.tyranid.db.tuple.{ TupleView, Tuple }



case class SqlEntity() extends Entity {

	override lazy val dbName = name.camelCaseToUnderLower.plural

	private def toCreateSql = {
		val sb = new StringBuilder
		sb ++= "CREATE TABLE " ++= dbName ++= " (\n"
		for ( a <- attribs )
			sb ++= "  " ++= a.dbName ++= " " ++= a.domain.sqlName ++= ",\n"

		sb ++= "  PRIMARY KEY(" ++= attribs.filter( _.isKey ).map( _.dbName ).mkString( ", " ) ++= ")\n"

		sb ++= ")"
		sb.toString
	}

	def create {
    /*
		Sql.commitUpdate( toCreateSql )
		if ( staticView != null )
			Sql.commitUpdate( staticView.toInsertSql( staticTuples:_* ) )
     */
	}

	private def toDropSql = "DROP TABLE IF EXISTS " + dbName

	def drop { Sql.commitUpdate( toDropSql ) }


}


abstract class SqlEnumEntity( nameLen: Int ) extends SqlEntity {

	"id"   is DbIntSerial       is 'key   ;
	"name" is DbChar( nameLen ) is 'label ;
}


/*

		-)  how do we get more type-safety into this?

	
		-)  what does SELECT and INSERT/UPDATE API look like?

		    SQL-template?


				SELECT name, gearScore
				  FROM characters


				when we get an actual result set back, we use ResultSetMetaData (RSMD) to map to viewattributes?

				* What about when we create a new view and it's for an insert, and so we're not going to do a SELECT yet, so hence no RSMD
				  --- we can get the metadata from the statement if we prepare it


				val charv = """
SELECT id, name, gearScore, resilience
  FROM characters
 WHERE id = ?
""" view

SELECT c#id, c#name, c#gearScore, c#resilience
  FROM character#c
 WHERE id = ?

 */
object SqlView {
	def apply( rsql:String ) = {
		val entries = new ArrayBuffer[SqlpEntry]

		def findEntity( alias: String ): Entity = {
			entries foreach {
				case t:SqlpTable if alias == t.alias => return t.table
				case _                               =>
			}

			null
		}

		var leafCount = 0
		val v = new SqlView

		val m = SqlView.r.pattern.matcher( rsql )
		var pos = 0
		while ( m.find( pos ) ) {
			val hash = rsql.indexOf( '#', m.start )

			val prefix = rsql.substring( m.start, hash )
			val suffix = rsql.substring( hash+1, m.end )

			entries += ( Schema.byDbName.get( prefix ) match {
								   case Some( en ) => new SqlpTable( m.start, m.end, en, suffix )	
							     case None       => leafCount += 1
									                    new SqlpField( m.start, m.end, prefix, suffix )
			             } )

			pos = m.end + 1
		}

		val leaves = new Array[ViewAttribute]( leafCount )
		var li = 0

		val sql = new StringBuilder
		var rs = 0

		entries foreach { e =>
			sql ++= rsql.substring( rs, e.start )
			rs = e.end

			e match {
			case f:SqlpField =>
				val en = findEntity( f.alias )
				val att = en.attByDbName( f.name )
				if ( att == null )
					throw new ModelException( "couldn't find attribute \"" + f.name + "\" in table \"" + en.dbName + "\"" )
				leaves( li ) = new ViewAttribute( v, att, li )
				li += 1
				if ( f.alias != "" )
					sql ++= f.alias += '.'
				sql ++= f.name
			case t:SqlpTable =>
				sql ++= t.table.dbName
				if ( t.alias != "" )
					sql += ' ' ++= t.alias
			}
		}
		sql ++= rsql.substring( rs )

		v.rsql = rsql
		v.sql = sql.toString
		v.leaves = leaves
		v
	}

	val r = "[a-zA-Z0-9_]*#[a-zA-Z0-9_]*".r
}

private abstract class SqlpEntry {
	val start: Int
	val end: Int
}

private case class SqlpField( start: Int, end: Int, alias: String, name: String ) extends SqlpEntry
private case class SqlpTable( start: Int, end: Int, table: Entity, alias: String ) extends SqlpEntry


class SqlView extends TupleView {

	var rsql:String = null
	var sql:String = null

	def query( params:AnyRef* ) = {
		Sql.connect { c =>
			val ps = c.prepareStatement( sql )
			val results = new ArrayBuffer[SqlRecord]

			try {
				for ( i <- 0 until params.length )
					ps.setObject( i+1, params( i ) )

				val rs = ps.executeQuery

				while ( rs.next ) {
					val tuple = new SqlRecord( this )

					for ( li <- 0 until leafCount ) {
						tuple.values( li ) = rs.getObject( li+1 )
					}

					results += tuple
				}
			} finally {
				ps.close
			}

			results
		}
	}

	def toInsertSql( tuples: SqlRecord* ) = {
		val sb = new StringBuilder

		sb ++= "INSERT INTO " ++= entity.dbName ++= " ("
		for ( li <- 0 until leafCount ) {
			if ( li > 0 ) sb += ','
			sb ++= leaves( li ).att.dbName
		}
		sb ++= ") VALUES\n  "

		var first = true
		for ( t <- tuples ) {
			if ( first )
				first = false
			else
				sb ++= ",\n  "

			sb += '('
			for ( li <- 0 until leafCount ) {
				if ( li > 0 ) sb += ','
				Sql.literal( sb, t( li ).asInstanceOf[AnyRef] )
			}
			sb += ')'
		}

		sb.toString
	}

	def makeRecords = new SqlRecord( this )
}

class SqlRecord( override val view:SqlView ) extends Tuple( view ) {

  override def /( key:String ) = apply( key ).asInstanceOf[SqlRecord]

	def save = {
		val en = view.entity
		val evas = view.eleaves
		val ekeys = view.ekeys

		val sb = new StringBuilder

		if ( isNew ) {
			sb ++= """
INSERT INTO """ ++= en.dbName ++= """
         ( """

			var first = true
	 		evas foreach { va =>
				if ( !va.att.domain.isAuto ) {
					if ( first ) first = false
					else         sb ++= ", "
					sb ++= va.att.dbName
				}
			}

			sb ++= """ )
  VALUES ( """

			first = true
	 		evas foreach { va =>
				if ( !va.att.domain.isAuto ) {
					if ( first ) first = false
					else         sb ++= ", "
					Sql.literal( sb, values( va.index ) )
				}
			}

			sb ++= " )"
		} else {
			sb ++= """
UPDATE """ ++= en.dbName ++= """
   SET """

			var first = true
	 		evas foreach { va =>
				if ( !va.att.isKey ) {
					if ( first ) first = false
					else         sb ++= ", "
					sb ++= va.att.dbName ++= " = "
					Sql.literal( sb, values( va.index ) )
				}
			}

			sb ++= """
 WHERE """

			first = true
			ekeys foreach { va =>
				if ( first ) first = false
				else         sb ++= """
   AND """
				sb ++= va.att.dbName ++= " = "
				Sql.literal( sb, values( va.index ) )
			}

		}

		//println( "SQL-->\n" + sb.toString + "\n" )
		Sql.commitUpdate( sb.toString )
	}
}


object SqlViewTest {

	/*

			what is the next step?

			  add a character

				  they enter name + realm



	 */



	def test = {
		val v = SqlView( """
SELECT #id, #name, #battle_group
  FROM realms#
 WHERE id < ?
""" )

		v.query( Integer.valueOf(2) ) foreach { tuple =>
			println( tuple.see( 1 ) + ":" + tuple.see( 2 ) )
		}

		/*
		val v2 = View( """
SELECT #id, #name, #gender
  FROM characters#
""" )

		println( "SQL-->\n" + v.sql )

		for ( l <- v.leaves ) {
			println( l.att.entity.name + "." + l.att.name )
		}

		v.query( Integer.valueOf(2) ) foreach { tuple =>
			println( tuple )
			tuple.save
		}

		val t2 = v2.makeTuple
		t2( 1 ) = "Fred"
		t2( 2 ) = Integer.valueOf( 1 )
		t2.save
		*/
	}
}

