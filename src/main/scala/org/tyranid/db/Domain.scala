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

package org.tyranid.db

import net.liftweb.http.SHtml


/*
 * * *   D o m a i n s
 */

abstract class Domain {

	val idType = IdType.ID_COMPLEX

	val sqlName:String

  /**
   * Is this field automatic populated by the underlying DBMS.
   */
	def isAuto = false

	def see( v:AnyRef ) =
		v match {
		case null => ""
		case v    => v.toString
		}

  def ui( r:Record, va:ViewAttribute, opts:(String,String)* ) = SHtml.text( r s va.name, v => r( va.name ) = v )
}


//*******   I n t

abstract class DbIntish extends Domain {
	override val idType = IdType.ID_32
}

object DbInt extends DbIntish {
	val sqlName = "INT"
}

object DbIntSerial extends DbIntish {
	val sqlName = "SERIAL"
	override def isAuto = true
}


//*******   L o n g

abstract class DbLongish extends Domain {
	override val idType = IdType.ID_64
}

object DbLong extends DbLongish {
	val sqlName = "BIGINT"
}

object DbLongSerial extends DbLongish {
	val sqlName = "BIGSERIAL"
	override def isAuto = true
}




//*******   U r l

object DbUrl extends Domain {
	val sqlName = "VARCHAR(256)"
}


//*******   E m a i l

object DbEmail extends Domain {
	val sqlName = "VARCHAR(128)"
}

//*******   P h o n e  N u m b e r

object DbPhone extends Domain {
	val sqlName = "VARCHAR(10)"
}

//*******   P a s s w o r d

object DbPassword extends Domain {
	val sqlName = "VARCHAR(64)"
}


//*******   B o o l e a n

object DbBoolean extends Domain {
	val sqlName = "CHAR(1)"
}

//*******   C h a r

object DbChar {
	def apply( len: Int ) = new DbChar( len )
}

class DbChar( len: Int ) extends Domain {
	val sqlName = "CHAR(" + len + ")"
}


//*******   V a r C h a r

object DbVarChar {
	def apply( len: Int ) = new DbVarChar( len )
}

class DbVarChar( len: Int ) extends Domain {
	val sqlName = "VARCHAR(" + len + ")"
}


//*******   T e x t

object DbText extends Domain {
	val sqlName = "TEXT"
}


//*******   D a t e

object DbDate extends Domain {
	val sqlName = "DATE"
}

//*******   D a t e T i m e

object DbDateTime extends Domain {
	val sqlName = "TIMESTAMP WITHOUT TIME ZONE"
}

//*******   L i n k

object DbLink {
	def apply( toEn: Entity ) = new DbLink( toEn )
}

class DbLink( toEn: Entity ) extends Domain {
	lazy val sqlName = toEn.idType match {
		                case IdType.ID_32      => "INT"
		                case IdType.ID_64      => "BIGINT"
		                case IdType.ID_COMPLEX => throw new ModelException( toEn.name + " has a complex ID and cannot be linked to." )
										}

	override def see( v:AnyRef ) =
		v match {
		case null => ""
		case n:Number => toEn.labelFor( n.longValue )
		}
}


