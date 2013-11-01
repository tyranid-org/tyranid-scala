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

package org.tyranid.document

import java.io.{ File, FileOutputStream, IOException }

import org.tyranid.Imp._
import org.tyranid.cloud.aws.{ S3, S3Bucket }
import org.tyranid.content.Content
import org.tyranid.db.{ DbInt, DbChar }
import org.tyranid.db.ram.RamEntity
import org.tyranid.db.tuple.{ Tuple, TupleView }
import org.tyranid.db.mongo.MongoEntity
import org.tyranid.db.mongo.Imp._
import org.tyranid.math.Base36

object Converter {
  lazy val bucket = B.getS3BucketByFullName( "convert-volerro-com" )
}

object ConvertState extends RamEntity( tid = "a0Nv" ) {
  type RecType = ConvertState
  override def convert( view:TupleView ) = new ConvertState( view )
  
  "_id"     is DbInt      is 'id;
  "name"    is DbChar(64) is 'label;

  override val addNames = Seq( "_id", "name" )
  
  val ConvertingId = 1
  val ConvertedId  = 2

  val Converting = add( ConvertingId, "Converting" )
  val Converted  = add( ConvertedId,  "Converted" )
}

case class ConvertState( override val view:TupleView ) extends Tuple( view )

object DocumentConverterType extends RamEntity( tid = "a12v" ) {
  type RecType = DocumentConverterType
  override def convert( view:TupleView ) = new DocumentConverterType( view )
  
  "_id"     is DbInt      is 'id;
  "name"    is DbChar(64) is 'label;

  override val addNames = Seq( "_id", "name" )
  
  val ZencoderId = 1
  val MadeToPrintId  = 2

  val Zencoder    = add( ZencoderId,    "Zencoder" )
  val MadeToPrint = add( MadeToPrintId, "MadeToPrint" )
}

case class DocumentConverterType( override val view:TupleView ) extends Tuple( view )
