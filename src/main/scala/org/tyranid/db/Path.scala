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

import java.util.Date

import org.bson.types.ObjectId
import com.mongodb.BasicDBList

import scala.collection.mutable
import scala.xml.NodeSeq

import org.tyranid.Imp._
import org.tyranid.bson.BsonObject
import org.tyranid.db.es.Es
import org.tyranid.logic.{ Invalid, Valid }
import org.tyranid.ui.{ UiObj }


/*
 * * *  Path
 */

//case class Path( nodes:Seq[PathNode] )

/*

    need to account for array index positions in the path


    class ArrayIndex extends PathNode

    write unit tests in terms of Tyranid models ... i.e. locale, etc.
 */

trait Path {

  def pathSize:Int
  def pathAt( idx:Int ):ViewAttribute

  def leaf:ViewAttribute
}

case class PathValue( path:Path, value:Any )

case class MultiPath( vas:ViewAttribute* ) extends Path {

  def pathSize = vas.length
  def pathAt( idx:Int ) = vas( idx )

  def leaf = vas.last
}


object Path {

  def flatten( r:Record ):Seq[PathValue] = {

    Nil
  }
}

