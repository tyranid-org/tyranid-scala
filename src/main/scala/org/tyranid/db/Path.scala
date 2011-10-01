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

/*

    +.  rewrite Completion to use Paths

    +.  write unit tests in terms of Tyranid models ... i.e. locale, etc.

    +.  implement flatten

    +.  differencing

    +.  change log writes


 */


trait PathNode {
  def name:String
  def label:String
}

case class ArrayIndex( idx:Int ) extends PathNode {

  def name  = idx.toString
  def label = name
}

trait Path {

  def name  = ( 0 until pathSize ).map( i => pathAt( i ).name ).mkString( "." )
  def label = ( 0 until pathSize ).map( i => pathAt( i ).label ).mkString( " . " )

  def pathSize:Int
  def pathAt( idx:Int ):PathNode

  def leaf:ViewAttribute = pathAt( pathSize - 1 ).asInstanceOf[ViewAttribute]

  def matches( other:Seq[String], ostart:Int = 0 ) = {

    !( ostart until other.size ).exists( i => other( i ) != pathAt( i-ostart ).name )
  }
}

case class MultiPath( nodes:PathNode* ) extends Path {

  def pathSize = nodes.length
  def pathAt( idx:Int ) = nodes( idx )
}

case class PathValue( path:Path, value:Any )

object Path {

  def flatten( r:Record ):Seq[PathValue] = {
    // TODO
    Nil
  }
}

