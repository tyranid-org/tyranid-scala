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

package org.tyranid

import scala.collection.mutable
import scala.collection.mutable.Buffer

import org.tyranid.Imp._

trait QuickCache {
  val qcache = mutable.Map[String,AnyRef]()
  def incV( key:String ) = putV( key, qcache.getOrElse( key, 0 )._i + 1 )
  def decV( key:String ) = putV( key, qcache.getOrElse( key, 0 )._i - 1 )
  def getV( key:String ) = qcache.getOrElse( key, null )
  def getVOrElse( key:String, any:Any ) = qcache.getOrElse( key, any.as[AnyRef] )
  def getVOrElseUpdate(  key:String, any:Any ) = qcache.getOrElseUpdate( key, any.as[AnyRef] )
  def putV( key:String, value:Any ) = qcache.put( key, value.as[AnyRef] )
  def clearCache( key:String = null ) = key.isBlank ? qcache.clear | qcache.remove( key )
}