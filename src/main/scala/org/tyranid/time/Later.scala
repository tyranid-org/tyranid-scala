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
 */

package org.tyranid.time

import java.util.Date

import scala.collection.mutable

import org.tyranid.Imp._
import org.tyranid.web.{ Weblet, WebContext }


object Later {
  private val laters = mutable.ArrayBuffer[Later]()

  def process {

    val now = new Date

    for ( later <- laters.synchronized { laters.extract( _.when >= now ) };
          if later.applies )
      later.send
  }

  def +=( later:Later ) = laters.synchronized {
    laters += later
  }
}

trait Later {
  def when:Date
  def applies:Boolean
  def send:Unit

}

