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

package org.tyranid.time

import java.util.Date

import scala.collection.mutable
import scala.concurrent.ops.spawn


case class Task( subject:String, var nextMs:Long, periodMs:Long, task: () => Unit )

object Scheduler {

  private val tasks = mutable.ArrayBuffer[Task]()

  def schedule( subject:String, start:Date, periodMs:Long )( task: () => Unit ) {

    tasks.synchronized {
      val idx = tasks.indexWhere( _.subject == subject )
      if ( idx != -1 ) tasks.remove( idx )
      tasks += Task( subject, start.getTime, periodMs, task )
    }
  }

  spawn {

    while ( true ) {

      val size =
        tasks.synchronized {
          tasks.sortBy( _.nextMs )
          tasks.size
        }

      val nowMs = System.currentTimeMillis

      for ( i <- 0 until size ) {
        val task = tasks( i )

        if ( nowMs >= task.nextMs ) {
          println( "Scheduler:  running " + task.subject + " at " + new Date().toString )

          try {
            task.task()
          } catch {
            case e =>
              println( "Problem running task:" )
              e.printStackTrace
          }

          task.nextMs +=  task.periodMs
        }
      }

      Thread.sleep( Time.OneMinuteMs )
    }
  }
}

