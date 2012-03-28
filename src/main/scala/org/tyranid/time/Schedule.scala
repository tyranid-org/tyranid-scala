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

import org.tyranid.Imp._
import org.tyranid.web.Weblet


case class Task( subject:String, var nextMs:Long, periodMs:Long, var active:Boolean, task: () => Unit ) {
  var runs = 0
  var lastRun:Date = null

  def run = {
    println( "Scheduler:  running " + subject + " at " + new Date().toString )

    val start = System.currentTimeMillis

    if ( periodMs > Time.OneHourMs )
      log( Event.Scheduler, "m" -> ( "running: " + subject ) )

    trylog {
      task()
    }

    runs += 1
    lastRun = new Date

    if ( periodMs > Time.OneHourMs )
      log( Event.Scheduler, "m" -> ( "completed: " + subject ), "du" -> ( System.currentTimeMillis - start ) )

    while ( nextMs < System.currentTimeMillis )
      nextMs += periodMs
  }
}

object Scheduler {

  private val tasks = mutable.ArrayBuffer[Task]()

  def schedule( subject:String, start:Date, periodMs:Long, active:Boolean = true )( task: () => Unit ) {

    tasks.synchronized {
      val idx = tasks.indexWhere( _.subject == subject )
      if ( idx != -1 ) tasks.remove( idx )
      tasks += Task( subject, start.getTime, periodMs, active, task )
    }
  }

  def ui( relative:Weblet ) = tasks.synchronized {

    <table class="dtable tablesort">
     <thead>
      <tr><th></th><th>Status</th><th></th><th>Task</th><th>Runs</th><th>Last Run</th></tr>
     </thead>
     <tbody>
      { for ( task <- tasks ) yield
      <tr>
       <td><a class="greenBtn" href={ relative.wpath + "/scheduler/run?task=" + task.subject }>Run</a></td>
       <td>{ if ( task.active ) "On" else "Off" }</td>
       <td>{
         if ( task.active )
           <a class="redBtn" href={ relative.wpath + "/scheduler/off?task=" + task.subject } style="width:100px;">Turn Off</a>
         else
           <a class="greenBtn" href={ relative.wpath + "/scheduler/on?task=" + task.subject } style="width:100px;">Turn On</a>
       }</td>
       <td>{ task.subject }</td>
       <td>{ task.runs }</td>
       <td>{ task.lastRun != null |* task.lastRun.toDateTimeStr }</td>
      </tr>
     }</tbody>
    </table>
  }

  def handle( relative:Weblet ) = {
    val t = T

    def task = {
      val subject = t.web.req.s( 'task )
      tasks.find( _.subject == subject ) 
    }

    if ( relative.rpath.startsWith( "/scheduler/" ) ) {
      relative.rpath.substring( 10 ) match {
      case "/run" =>
        task foreach { task =>
          background { task.run }
          t.session.notice( "Running: " + task.subject )
        }
        t.web.redirect( relative.wpath + "/scheduler" )

      case "/off" =>
        task foreach { task =>
          task.active = false
          t.session.notice( "Deactivated: " + task.subject )
        }

        t.web.redirect( relative.wpath + "/scheduler" )

      case "/on" =>
        task foreach { task =>
          task.active = true
          t.session.notice( "Activated: " + task.subject )
        }
        t.web.redirect( relative.wpath + "/scheduler" )

      case _ =>
        relative._404
      }

      true
    } else {
      false
    }
  }

  background {

    while ( true ) {
      
      val size =
        tasks.synchronized {
          tasks.sortBy( _.nextMs )
          tasks.size
        }

      val nowMs = System.currentTimeMillis

      for ( i <- 0 until size ) {
        val task = tasks( i )

        if ( task.active && nowMs >= task.nextMs )
          task.run
      }

      Thread.sleep( Time.OneMinuteMs )
    }
  }
}

