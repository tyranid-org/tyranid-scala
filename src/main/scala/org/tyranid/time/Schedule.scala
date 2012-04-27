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

package org.tyranid.time

import java.util.Date

import scala.collection.mutable

import org.tyranid.Imp._
import org.tyranid.web.{ Weblet, WebContext }


case class Task( subject:String, var nextMs:Long, periodMs:Long, var active:Boolean, task: () => Unit ) {
  var runs = 0
  var lastRun:Date = null

  def run( manual:Boolean ) = {
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

    if ( !manual )
      while ( nextMs < System.currentTimeMillis )
        nextMs += periodMs
  }
}

object Scheduler {

  private[time] val tasks = mutable.ArrayBuffer[Task]()

  def schedule( subject:String, start:Date, periodMs:Long, active:Boolean = true )( task: () => Unit ) {

    tasks.synchronized {
      val idx = tasks.indexWhere( _.subject == subject )
      if ( idx != -1 ) tasks.remove( idx )
      tasks += Task( subject, start.getTime, periodMs, active, task )
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
          task.run( manual = false )
      }

      Thread.sleep( Time.OneMinuteMs )
    }
  }
}

object Schedulelet extends Weblet {

  def handle( web:WebContext ) {

    if ( !T.user.isGod )
      _404

    def task = {
      val subject = web.req.s( 'task )
      Scheduler.tasks.find( _.subject == subject ) 
    }

    rpath match {
    case "/" =>
      shell(
        <table class="dtable tablesort {sortlist: [[1,1],[2,0]]}">
         <thead>
          <tr><th class="{sorter: false}"></th><th>Status</th><th>Task</th><th>Runs</th><th>Last Run</th><th>Next Run</th></tr>
         </thead>
         <tbody>
          { for ( task <- Scheduler.tasks ) yield
          <tr>
           <td><a class="greenBtn" href={ wpath + "/run?task=" + task.subject }>Run</a></td>
           <td>{
             if ( task.active )
               <a class="greenBtn" href={ wpath + "/off?task=" + task.subject } style="width:50px;">On</a>
             else
               <a class="redBtn" href={ wpath + "/on?task=" + task.subject } style="width:50px;">Off</a>
           }</td>
           <td>{ task.subject }</td>
           <td>{ task.runs }</td>
           <td>{ task.lastRun != null |* task.lastRun.toDateTimeStr }</td>
           <td>{
             if ( task.active )
               new Date( task.nextMs ).toDateTimeStr
             else
               "---"
           }</td>
          </tr>
         }</tbody>
        </table> )

    case "/run" =>
      task foreach { task =>
        background { task.run( manual = true ) }
        T.session.notice( "Running: " + task.subject )
      }
      web.redirect( wpath )

    case "/off" =>
      task foreach { task =>
        task.active = false
        T.session.notice( "Deactivated: " + task.subject )
      }

      web.redirect( wpath )

    case "/on" =>
      task foreach { task =>
        task.active = true
        T.session.notice( "Activated: " + task.subject )
      }
      web.redirect( wpath )

    case _ =>
      _404
    }
  }
}

