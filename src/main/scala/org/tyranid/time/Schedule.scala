/**
\ * Copyright (c) 2008-2013 Tyranid <http://tyranid.org>
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
import org.tyranid.json.JsModel
import org.tyranid.web.{ Weblet, WebContext }


case class Task( subject:String, var nextMs:Long, periodMs:Long, var active:Boolean, task: () => Unit, skipWeekend:Boolean = false ) {
  var runs = 0
  var lastRun:Date = null

  def run( manual:Boolean ) {
    if ( !( skipWeekend && new Date().isWeekend ) ) { 
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
}

object Scheduler {

  private[time] val tasks = mutable.ArrayBuffer[Task]()

  def schedule( subject:String, start:Date, periodMs:Long, active:Boolean = true, skipWeekend:Boolean = false )( task: () => Unit ) {

    tasks.synchronized {
      val idx = tasks.indexWhere( _.subject == subject )
      if ( idx != -1 ) tasks.remove( idx )
      tasks += Task( subject, start.getTime, periodMs, active, task, skipWeekend = skipWeekend )
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

  private def jsonTasks = {
    val tasks = new mutable.ArrayBuffer[Map[String,Any]]
    
    for ( task <- Scheduler.tasks ) {
      tasks += Map(
           "id" -> task.subject,
           "status" -> task.active,
           "runs" -> task.runs,
           "lastRun" -> ( ( task.lastRun == null ) ? 0 | task.lastRun ),
           "nextRun"  -> ( task.active ? new Date( task.nextMs ) | 0 )
      )
    }
    
    tasks
  }
  
  def handle( web:WebContext ) {

    if ( !T.user.isGod )
      _404

    def task = {
      val subject = web.req.s( 'task )
      Scheduler.tasks.find( _.subject == subject ) 
    }

    rpath match {
    case "/" =>      
      web.jsRes( 
        JsModel(
           Map(
             "tasks" -> jsonTasks
           ),
           name = "scheduler"
       ) )
    case "/run" =>
      task foreach { task =>
        background { task.run( manual = true ) }
        T.session.notice( "Running: " + task.subject )
      }
      
      web.jsRes()
    case "/toggle" =>
      task foreach { task =>
        task.active = !task.active
      }

      web.jsRes( 
        JsModel(
           Map(
             "tasks" -> jsonTasks
           ),
           name = "scheduler"
       ) )
    case _ =>
      _404
    }
  }
}

