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

package org.tyranid.time

import java.util.{ Calendar, Date }

import scala.collection.mutable

import com.mongodb.{ DBObject, WriteConcern }

import org.tyranid.Imp._
import org.tyranid.db.mongo.{ MongoEntity, MongoRecord, DbMongoId }
import org.tyranid.db.{ DbChar, DbDateTime, DbInt, DbLong, DbBoolean }
import org.tyranid.db.mongo.Imp._
import org.tyranid.json.JsModel
import org.tyranid.net.Ip
import org.tyranid.web.{ Weblet, WebContext }



/*

    Scheduled Tasks across multiple servers

    X.  add support to Task for local vs. global tasks
 
    X.  be able to generate an identifier for a particular run of a scheduled task:  taskRunId

        X. how do we sync run times across multiple servers ? ... via rounding and UTC times

    X.  when a server has free time, it checks to see if any tasks are due and if they are in the ScheduledTask table

        if not, add the run task to the scheduled task table along with the server id

    /.  add runs UI in admin

        /. add "active" flag to TaskStats so you can turn off a task from the console

        /. add a "run immediate" flag to TaskStats so you run a task from the console

 */

object TaskStats extends MongoEntity( "a06t" ) {
  override lazy val dbName = "taskStats"

  type RecType = TaskStats
  override def convert( obj:DBObject, parent:MongoRecord ) = new TaskStats( obj, parent )
  
  "_id"         is DbInt             as "Task ID" is 'id;

  "runs"        is DbInt             ;

  "lastSv"      is DbChar(32)        as "Last Server ID";
  "lastStart"   is DbDateTime        as "Last Start Time";
  "lastEnd"     is DbDateTime        as "Last End Time";

  "active"      is DbBoolean         ;
  "runRequest"  is DbBoolean         ;
}

class TaskStats( obj:DBObject, parent:MongoRecord ) extends MongoRecord( TaskStats.makeView, obj, parent ) {
}



object TaskRun extends MongoEntity( "a05t" ) {
  type RecType = TaskRun
  override def convert( obj:DBObject, parent:MongoRecord ) = new TaskRun( obj, parent )
  
  "_id"         is DbChar(32)        as "Task Run ID" is 'id;

  "t"           is DbInt             as "Task ID";

  "sv"          is DbChar(32)        as "Server ID";
  "start"       is DbDateTime        as "Start Time";
  "end"         is DbDateTime        as "End Time";
}

class TaskRun( obj:DBObject, parent:MongoRecord ) extends MongoRecord( TaskRun.makeView, obj, parent ) {
}



case class Task( id:Int, subject:String, var nextMs:Long, periodMs:Long, var enabled:Boolean, task: () => Unit, skipWeekend:Boolean = false, allServers:Boolean = false ) {
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

  def runIdFor( ms:Long ) = {

    val c = new Date( ms ).toUtcCalendar

    periodMs match {
    case Time.OneMinuteMs =>
      c.set( Calendar.SECOND, 0 )

    case d if d == 2 * Time.OneMinuteMs =>
      c.set( Calendar.SECOND, 0 )
      val minutes = c.minute
      if ( minutes % 2 == 1 )
        c.set( Calendar.MINUTE, minutes - 1 )

    case d if d == 5 * Time.OneMinuteMs =>
      c.set( Calendar.SECOND, 0 )
      val minutes = c.minute
      val subtraction = minutes % 5
      if ( subtraction != 0 )
        c.set( Calendar.MINUTE, minutes - subtraction )

    case Time.OneHourMs =>
      c.set( Calendar.SECOND, 0 )
      c.set( Calendar.MINUTE, 0 )

    case Time.OneDayMs =>
      c.setMidnight

    case d =>
      throw new RuntimeException( "Schedule interval of " + d + " unsupported." )
    }

    c.set( Calendar.MILLISECOND, 0 )

    "%02d,%04d.%02d.%02d %02d:%02d".format( id, c.year, c.month + 1, c.dayOfMonth, c.hour24, c.minute )
  }
}

object Scheduler {
  private[time] val tasks = mutable.ArrayBuffer[Task]()

  def schedule( id:Int, subject:String, start:Date, periodMs:Long, active:Boolean = true, skipWeekend:Boolean = false, allServers:Boolean = false )( task: () => Unit ) {

    tasks.synchronized {
      //val sTask = TaskRun( ScheduledTask.db.findOrMake( Mobj( "subject" -> subject ) ) )
      
      //if ( sTask.isNew )
      //  sTask.save
        
      val idx = tasks.indexWhere( _.subject == subject )
      if ( idx != -1 ) tasks.remove( idx )
      val t = Task( id, subject, start.getTime, periodMs, active, task, skipWeekend = skipWeekend, allServers = allServers )

      // run this just to verify it's a valid interval at boot time (exception will be thrown if this fails)
      t.runIdFor( System.currentTimeMillis )

      tasks += t
    }
  }

  def start = {
    background {
      while ( true ) {
        val size =
          tasks.synchronized {
            tasks.sortBy( _.nextMs )
            tasks.size
          }
  
        val nowMs = System.currentTimeMillis
  
        for ( i <- 0 until size;
              task = tasks( i );
              if task.enabled;
              nextMs = task.nextMs;
              if nowMs >= nextMs ) {

          if ( task.allServers ) {
            task.run( manual = false )
          } else {
            val taskRunId = task.runIdFor( nextMs )

            val startOn = new Date
            val sv = Ip.Host.toString

            val tr = Mobj(
              "_id"   -> taskRunId,
              "sv"    -> sv,
              "start" -> startOn,
              "t"     -> task.id
            )

            val wr = TaskRun.db.insert( tr, WriteConcern.NONE )

            val code = wr.getField( "code" )
spam( "----- CODE " + code )

            if ( code != 11000 ) {
spam( "----- updating " + task.id )
              TaskStats.db.update(
                Mobj( "_id" -> task.id ),
                Mobj( $set -> Mobj( "lastSv" -> sv, "lastStart" -> startOn ), $inc -> Mobj( "runs" -> 1 ) ),
                true,
                false
              )

              task.run( manual = false )

              val endAt = new Date
              TaskRun  .db.update( Mobj( "_id" -> taskRunId ), Mobj( $set -> Mobj( "end" -> endAt     ) ) )
              TaskStats.db.update( Mobj( "_id" -> task.id   ), Mobj( $set -> Mobj( "lastEnd" -> endAt ) ) )
            }
          }
        }
  
        Thread.sleep( Time.OneMinuteMs )
      }
    }
  }
}

object Schedulelet extends Weblet {

  private def jsonTasks = {
    val taskStats = TaskStats.db.find().toSeq

    for ( task <- Scheduler.tasks ) yield {

      val ( runs, lastSv, lastStart, lastEnd ) =
        taskStats.find( _.i( '_id ) == task.id ) match {
        case Some( stats ) => ( stats.i( 'runs ), stats.s( 'lastSv ), stats.t( 'lastStart ), stats.t( 'lastEnd   ) )
        case _             => ( 0,                "",                 "",                    ""                    )
        }

      Map(
        "id"        -> task.subject,
        "status"    -> task.active,
        "runs"      -> runs,
        "lastSv"    -> lastSv,
        "lastStart" -> lastStart,
        "lastEnd"   -> lastEnd,
        "nextRun"   -> ( task.active ? new Date( task.nextMs ) | 0 )
      )
    }
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
        val ts = TaskStats.getById( task.id )
        val active =
          if ( ts != null ) {
            if ( ts.has( 'active ) )
              ts.b( 'active )
            else
              task.active
          } else {
            task.active
          }

        TaskStats.db.update( Mobj( "_id" -> task.id ), Mobj( $set -> Mobj( "active" -> active ) ), true, false )
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

