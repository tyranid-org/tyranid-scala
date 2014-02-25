package org.tyranid.cloud.stackdriver


/**
 * Copyright (c) 2008-2014 Tyranid <http://tyranid.org>
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

import com.stackdriver.api.custommetrics.CustomMetricsPoster

import java.util.Date

import scala.collection.mutable

import org.tyranid.Imp._

case class StackDriverApp( apiKey:String ) {
  
  def poster = new CustomMetricsPoster( apiKey )
  
  def ifProd( block: => Unit ) {
    if ( B.PRODUCTION ) {
      background( "stackdriver custom stat" ) {
        block
      }
    }
  }
  
  def sendGlobal( metricName:String, value:Double, when:Date = null ) = ifProd {
    ( when == null ) ? poster.sendMetricDataPoint( metricName, value ) | poster.sendMetricDataPoint( metricName, value, when )
  }
  
  def sendGlobals( metricName:String, values:Double* ) {
    val poster = this.poster
    
    for ( value <- values )
      poster.sendMetricDataPoint( metricName, value )
  }

  def sendGlobalsWhen( metricName:String, when:Date, values:Double* ) = ifProd {
    val poster = this.poster
    
    for ( value <- values )
      ( when == null ) ? poster.sendMetricDataPoint( metricName, value ) | poster.sendMetricDataPoint( metricName, value, when ) 
  }
  
  def sendDiffGlobals( map:Map[String,Double], when:Date = null ) = ifProd {
    val poster = this.poster
    
    for ( key <- map.keys )
      ( when == null ) ? poster.sendMetricDataPoint( key, map.get( key ).get ) | poster.sendMetricDataPoint( key, map.get( key ).get, when )
  }

  def sendMe( metricName:String, value:Double, when:Date = null ) = ifProd {
    ( when == null ) ? poster.sendInstanceMetricDataPoint( metricName, value, B.ec2InstanceId ) | poster.sendInstanceMetricDataPoint( metricName, value, when, B.ec2InstanceId )
  }
  
  def sendMes( metricName:String, values:Double* ) = ifProd {
    val poster = this.poster
    
    for ( value <- values )
      poster.sendInstanceMetricDataPoint( metricName, value, B.ec2InstanceId )
  }
  
  def sendMesWhen( metricName:String, when: Date, values:Double* ) = ifProd {
    val poster = this.poster
    
    for ( value <- values )
      ( when == null ) ? poster.sendInstanceMetricDataPoint( metricName, value, B.ec2InstanceId ) | poster.sendInstanceMetricDataPoint( metricName, value, when, B.ec2InstanceId )
  }
  
  def sendDiffMes( map:Map[String,Double], when:Date = null ) = ifProd {
    val poster = this.poster
    
    for ( key <- map.keys )
      ( when == null ) ? poster.sendInstanceMetricDataPoint( key, map.get( key ).get, B.ec2InstanceId ) | poster.sendInstanceMetricDataPoint( key, map.get( key ).get, when, B.ec2InstanceId ) 
  }
  
  lazy val METRIC_WEB_SESSION_COUNT = "web_session_count"
  lazy val METRIC_LITE_COUNT = B.liteAppName + "_count"
  
  //// Tyranid specific calls
    
  def webSessionCount( value:Double ) {
    sendMe( METRIC_WEB_SESSION_COUNT, value )
  }
  
  def sendLiteCount = ifProd {
    import org.tyranid.session.WebSession
    
    val sessions = WebSession.sessions.values.seq
    
    sendMe( METRIC_LITE_COUNT, sessions.filter( ws => {
      val tyrSession = ws.getAttribute( WebSession.HttpSessionKey ).as[org.tyranid.session.Session]
      tyrSession != null && tyrSession.b( "lite" )
    } ).size )
  }  
}

