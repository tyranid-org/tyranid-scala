/**
 *   Copyright (c) 2008-2014 Tyranid <http://tyranid.org>
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

package org.tyranid.cloud.aws

import com.amazonaws.services.ec2.AmazonEC2Client
import com.amazonaws.services.ec2.model.{ DescribeInstancesRequest, Instance }

import collection.JavaConversions._

import org.tyranid.Imp._

object EC2 {
  val ec2 = new AmazonEC2Client( B.awsCredentials )
    
  lazy val myInstance:Instance = {
    val instanceId = B.ec2InstanceId
      
    if ( instanceId.notBlank ) {
      val describeReq = new DescribeInstancesRequest
      describeReq.setInstanceIds( Seq( instanceId ) )
        
      val describeRes = ec2.describeInstances( describeReq )        
      val reservations = describeRes.getReservations
      
      if ( reservations.size == 1 ) {
        val instances = reservations.get( 0 ).getInstances
     
        if ( instances.size == 1 )
          instances.get( 0 )  
        else
          null
      } else {
        null
      }   
    } else {
      null
    }
  }
  
  def myInstanceTags = ( myInstance == null ) ? null | myInstance.getTags
  
  def myInstanceHasTag( tagName: String, value:String = null ): Boolean = {
    val tags = myInstanceTags
    
    if ( tags == null )
      return false
      
    for ( tag <- tags )
      if ( tag.getKey == tagName && ( value.isBlank ? true | tag.getValue == value ) )
        return true
        
    return false
  }
}