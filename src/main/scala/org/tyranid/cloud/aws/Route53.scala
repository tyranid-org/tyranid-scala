/**
 *   Copyright (c) 2008-2013 Tyranid <http://tyranid.org>
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

import com.amazonaws.services.route53.AmazonRoute53Client
import com.amazonaws.services.route53.model.{ CreateHostedZoneRequest, GetHostedZoneRequest, ListResourceRecordSetsRequest, HostedZoneConfig, ChangeAction, RRType, ResourceRecordSet, ResourceRecord, ChangeBatch, Change, ChangeResourceRecordSetsRequest }

import java.util.ArrayList

import scala.collection.JavaConversions._

import scala.collection.mutable
import scala.collection.mutable.Buffer

import org.tyranid.Imp._

object Route53 {
  private val route53 = new AmazonRoute53Client( B.awsCredentials )
  
  def subdomainExists( subdomain:String, recType:String = "A" ):Boolean = {
    var request = new ListResourceRecordSetsRequest( B.domainRoute53ZoneId )
                    .withStartRecordName( subdomain )
                    .withStartRecordType( recType )
                    .withMaxItems( "1000" )
    
    val result = route53.listResourceRecordSets( request )
    val rrSets = result.getResourceRecordSets()
    
    for ( rr <- rrSets ) {
      if ( rr.getName.startsWith( subdomain ) )
        return true
    }
    
    false
  }
  
  def createSubdomain( subdomain:String ) {
    if ( subdomainExists( subdomain ) ) {
      println( "Already exists!" )
    } else {
      val rr =  new ResourceRecord
      rr.setValue( "50.19.108.91" )
      
      val rrSet = new ArrayList[ResourceRecord]
      rrSet.add( rr )
      
      val rrs = new ResourceRecordSet
      rrs.setName( subdomain + "." + B.domain )
      rrs.setType( RRType.A )
      rrs.setTTL( 300l )
      rrs.setResourceRecords( rrSet )
      
      val change = new Change
      change.setAction( ChangeAction.CREATE )
      change.setResourceRecordSet( rrs )
      
      val changes = new ArrayList[Change]
      changes.add( change )
      
      val changeBatch = new ChangeBatch
      changeBatch.setChanges( changes )
      
      val result = route53.changeResourceRecordSets( new ChangeResourceRecordSetsRequest()
                            .withHostedZoneId( B.domainRoute53ZoneId )
                            .withChangeBatch( changeBatch ) )
                            
      println( "status: " + result.getChangeInfo().getStatus() )
    }
  }
}

