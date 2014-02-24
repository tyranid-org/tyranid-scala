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

package org.tyranid.secure

import org.scalatest.FunSuite

import org.tyranid.Imp._


class MultipassSuite extends FunSuite {

  /*

  from:  http://support.assistly.com/customer/portal/questions/17123-multipass-sso-troubleshooting

api key: multipass_api_key

site key: assistly_subdomain

salted api key + site key: multipass_api_keyassistly_subdomain

hash (hex): c63fc05fd223981a5e13b7ddac7091cf414b2c56

salted hash (hex): c63fc05fd223981a5e13b7ddac7091cf

---

json data: {"uid":"123abc","customer_email":"testuser@yoursite.com","customer_name":"Test User","expires":"2011-07-06 23:28:40Z"}

data after double XOR first block of 16 bytes with IV (hex): 3452100737717602575d41413016405522637573746f6d65725f656d61696c223a22746573747573657240796f7572736974652e636f6d222c22637573746f6d65725f6e616d65223a22546573742055736572222c2265787069726573223a22323031312d30372d30362032333a32383a34305a227d

pad size: 10

data after padding (hex): 3452100737717602575d41413016405522637573746f6d65725f656d61696c223a22746573747573657240796f7572736974652e636f6d222c22637573746f6d65725f6e616d65223a22546573742055736572222c2265787069726573223a22323031312d30372d30362032333a32383a34305a227d0a0a0a0a0a0a0a0a0a0a

data after encrpyting (hex): 38682d3a02a29072a244e7836f34420f5be548732d8bf86359db2166717fceb94fa9527cd283b065df104640098c6f4ea788e3ce53a20ae5c9bc12c9500a98286b012d7e10abc858c91d419bf68107e480269ef5b4f6f935eae8cd4f220ce4dbfb6517826cbe94c769ec0097091c58532e85c0160d1e541c6c721eb010cb02d3

data after base64 encoding: OGgtOgKikHKiROeDbzRCD1vlSHMti/hjWdshZnF/zrlPqVJ80oOwZd8QRkAJjG9Op4jjzlOiCuXJvBLJUAqYKGsBLX4Qq8hYyR1Bm/aBB+SAJp71tPb5NerozU8iDOTb+2UXgmy+lMdp7ACXCRxYUy6FwBYNHlQcbHIesBDLAtM=

data after converting to URL safe variant: OGgtOgKikHKiROeDbzRCD1vlSHMti_hjWdshZnF_zrlPqVJ80oOwZd8QRkAJjG9Op4jjzlOiCuXJvBLJUAqYKGsBLX4Qq8hYyR1Bm_aBB-SAJp71tPb5NerozU8iDOTb-2UXgmy-lMdp7ACXCRxYUy6FwBYNHlQcbHIesBDLAtM

finished multipass string: OGgtOgKikHKiROeDbzRCD1vlSHMti_hjWdshZnF_zrlPqVJ80oOwZd8QRkAJjG9Op4jjzlOiCuXJvBLJUAqYKGsBLX4Qq8hYyR1Bm_aBB-SAJp71tPb5NerozU8iDOTb-2UXgmy-lMdp7ACXCRxYUy6FwBYNHlQcbHIesBDLAtM

   */


  test( "multipass" ) {

    val multipass = new Multipass( "assistly_subdomain", "multipass_api_key" )

    val rslt = multipass.json( """{"uid":"123abc","customer_email":"testuser@yoursite.com","customer_name":"Test User","expires":"2011-07-06 23:28:40Z"}""" )

    val expected = "OGgtOgKikHKiROeDbzRCD1vlSHMti_hjWdshZnF_zrlPqVJ80oOwZd8QRkAJjG9Op4jjzlOiCuXJvBLJUAqYKGsBLX4Qq8hYyR1Bm_aBB-SAJp71tPb5NerozU8iDOTb-2UXgmy-lMdp7ACXCRxYUy6FwBYNHlQcbHIesBDLAtM"

    multipass.props( "uid", "redirect", "email", "name", "org" -> "acme" )

    assert( rslt  === expected )
  }
}

