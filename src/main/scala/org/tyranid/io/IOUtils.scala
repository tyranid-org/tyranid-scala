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

package org.tyranid.io

import java.io.{ IOException, FileOutputStream, InputStream, OutputStream }
import java.net.URL

import scala.collection.mutable
import scala.collection.JavaConversions._
import scala.xml.NodeSeq

import net.liftweb.http.{ FileParamHolder, SHtml }

import org.tyranid.Imp._
import org.tyranid.cloud.aws.{ S3, S3Bucket }
import org.tyranid.db.{ Domain, Record, Scope }
import org.tyranid.ui.Field

object IOUtils {
  def transfer( input: InputStream, out: OutputStream ) {
  	val buffer = new Array[Byte](8192)
  		
  	def transfer() {
  		val read = input.read( buffer )
  		if ( read >= 0 ) {
  			out.write( buffer, 0, read )
  			transfer()
  		}
  	}
  		
  	transfer()
  }
}


