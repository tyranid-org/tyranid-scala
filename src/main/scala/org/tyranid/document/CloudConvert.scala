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

package org.tyranid.document

import java.io.File

import org.tyranid.Imp._
import org.tyranid.json.Json
import org.tyranid.http.Http

object CloudConvert {
  val GET_PROCESS_URL = "https://api.cloudconvert.org/process"
    
}

case class CloudConvertApp( apiKey:String ) {
  def convert( inFile:File, outFile:File ) = {
    // Get the URL used for processing
    var json = Json.parse( 
        Http.GET( CloudConvert.GET_PROCESS_URL, Map(
          "inputformat" -> inFile.getName.suffix( '.' ).toLowerCase,
          "outputformat" -> outFile.getName.suffix( '.' ).toLowerCase,
          "apikey" -> apiKey
          ) ).s ).get( 0 )
            
    val processUrl = json.s( 'url )
    
    // Send the input file
    Http.POST_FILE( processUrl, inFile, inFile.length, inFile.getName(), params = Map( "format" -> outFile.getName.suffix( '.' ).toLowerCase ) )
    
    // Wait for results
    var finished = false
    while ( !finished ) {
      Thread.sleep( 1000 )
      json = Json.parse( Http.GET( processUrl ).s ).get( 0 )
      
      val step = json.s( 'stop )
      
      step match {
        case "finished" =>
          finished = true
        case s =>
          println( "Step: " + s + ", percent: " + json.s( 'percent ) )
      }
    }
    
    val output = json.get( 'output )
    
    val file = Http.GET_File( output.s( 'url ) )
    
    file.renameTo( outFile )
    
    outFile
  }
}