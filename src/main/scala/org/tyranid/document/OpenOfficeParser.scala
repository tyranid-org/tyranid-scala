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

package org.tyranid.document

import scala.xml.{ Text, Node }

import java.util.zip.{ ZipFile, ZipEntry }

import org.tyranid.Imp._

object OpenOfficeParser {
  def getText( fileName:String ):String = {
    var parser = new OpenOfficeParser()
    
	val zipFile = new ZipFile( fileName )
	val entries = zipFile.entries()
	  
	while ( entries.hasMoreElements() ) {
	  val entry = entries.nextElement().as[ZipEntry]
	                               
	  if ( entry.getName == "content.xml" ) {
	    parser.getText( zipFile.getInputStream( entry ).asString.toXml )
	    return parser.textBuffer.toString
	  }
	}
	  
	return null
  }      
}

class OpenOfficeParser {
  val textBuffer = new StringBuilder()
  
  def getText( e:Node ) {
    val prefix = e.prefix
    val name = e.label
    
	val elementName = prefix.isBlank ? name | prefix + ":" + name
            
    if ( elementName.startsWith( "text" ) ) {
      name match {
      case "tab" =>
        textBuffer ++= "\t"
      case "s" =>  // add space for text:s
        textBuffer ++= " "
      case _ =>
        for ( child <- e.child ) {
          if ( child.is[Text] )  
            textBuffer ++= child.toString() // might have to look for another method if this returns the outside elements too
          else
            getText( child ) // Recursively process the child element
          
          if ( name == "p" )
            textBuffer ++=  "\n"
        }                    
      }
    } else {
      for ( child <- e.child )
        getText( child )                    
    }
  }
}