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

package org.tyranid.cad

import java.io.{ ByteArrayOutputStream, ByteArrayInputStream, File, FileInputStream, InputStream } 
 
import org.tyranid.Imp._

import org.kabeja.parser.ParserBuilder
import org.kabeja.svg.{ RootLayerFilter, SVGGenerator, StyleAttributeFilter }
import org.kabeja.processing.PolylineConverter
import org.kabeja.xml.SAXPrettyOutputter

object AutoCad {

  def convertToSvg( dxfFile:File ) = {
    val xml = convertToSvgXml( dxfFile )
    val svgStartIdx = xml.isBlank ? -1 | xml.indexOf( "<svg" )
    xml.denull.substring( ( svgStartIdx > -1 ) ? svgStartIdx | 0 )    
  }
  
  def convertToSvgXml( dxfFile:File ) = {
    var fin:FileInputStream = new FileInputStream( dxfFile )
    convertInToSvgXml( fin )
  }
  
  def convertToSvg( dxfText:String ) = {
    val xml = convertToSvgXml( dxfText )
    val svgStartIdx = xml.isBlank ? -1 | xml.indexOf( "<svg" )
    xml.denull.substring( ( svgStartIdx > -1 ) ? svgStartIdx | 0 )    
  }
  
  def convertToSvgXml( dxfText:String ) = {
    val in:ByteArrayInputStream = new ByteArrayInputStream( dxfText.getBytes() )    
    convertInToSvgXml( in )
  }
  
  def convertInToSvgXml( in:InputStream ) = {
    try{
      val parser = ParserBuilder.createDefaultParser()
      parser.parse( in, "UTF-8" )
      val doc = parser.getDocument()
      val noprops = new java.util.HashMap()
      val pp = new PolylineConverter()
      pp.setProperties( noprops )
      pp.process( doc, noprops )
      val generator = new SVGGenerator()
      generator.setProperties( noprops )
      val filter1 = new RootLayerFilter()
      filter1.setProperties( noprops )
      val filter2 = new StyleAttributeFilter()
      filter2.setProperties( noprops )
      filter1.setContentHandler(filter2)
      
      val serializer = new SAXPrettyOutputter()
      val baos = new ByteArrayOutputStream()
      serializer.setOutput( baos )
      serializer.setProperties(noprops)
      
      filter2.setContentHandler(serializer)
      generator.generate(doc, filter1, null)
      
      new String( baos.toByteArray() )
    } catch {
      case e:Exception =>
        log( Event.Converter, "m" -> "Conversion of dxf to svg failed.", "ex" -> e )
        null
      case _:Throwable =>
        null
    } finally {
      if ( in != null )
        in.close
    }
  }
}