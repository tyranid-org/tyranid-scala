/**
 * Copyright (c) 2008-2012 Tyranid <http://tyranid.org>
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

package org.tyranid.image

import java.io.File
import java.awt.color.{ ICC_ColorSpace, ICC_Profile }
import java.awt.image.{ Raster, WritableRaster, BufferedImage, ColorConvertOp }

import javax.imageio.{ ImageIO, IIOException }

import org.apache.sanselan.common.byteSources.ByteSourceFile
import org.apache.sanselan.formats.jpeg.JpegImageParser
import org.apache.sanselan.formats.jpeg.segments.UnknownSegment
import org.apache.sanselan.Sanselan

import org.tyranid.Imp._

class JpegReader {
  val COLOR_TYPE_RGB = 1
  val COLOR_TYPE_CMYK = 2
  val COLOR_TYPE_YCCK = 3

  var colorType = COLOR_TYPE_RGB
  var hasAdobeMarker = false

  def readImage( file:File ) : BufferedImage = {
    colorType = COLOR_TYPE_RGB
    hasAdobeMarker = false

    val stream = ImageIO.createImageInputStream(file)
    val iter = ImageIO.getImageReaders(stream)
    
    while ( iter.hasNext() ) {
      val reader = iter.next()
      reader.setInput(stream)

      var image:BufferedImage = null
      var profile:ICC_Profile = null
      
      try {
        image = reader.read(0)
      } catch {
        case e:IIOException =>
           colorType = COLOR_TYPE_CMYK
           checkAdobeMarker(file)
           profile = Sanselan.getICCProfile(file)
           val raster = reader.readRaster(0, null).as[WritableRaster]
           
           if ( colorType == COLOR_TYPE_YCCK)
             convertYcckToCmyk(raster)
             
           if (hasAdobeMarker)
             convertInvertedColors(raster)
             
           image = convertCmykToRgb(raster, profile)
       }

       return image;
    }

    return null;
  }

  def checkAdobeMarker( file:File ) {
    val parser = new JpegImageParser()
    val byteSource = new ByteSourceFile(file)
    
    val pass = new Array[Int](1)
    pass(0) = 0xffee
    
    val segments = parser.readSegments(byteSource, pass, true)
    
    if (segments != null && segments.size() >= 1) {
      val app14Segment = segments.get(0).as[UnknownSegment]
      
      val data = app14Segment.bytes
      
      if ( data.length >= 12 && data(0) == 'A' && data(1) == 'd' && data(2) == 'o' && data(3) == 'b' && data(4) == 'e') {
        hasAdobeMarker = true;
        val transform = app14Segment.bytes(11) & 0xff
        
        if (transform == 2)
          colorType = COLOR_TYPE_YCCK;
      }
    }
  }

  def convertYcckToCmyk( raster:WritableRaster ) {
    val height = raster.getHeight()
    val width = raster.getWidth()
    val stride = width * 4
    val pixelRow = new Array[Int](stride)
    
    for ( h <- 0 until height ) {
      raster.getPixels(0, h, width, 1, pixelRow)

      for ( x <- 0 until stride by 4 ) {
        var y = pixelRow(x)
        val cb = pixelRow(x + 1)
        val cr = pixelRow(x + 2)

        var c = (y + 1.402 * cr - 178.956)._i
        var m = (y - 0.34414 * cb - 0.71414 * cr + 135.95984)._i
        y = (y + 1.772 * cb - 226.316)._i

        if (c < 0) c = 0; else if (c > 255) c = 255
        if (m < 0) m = 0; else if (m > 255) m = 255
        if (y < 0) y = 0; else if (y > 255) y = 255

        pixelRow(x) = 255 - c
        pixelRow(x + 1) = 255 - m
        pixelRow(x + 2) = 255 - y
      }

      raster.setPixels(0, h, width, 1, pixelRow)
    }
  }

  def convertInvertedColors( raster:WritableRaster) {
    val height = raster.getHeight()
    val width = raster.getWidth()
    val stride = width * 4;
    val pixelRow = new Array[Int](stride)
    
    for ( h <- 0 until height ) {
      raster.getPixels(0, h, width, 1, pixelRow)
      
      for ( x <- 0 until stride ) {
        pixelRow(x) = 255 - pixelRow(x)
        raster.setPixels(0, h, width, 1, pixelRow)
      }
    }
  }

  def convertCmykToRgb( cmykRaster:Raster, acmykProfile:ICC_Profile ): BufferedImage = {
    val cmykProfile = ( acmykProfile == null ) ? ICC_Profile.getInstance( classOf[JpegReader].getResourceAsStream( "/ISOcoated_v2_300_eci.icc" ) ) |
                                                 acmykProfile
        
    val cmykCS = new ICC_ColorSpace(cmykProfile);
    val rgbImage = new BufferedImage(cmykRaster.getWidth(), cmykRaster.getHeight(), BufferedImage.TYPE_INT_RGB)
    val rgbRaster = rgbImage.getRaster()
    val rgbCS = rgbImage.getColorModel().getColorSpace()
    val cmykToRgb = new ColorConvertOp(cmykCS, rgbCS, null)
    cmykToRgb.filter(cmykRaster, rgbRaster)
    return rgbImage
  }
}