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

import java.awt.{ Color, RenderingHints }
import java.awt.image.{ BufferedImage }

import java.io.{ FileNotFoundException, IOException, File }
import java.net.URL

import javax.imageio.{ ImageIO, ImageReader }
import javax.imageio.stream.MemoryCacheImageInputStream

import scala.collection.mutable
import scala.collection.JavaConversions._
import scala.xml.NodeSeq
//import scala.xml.NodeSeq.Empty

import org.tyranid.Imp._
import org.tyranid.cloud.aws.{ S3, S3Bucket }
import org.tyranid.db.{ Domain, Record, Scope }
import org.tyranid.io.DbFile
import org.tyranid.ui.PathField
import org.tyranid.web.Html


    


// Might be able to get an image out of pdfbox:
/*
 List pages = document.getDocumentCatalog().getAllPages();
            Iterator iter = pages.iterator();
            while( iter.hasNext() )
            {
                PDPage page = (PDPage)iter.next();
                PDResources resources = page.getResources();
                Map images = resources.getImages();
                if( images != null )
                {
                    Iterator imageIter = images.keySet().iterator();
                    while( imageIter.hasNext() )
                    {
                        String key = (String)imageIter.next();
                        PDXObjectImage image = (PDXObjectImage)images.get( key );
                        String name = getUniqueFileName( key, image.getSuffix() );
                        System.out.println( "Writing image:" + name );
                        image.write2file( name );
                    }
                }
            }
*/

case class Dimensions( height:Int = -1, width:Int = -1,
                       minWidth:Int = -1,
                       maxWidth:Int = -1,
                       minHeight:Int = -1,
                       maxHeight:Int = -1 ) {
  def css = {
    val sb = new StringBuilder
    if ( height > 0 )
      sb ++= "height:" ++= height.toString ++= "px;"
    if ( width > 0 )
      sb ++= "width:" ++= width.toString ++= "px;"

    if ( minHeight > 0 )
      sb ++= "min-height:" ++= minHeight.toString ++= "px;"
    if ( maxHeight > 0 )
      sb ++= "max-height:" ++= maxHeight.toString ++= "px;"

    if ( minWidth > 0 )
      sb ++= "min-width:" ++= minWidth.toString ++= "px;"
    if ( maxWidth > 0 )
      sb ++= "max-width:" ++= maxWidth.toString ++= "px;"

    sb.toString
  }

  def scale( maxWidth:Int = -1, maxHeight:Int = -1, minWidth:Int = -1, minHeight:Int = -1 ) = {
    var w = width
    var h = height

    // if we know the dimensions, scale on the server side ... otherwise pass along the min-maxes along to the child dimensions class so we can use css styles

    if ( w == -1 || h == -1 ) {
      Dimensions( height = h, width = w, minWidth = minWidth, maxWidth = maxWidth, minHeight = minHeight, maxHeight = maxHeight )
    } else {

      if ( minWidth != -1 && w < minWidth ) {
        h = ( minWidth * h.toDouble / w ).toInt
        w = minWidth
      }

      if ( minHeight != -1 && h < minHeight ) {
        w = ( minHeight * w.toDouble / h ).toInt
        h = minHeight
      }

      if ( maxWidth != -1 && w > maxWidth ) {
        h = ( maxWidth * h.toDouble / w ).toInt
        w = maxWidth
      }

      if ( maxHeight != -1 && h > maxHeight ) {
        w = ( maxHeight * w.toDouble / h ).toInt
        h = maxHeight
      }

      Dimensions( height = h, width = w )
    }
  }

  def pixels = height * width

  lazy val portraitRank = {
    val h = height
    val w = width

    val ratio = w.toDouble / h.toDouble
    val pixels = h * w
    val idealPixels = 720*480

    var sizeMult = pixels.toDouble / idealPixels.toDouble

    if ( sizeMult > 1 )
      sizeMult = 1 / sizeMult

    val ratioMult = ( 3 - scala.math.abs( ratio - 1.77 ) )

    ratioMult * sizeMult
  }
}

class DbImageish( bucket:S3Bucket ) extends DbFile( bucket )

object DbImage {
  def apply( bucketPrefix:String ) = new DbImage( B.getS3Bucket( bucketPrefix ) )
}

case class DbImage( bucket:S3Bucket ) extends DbImageish( bucket ) {
}


object DbThumbnail {
  def apply( bucketPrefix:String ) = new DbThumbnail( B.getS3Bucket( bucketPrefix ) )
}

case class DbThumbnail( bucket:S3Bucket ) extends DbImageish( bucket ) {

  override def cell( s:Scope, f:PathField ) = {
    var v = f.path.s( s.rec ).trim
    if ( v.isBlank )
      v = f.blank.map( _.toString ) getOrElse null

    v.isBlank ? NodeSeq.Empty | <img src={ v } style="width:50px; height:50px;"/>
  }
  
  override def ui( s:Scope, f:PathField ): NodeSeq = {
    <div class='thumbnailImg'>
      <div><input id={ f.id } name={ f.id } type="file"/></div>
    </div>
  }
}

object Image {

  private val cache = mutable.Map[URL,Image]()

  def apply( url:URL ):Image = synchronized {
    cache.getOrElseUpdate(
      url,
      queryDimensions( url ) match {
      case Some( dims ) => Image( url, Some( dims ) )
      case None         => new Image( url )
      } )
  }

  
  def suffixes( contentType:String, path:String ) = {
    val suffixes = mutable.ArrayBuffer[String]()

    contentType match {
    case "image/bmp"  => suffixes += "bmp"
    case "image/gif"  => suffixes += "gif"
    case "image/jpeg" => suffixes += "jpeg"
    case "image/png"  => suffixes += "png"
    case _            =>
    }

    val suffix = path.suffix( '.' )
    if ( suffix.length > 0 && !suffixes.contains( suffix ) )
      suffixes += suffix

    // always try these, if the first choice fails
    if ( !suffixes.contains( "png"  ) ) suffixes += "png"
    if ( !suffixes.contains( "jpeg" ) ) suffixes += "jpeg"
    if ( !suffixes.contains( "jpg"  ) ) suffixes += "jpg"
    if ( !suffixes.contains( "gif"  ) ) suffixes += "gif"

    suffixes
  }

  def testImage( reader:ImageReader, iis:MemoryCacheImageInputStream ) =
    try {
      iis.seek( 0L )
      reader.setInput( iis )

      Some( Dimensions( width  = reader.getWidth( reader.getMinIndex ),
                        height = reader.getHeight( reader.getMinIndex ) ) )

    } catch {
      case e:Exception =>
        //e.printStackTrace
        None

    } finally {
      reader.dispose
    }

  def queryDimensions( file:java.io.File ):Option[Dimensions] = {
    var iis:MemoryCacheImageInputStream = null

    try {
      val in = new java.io.FileInputStream( file )
      iis = new MemoryCacheImageInputStream( in )
      val contentType = org.tyranid.io.File.mimeTypeFor( file.getName )

      for ( suffix <- suffixes( contentType, file.getName );
            reader <- ImageIO.getImageReadersBySuffix( suffix );
            dimensions <- testImage( reader, iis ) )
        return Some( dimensions )

      println( "dimensions test failed for " + file.getAbsolutePath )
    } catch {
    case e:FileNotFoundException =>
      println( "Image.getDimensions ... File not found: " + file.getAbsolutePath )

    case e:IOException =>
      e.printStackTrace

    } finally {
      if ( iis != null )
        iis.close
    }

    None
  }
  
	def queryDimensions( url:URL ):Option[Dimensions] = {
		var iis:MemoryCacheImageInputStream = null

		try {
   		val urlc = url.openConnection
   		urlc.setConnectTimeout( 45000 )
			
			iis = new MemoryCacheImageInputStream( urlc.getInputStream )

			for ( suffix <- suffixes( urlc.getContentType, url.getPath );
						reader <- ImageIO.getImageReadersBySuffix( suffix );
	      		dimensions <- testImage( reader, iis ) )
				return Some( dimensions )

		  println( "dimensions test failed for " + url.getPath )
		} catch {
		case e:FileNotFoundException =>
      println( "Image.getDimensions ... 404 on " + url.toString )

		case e:IOException =>
			e.printStackTrace

		} finally {
			if ( iis != null )
				iis.close
		}

		None
	}

  private def analyze( pageUrl:URL, names:Seq[String] ) = names.distinct.map( _.safeUrl( base = pageUrl ) ).filter( _ != null ).map( apply ).filter( img => img != null && img.pixels > 0 )

  def bestForPage( pageUrl:URL, html:Html ):Image = {

    val ogImages = analyze( pageUrl, html.ogImages )
    if ( ogImages.size > 0 )
      return ogImages( 0 )

    if ( true ) return null

    val images = analyze( pageUrl, html.images )

    if ( images.size > 0 )
      images.maxBy( _.portraitRank )
    else
      null
  }

  def optionsForPage( pageUrl:URL, html:Html ):Seq[Image] = {
    val ogImagePaths = html.ogImages
    val ogImages = analyze( pageUrl, ogImagePaths )

    ogImages ++ analyze( pageUrl, html.images -- ogImagePaths ).filter( _.portraitRank > 0.005 ).sortBy( _.portraitRank )
  }
}

case class Image( url:URL, dims:Option[Dimensions] = None ) {
  def pixels = dims.flatten( _.pixels, 0 )
  def portraitRank = dims.flatten( _.portraitRank, 0.0 )
  def dimensions( maxWidth:Int = -1, maxHeight:Int = -1 ) = dims.get.scale( maxWidth, maxHeight )
  def cssDimensions( maxWidth:Int = -1, maxHeight:Int = -1 ) = dimensions( maxWidth, maxHeight ).css
}


case class Thumbnail( name:String, code:String, dimensions:Dimensions ) {
  def url( tid:String ) = "/io/thumb/" + tid + "/" + code
}

object Thumbnail {
  val Large  = Thumbnail( "large",  "l", Dimensions( 260, 169 ) ) // Dashboard
  val Medium = Thumbnail( "medium", "m", Dimensions( 140,  91 ) ) // Timeline
  val Small  = Thumbnail( "small",  "s", Dimensions( 100,  65 ) ) // Project header
  val Tiny   = Thumbnail( "tiny",   "t", Dimensions(  40,  40 ) ) // Dashboard drop-down

  def generate( originalFile:File, thumbW:Int, thumbH:Int, thumbnailFile:File = null ):File = {
    val image = ImageIO.read( originalFile )
    
    if ( image == null ) {
      println( "Error: Unable to get thumb from file: " + originalFile.getAbsolutePath )
      return null
    }
    
    val imageWidth  = image.getWidth( null )
    val imageHeight = image.getHeight( null )

    val thumbFile = ( thumbnailFile == null ) ? File.createTempFile( originalFile.getName(), ".tmp" ) | thumbnailFile 

    // Do not create thumbs that are bigger than the original image, or if the original image is the right size, then use that
    if ( ( thumbW > imageWidth && thumbH > imageHeight ) || ( imageWidth == thumbW && imageHeight == thumbH ) ) {
      org.apache.commons.io.FileUtils.copyFile( originalFile, thumbFile )
    } else {
      //val thumbRatio:Double = double2Double( thumbW ) / double2Double( thumbH )
      val imageRatio:Double = double2Double( imageWidth ) / double2Double( imageHeight )
      
      var thumbHeight = thumbH
      var thumbWidth  = thumbW
      
      if ( imageWidth > imageHeight )
        thumbHeight = ( thumbWidth / imageRatio )._i
      else
        thumbWidth = ( thumbHeight * imageRatio )._i
      
      if ( imageWidth < thumbWidth && imageHeight < thumbHeight ) {
        thumbWidth = imageWidth
        thumbHeight = imageHeight
      } else if ( imageWidth < thumbWidth ) {
        thumbWidth = imageWidth
      } else if ( imageHeight < thumbHeight ) {
        thumbHeight = imageHeight
      }
  
      val thumbImage = new BufferedImage( thumbWidth, thumbHeight, BufferedImage.TYPE_INT_RGB )
      val graphics2D = thumbImage.createGraphics()
      
      graphics2D.setBackground( Color.WHITE )
      graphics2D.setPaint( Color.WHITE );
      graphics2D.fillRect( 0, 0, thumbWidth, thumbHeight )
      graphics2D.setRenderingHint( RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR )
      graphics2D.drawImage( image, 0, 0, thumbWidth, thumbHeight, null )
       
      // Only crop if needed
      if ( thumbWidth < thumbW || thumbHeight < thumbH ) {
//      if ( imageWidth < thumbW || imageHeight < thumbH ) {
        ImageIO.write( thumbImage, "JPG", thumbFile )
      } else {
        var (x,y,w,h) = ( 0, 0, thumbW, thumbH )
        
        if ( thumbWidth > thumbW ) {
          x = ( thumbWidth - thumbW ) / 2 
        } else if ( thumbHeight > thumbH ) {
          y = ( thumbHeight - thumbH ) / 2
        }
        
        try {
          ImageIO.write( thumbImage.getSubimage( x, y, w, h ), "JPG", thumbFile ) 
        } catch {
          case e =>
            println( "ERROR: " + e.getMessage() )
            println( "orig: " + thumbWidth + ", " + thumbHeight )
            println( "cropped: " + x + ", " + y + ", " + w + ", " + h )
        }
      }
    }
    
    return thumbFile
  }  
}

