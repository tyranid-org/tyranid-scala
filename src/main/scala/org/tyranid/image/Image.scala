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

import java.io.{ FileNotFoundException, IOException }
import java.net.URL

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
    val v = f.path.s( s.rec ).trim
    v.isBlank ? NodeSeq.Empty | <img src={ f.path.s( s.rec ) } style="width:50px; height:50px;"/>
  }
  
  override def ui( s:Scope, f:PathField ): NodeSeq = {
    <div class='thumbnail'>
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
      case Some( ( width, height ) ) => Image( url, Some( width ), Some( height ) )
      case None                      => new Image( url )
      } )
  }

	def queryDimensions( url:URL ):Option[( Int, Int )] = {
    import javax.imageio.{ ImageIO, ImageReader }
    import javax.imageio.stream.MemoryCacheImageInputStream

		var iis:MemoryCacheImageInputStream = null

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
      if ( !suffixes.contains( "png" ) )  suffixes += "png"
		  if ( !suffixes.contains( "jpeg" ) ) suffixes += "jpeg"
      if ( !suffixes.contains( "gif" ) )  suffixes += "gif"

		  suffixes
	  }

	  def test( reader:ImageReader ) =
		  try {
			  iis.seek( 0L )
     	  reader.setInput( iis )

			  Some( ( reader.getWidth( reader.getMinIndex ),
		            reader.getHeight( reader.getMinIndex ) ) )

		  } catch {
			  case e:Exception =>
			    //e.printStackTrace
			    None

		  } finally {
			  reader.dispose
		  }

		try {
   		val urlc = url.openConnection
   		urlc.setConnectTimeout( 45000 )
			
			iis = new MemoryCacheImageInputStream( urlc.getInputStream )

			for ( suffix <- suffixes( urlc.getContentType, url.getPath );
						reader <- ImageIO.getImageReadersBySuffix( suffix );
	      		dimensions <- test( reader ) )
				return Some( dimensions )

		  println( "dimensions test failed for " + url.getPath )
		} catch {
		case e:FileNotFoundException =>
      println( "Http.getDimensions ... 404 on " + url.toString )

		case e:IOException =>
			e.printStackTrace

		} finally {
			if ( iis != null )
				iis.close
		}

		None
	}

  private def analyze( pageUrl:URL, names:Seq[String] ) = names.distinct.map( url => apply( new URL( pageUrl, url ) ) ).filter( img => img != null && img.pixels > 0 )

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

case class Image( url:URL, width:Option[Int] = None, height:Option[Int] = None ) {

  def pixels = width.getOrElse( 0 ) * height.getOrElse( 0 )

  lazy val portraitRank = {
    val h = height.get
    val w = width.get

    val ratio = w.toDouble / h.toDouble
    val pixels = h * w
    val idealPixels = 720*480

    var sizeMult = pixels.toDouble / idealPixels.toDouble

    if ( sizeMult > 1 )
      sizeMult = 1 / sizeMult

    val ratioMult = ( 3 - scala.math.abs( ratio - 1.77 ) )

    ratioMult * sizeMult
  }

  def dimensions( maxWidth:Int = -1, maxHeight:Int = -1 ) = {
    var bw = width.get
    var bh = height.get

    var w = bw
    var h = bh

    if ( maxWidth != -1 && w > maxWidth ) {
      h = maxWidth * h / w
      w = maxWidth
    }

    if ( maxHeight != -1 && h > maxHeight ) {
      w = maxHeight * w / h
      h = maxHeight
    }

    ( w, h )
  }

  def cssDimensions( maxWidth:Int = -1, maxHeight:Int = -1 ) = {
    val ( w, h ) = dimensions( maxWidth, maxHeight )

    "width:" + w + "px;height:" + h + "px;"
  }
}

