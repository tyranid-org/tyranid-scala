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

package org.tyranid.image

import java.io.IOException
import java.net.URL

import scala.collection.mutable
import scala.collection.JavaConversions._
import scala.xml.NodeSeq

import net.liftweb.http.{ FileParamHolder, SHtml }

import org.tyranid.Imp._
import org.tyranid.cloud.aws.{ S3, S3Bucket }
import org.tyranid.db.{ Domain, Record, Scope }
import org.tyranid.io.DbFile
import org.tyranid.ui.Field

object DbImage {

  def apply( bucketPrefix:String ):DbImage = new DbImage( Tyr.s3Buckets( bucketPrefix ) )
}

class DbImage( bucket:S3Bucket ) extends DbFile( bucket ) {

  override def ui( s:Scope, f:Field, opts:(String,String)* ): NodeSeq =
    /*SHtml.text( s.rec s f.va, v => s.rec( f.va ) = v, "class" -> "textInput" ) ++ */
    <div class='thumbnail'>
      { 
      if ( ( s.rec s f.va ).isBlank ) { // TODO:  Replace this with a blank/default image for ALL images
//        <img src='https://d33lorp9dhlilu.cloudfront.net/generic-image.png' style='float:left;'/>
      } else {
//        <img src={ url( s.rec s f.va ) } style='float:left;'/>
      }
      }
      <div> { SHtml.fileUpload( save( s.rec, f ) _ ) }</div>
    </div>
}

object Image {

  private val cache = mutable.Map[URL,Image]()

  def apply( url:URL ):Image = synchronized {
    cache.getOrElseUpdate(
      url,
      queryDimensions( url ) match {
      case Some( ( width, height ) ) => Image( url, Some( width ), Some( height ) )
      case None                      => Image( url )
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
			    e.printStackTrace
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
			case e:IOException =>
				e.log

		} finally {
			if ( iis != null )
				iis.close
		}

		None
	}
}

case class Image( url:URL, width:Option[Int] = None, height:Option[Int] = None ) {


}

