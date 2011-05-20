
package org.tyranid.image

import java.io.IOException
import java.net.URL

import scala.collection.mutable
import scala.collection.JavaConversions._

import org.tyranid.Imp._


object Image {

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
				e.printStackTrace

		} finally {
			if ( iis != null )
				iis.close
		}

		None
	}
}

