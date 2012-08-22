package org.tyranid.pdf

import java.io.{ File, FileOutputStream, IOException }

import com.pdfcrowd._;  //pointing at volerro/lib

import org.tyranid.math.Base36

import org.tyranid.Imp._
      
object Pdf {
  def urlToFile( url:String, outFile:File ) = {
    var fileStream:FileOutputStream = null
    
	  try {
	    fileStream = new FileOutputStream( outFile )     
	 
	    // create an API client instance
	    val client:Client = new Client( B.pdfCrowdName, B.pdfCrowdKey )
	    client.useSSL( true )
	        
	    // convert a web page and save the PDF to a file
	    client.convertURI( url, fileStream )
	  } catch {
	    case why:PdfcrowdError => 
	      println( why.getMessage )
	    case e:IOException =>
	      e.printStackTrace();
	  } finally {
	    if ( fileStream != null )
	      fileStream.close
	  }
  }
}