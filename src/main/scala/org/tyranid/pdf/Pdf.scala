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

package org.tyranid.pdf

import java.io.{ File, FileOutputStream, IOException }

import com.pdfcrowd.{ Client, PdfcrowdError }

import org.tyranid.Imp._
import org.tyranid.app.AppStat
import org.tyranid.cloud.aws.S3
import org.tyranid.content.Content
import org.tyranid.document.{ ConvertState, Converter, DocumentConverterType }
import org.tyranid.db.mongo.Imp._

object Pdf {
  val lock = ""
  val lock2 = ""
    
  def urlToFile( url:String, outFile:File, enableHyperlinks:Boolean = false, username:String = null, password:String = null ) = {
    var doConvertApi = false
    
    // PDF Crowd API only allows one at a time
    lock.synchronized {
      var useJavascript = true
      var retry = true
      var fileStream:FileOutputStream = null
      var retryCount = 0
        
      while ( retry ) {
        retryCount = retryCount + 1
        
    	  try {
    	    fileStream = new FileOutputStream( outFile )     
    	 
    	    // create an API client instance
    	    val client:Client = new Client( B.pdfCrowdName, B.pdfCrowdKey )
    	    client.useSSL( true )
    	        
    	    // convert a web page and save the PDF to a file
          if ( B.onePagePdf )
            client.setPageHeight( -1 )
    	    
          client.enableHyperlinks( enableHyperlinks )
          
          if ( !useJavascript )
            client.enableJavaScript( false )
          
          val finalUrl = ( username.notBlank && password.notBlank ) ? {
            val idx = url.toLowerCase().indexOf( "://" )
            
            if ( idx > -1 ) {
              url.substring( 0, idx + 3 ) + username + ":" + password +  "@" + url.substring( idx + 3  ) 
            } else {
              ( username + ":" + password +  "@" + url )
            }
          } | url
          
          //println( finalUrl )
          if ( retryCount == 1 )
            AppStat.PdfCrowdSend
            
    	    client.convertURI( finalUrl, fileStream )
    	    
    	    if ( !useJavascript )
            B.sendMessage( "When importing the URL [" + url + "], there was an error.  It was tried again with Javascript turned off on the page and it was successful.  The result may not exactly reflect what the page looked like.", T.user.tid )
          
          retry = false
          AppStat.PdfCrowdSuccess
    	  } catch {
    	    case why:PdfcrowdError =>
    	      why.statusCode match {
    	        case js if useJavascript => // 510 = 413 Timed out. Can't load the specified URL.
    	          println( "PDF failure, status code: " + js )
    	          useJavascript = false
                AppStat.PdfCrowdRetry
    	        case sc =>
    	          AppStat.PdfCrowdFailure
                retry = false
                
    	          if ( sc == 502 ) {
    	            doConvertApi = true
    	            println( "PDF Crowd failed, trying convertApi." )
    	          } else {
      	        //503 - Simultaneous API calls from a single IP are not allowed.
                  val msg = why.getMessage
                  println( "status code: " + sc )
                  throw new RuntimeException( "Status code: " + sc + ", " + msg.firstSuffix( '-' ) or msg )
    	          }
    	        }
    	    case e:IOException =>
    	      AppStat.PdfCrowdFailure
    	      retry = false
            println( e.getMessage )
            throw new RuntimeException( e )
    	  } finally {
    	    if ( fileStream != null )
    	      fileStream.close
    	  }
      }
    }
 
    if ( doConvertApi )
      convertApiUrl( url, outFile, username, password )
    else 
      outFile
  }
  
  def convertApiUrl( url:String, outFile:File, username:String = null, password:String = null ) = {
    // Only do one at a time
    lock2.synchronized {
      var useJavascript = true
      var retry = true
      var fileStream:FileOutputStream = null
      var retryCount = 0

      
      while ( retry ) {
        retryCount = retryCount + 1

        try {
          fileStream = new FileOutputStream( outFile )     
       
          val idx = url.lastIndexOf( '?' )
          val finalUrl = ( idx > -1 ) ? ( url.substring(0, idx) + java.net.URLEncoder.encode( url.substring( idx ), "UTF-8" ) ) | url
          
          if ( retryCount == 1 )
            AppStat.PdfConvertApiSend
          
          val result = "http://do.convertapi.com/Web2Pdf".POST(
              Map( "ApiKey" -> B.convertApiKey,
                   "Scripts" -> ( useJavascript ? "true" | "false" ),
                   "AuthUsername" -> username,
                   "AuthPassword" -> password,
                   "CUrl" -> finalUrl
              ) )
          
          val res = result.response
          val entity = res.getEntity
          
          if ( entity != null )
            entity.getContent.transferTo( fileStream, true )
             
          // convert a web page and save the PDF to a file
//          if ( B.onePagePdf )
//            client.setPageHeight( -1 )
          
          if ( !useJavascript )
            B.sendMessage( "When importing the URL [" + url + "], there was an error.  It was tried again with Javascript turned off on the page and it was successful.  The result may not exactly reflect what the page looked like.", T.user.tid )
            
          retry = false
          AppStat.PdfConvertApiSuccess
        } catch {
          case e:IOException =>
            AppStat.PdfConvertApiFailure
            println( e.getMessage )
            
            if ( useJavascript ) {
              println( "PDF failure, turning off JS and trying again." )
              useJavascript = false
              AppStat.PdfConvertApiRetry
            } else {
              AppStat.PdfConvertApiFailure
              retry = false
              throw new RuntimeException( e )
            }
        } finally {
          if ( fileStream != null )
            fileStream.close
        }
      }
      
      outFile
    }
  }

  def convert( content:Content, inFile:File, fileSize:Long, fileName:String ): Boolean = {
    // Already converted or not a document
    if ( !B.documentEntity.hasTid( content.tid ) || content.i( 'convertState ) == ConvertState.ConvertedId )
      return false
      
    val ext = fileName.suffix( '.' ).toLowerCase
    
    ext match {
      case "indd" =>        
        S3.copy( B.filesBucket, content.s3Path, Converter.bucket, "In/" + content.id._s + "." + ext )
        B.documentEntity.db.update( Mobj( "_id" -> content.id ), Mobj( $set -> Mobj( "convertState" -> ConvertState.ConvertingId, "converter" -> DocumentConverterType.MadeToPrintId ) ) )
        content( "convertState" ) = ConvertState.ConvertingId
        content( "converter" ) = DocumentConverterType.MadeToPrintId
        
        // No need to save, I did the update myself
        return false
      case _ =>
        
    }
    
    false
  }
  
  def finishedConverting( content:Content ): Boolean = {
    val convertState = content.i( 'convertState )
    
    convertState match {
      case ConvertState.ConvertingId =>
        val ext = content.s( 'fileName ).suffix( '.' ).toLowerCase
        
        ext match {
          case "indd" =>
            val bucket = Converter.bucket
            val outPath = "Out/" + content.id._s + ".pdf"
            val complete = S3.exists( bucket, outPath )
            
            if ( complete ) {
              // Download it and send it to the document previewer
              val convertedFile = S3.getFile( bucket, outPath, ".pdf" )
              B.docPreviewApp.upload( convertedFile, convertedFile.length, convertedFile.getName, content )

              // Remove it from input and output buckets
              val outFiles = S3.getFilenames( bucket, prefix = "Out/" + content.id._s )
              S3.deleteAll( bucket, outFiles )
              //S3.delete( bucket, "In/" + content.id._s + "." + ext )
              
              // Update the content to let us know it was converted
              content( "convertState" ) = ConvertState.ConvertedId
              content.save
              
              convertedFile.delete
              return true
            }
            
            // Not complete yet
            return false
          case _ =>
        }
      case _ =>
    }
    
    false
  }
  
  def convertApiPptx( inFile:File, outFile:File ) = {
    // Only do one at a time
    var fileStream:FileOutputStream = null
  
    try {
      fileStream = new FileOutputStream( outFile )     
   
      AppStat.PdfConvertApiSend
      
      val result = org.tyranid.http.Http.POST_FILE( "http://do.convertapi.com/PowerPoint2Pdf", inFile, inFile.length, inFile.getName, Map( "ApiKey" -> B.convertApiKey ) )
      val res = result.response
      val entity = res.getEntity
      
      if ( entity != null )
        entity.getContent.transferTo( fileStream, true )
         
      AppStat.PdfConvertApiSuccess
    } catch {
      case e:IOException =>
        AppStat.PdfConvertApiFailure
        println( e.getMessage )
        throw new RuntimeException( e )
    } finally {
      if ( fileStream != null )
        fileStream.close
    }
    
    outFile
  }

  def convertApiImage( inFile:File, outFile:File ) = {
    // Only do one at a time
    var fileStream:FileOutputStream = null
  
    try {
      fileStream = new FileOutputStream( outFile )     
   
      AppStat.PdfConvertApiSend
      
      val result = org.tyranid.http.Http.POST_FILE( "http://do.convertapi.com/Image2Pdf", inFile, inFile.length, inFile.getName, Map( "ApiKey" -> B.convertApiKey ) )
      val res = result.response
      val entity = res.getEntity
      
      if ( entity != null )
        entity.getContent.transferTo( fileStream, true )
         
      AppStat.PdfConvertApiSuccess
    } catch {
      case e:IOException =>
        AppStat.PdfConvertApiFailure
        println( e.getMessage )
        throw new RuntimeException( e )
    } finally {
      if ( fileStream != null )
        fileStream.close
    }
    
    outFile
  }
  
}