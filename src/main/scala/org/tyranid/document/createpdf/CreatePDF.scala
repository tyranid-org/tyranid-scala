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

package org.tyranid.document.createpdf

import scala.collection.mutable
import scala.collection.JavaConversions._

import scala.xml.{ Unparsed, NodeSeq }


import java.io.{ File, FileOutputStream, FileInputStream }

//import com.gargoylesoftware.htmlunit.html._
//import com.gargoylesoftware.htmlunit._

import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.app.AppStat
import org.tyranid.db.mongo.Imp._
import org.tyranid.json.Json
import org.tyranid.http.Http
import org.tyranid.math.Base64
import org.tyranid.session.Session
import org.tyranid.web.{ Weblet, WebContext }

object CreatePDF {
  /*
  val debug = true
  
  val START_PAGE = "https://createpdf.acrobat.com/signin.html"
  val APP_PAGE = "https://createpdf.acrobat.com/app.html"
  val userFirstName = "Kevin"
    
  val supportedFormats = List( "AI", "INDD", "PSD", "PS", "BMP", "GIF", "JPEG", "JPG", "PNG", "TIF", "TIFF", "XLS", "XLSX", "PPT", "PPTX", "PUB", 
      "DOC", "DOCX", "RTF", "ODT", "ODS", "ODP", "ODG", "ODF", "SXW", "SXI", "SXD", "STW", "TXT" )

  def supports( filename:String ) = {
    val suffix = filename.suffix( '.' )
    
    supportedFormats.contains( suffix.toUpperCase )
  }
  
  def exportPDF( file:File, fileSize:Long, filename:String, obj:DBObject ): Boolean = {
    val wc = new WebClient( BrowserVersion.FIREFOX_17 )
    val options = wc.getOptions
    options.setCssEnabled( true )
    options.setTimeout( 0 )
    options.setJavaScriptEnabled( true )
    options.setThrowExceptionOnScriptError( false )  
    
    val app = new CreatePDF( wc ).signin  
    
    if ( app != null  ) {
      val headers = app.getWebResponse().getResponseHeaders()
      
      
      for ( header <- headers ) {
        println( header.getName() + "=" + header.getValue() )
      }
      
      //wc.waitForBackgroundJavaScript( 5000 )
    }
    
    true
  }
}

class CreatePDF( wc:WebClient ) {
  val debug = CreatePDF.debug
  
  def signin = {
    val topPage = wc.getPage[HtmlPage]( CreatePDF.START_PAGE )
    
    val page = topPage.getFrames().get(0).getEnclosedPage().as[HtmlPage]
    
    if ( debug ) {
      println(  "========== SIGNIN PAGE START ===========" )
      println( page.asXml )
      println(  "========== SIGNIN PAGE END ===========" )
    }
    
    val form = page.getElementById( "signin" ).as[HtmlForm]
    
    val unFld = page.getElementById( "username" ).as[HtmlInput]
    unFld.setValueAttribute( B.createPDFUsername )
    
    val pwFld = page.getElementById( "password" ).as[HtmlPasswordInput]
    pwFld.setValueAttribute( B.createPDFPassword )
    
    val sbtButton = page.getElementById( "sign_in" ).asInstanceOf[HtmlButton]
        
    val response = sbtButton.click().asInstanceOf[HtmlPage]
    
    val app = wc.getPage[HtmlPage]( CreatePDF.APP_PAGE )
    
    // Wait 5 secs for the JS to finish   
    wc.waitForBackgroundJavaScript( 5000 )
    
    if ( debug ) {
      println(  "========== APP PAGE START ===========" )
      println( app.asXml )
      println(  "========== APP PAGE END ===========" )
    }
    
    val divEl = app.getElementById( "user-name" ).as[HtmlDivision]
    
    if ( divEl == null ) {
      println( "Unable to get name!" )
      null
    } else {
      val firstName = divEl.asText.trim
      
      if ( firstName != CreatePDF.userFirstName )
        println( "First name is not " + CreatePDF.userFirstName + ", it is " + firstName )    
        
      app
    }
  } 
  */   
}
