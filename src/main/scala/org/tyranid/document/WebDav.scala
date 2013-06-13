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

package org.tyranid.document

import java.io.{ BufferedOutputStream, File, FileOutputStream }

import scala.collection.JavaConversions._

import com.github.sardine.{ DavResource, Sardine, SardineFactory }
import com.github.sardine.impl.{ SardineException }

import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.db.{ DbInt, DbChar, DbBoolean, DbDateTime }
import org.tyranid.db.mongo.{ MongoEntity, MongoRecord }

object WebDavResource extends MongoEntity(tid = "a0Uw") {
  "_id"     is DbInt       is 'id is 'client;
  "name"    is DbChar(64)  is 'label is 'client;
  "path"    is DbChar(128) is 'client;
  "on"      is DbDateTime  is 'client;
  "type"    is DbChar(10)  is 'client;
  "isDir"   is DbBoolean   is 'client;
  "size"    is DbInt       is 'client;
  "mod"     is DbDateTime  is 'client;
  
  def convert( r:DavResource ) = {
    val cr = WebDavResource.make
    
    cr( '_id )   = r.getPath.hashCode
    cr( 'name )  = r.getName
    cr( 'path )  = r.getPath
    cr( 'on )    = r.getCreation
    cr( 'type )  = r.getContentType
    cr( 'isDir ) = r.isDirectory
    cr( 'size )  = r.getContentLength
    cr( 'mod )   = r.getModified
    cr
  }
}

class WebDavResource( obj:DBObject, parent:MongoRecord ) extends MongoRecord( WebDavResource.makeView, obj, parent ) {} 

trait WebDav {
  val WEBDAV_URL:String = ""
  
  def webDavUrl( username:String ) = WEBDAV_URL
  
  def list( path:String, username:String, password:String ) = {
    try {
      SardineFactory.begin( username, password ).list( webDavUrl( username ) + path.or( "/" ) ).tail.map( r => WebDavResource.convert( r ) ).toSeq
    } catch {
      case se:SardineException =>
        throw new RuntimeException( se.getResponsePhrase() )
    }
  }
  
  def getFile( path:String, username:String, password:String ) = {
    // Must look like [some path]/file.ext
    val file = File.createTempFile( path.suffix( '/' ).prefix( '.' ), "." + path.suffix( '.' ) )
    val sardine = SardineFactory.begin( username, password )
    sardine.enableCompression()

    try {
      val out = new BufferedOutputStream( new FileOutputStream( file ) )
      
      // Sardine does not like dealing with spaces, so adjust the URL to something it can handle 
      val in = sardine.get( webDavUrl( username ) + path.split( "/" ).map( part => java.net.URLEncoder.encode( part, "UTF-8" ).replaceAll( java.util.regex.Pattern.quote("+"), "%20" ) ).mkString( "/" ) )

      in.transferTo( out )

      in.close
      out.flush
      out.close
    } catch {
      case se: SardineException =>
        println( "Sarding exception: " + se.getResponsePhrase() )
        se.printStackTrace
        throw new RuntimeException( se.getResponsePhrase() )
      case e: Throwable =>
        e.printStackTrace
        throw e
    }
    
    file
  }
}

object Cubby extends WebDav {
  override val WEBDAV_URL = "https://webdav.cubby.com/"    
}

object FilesAnywhere extends WebDav {
  override def webDavUrl( username: String ) = "https://webfolder.filesanywhere.com/" + username
}

object Box extends WebDav {
  override val WEBDAV_URL = "https://dav.box.com/dav/"  
}