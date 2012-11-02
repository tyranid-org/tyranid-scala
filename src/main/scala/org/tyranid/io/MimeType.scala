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

package org.tyranid.io

import scala.collection.mutable

import org.tyranid.Imp._


case class MetaMimeType( id:String, baseLabel:String, includes:Seq[MetaMimeType] ) {

  def types = MimeType.types.filter( mt => mt.meta == this || includes.contains( mt.meta ) )

  lazy val mimeTypes = types.map( _.mimeType ).sorted.distinct

  lazy val extensions = types.flatMap( _.extensions )

  lazy val label = baseLabel + " (" + extensions.sorted.map( "." + _ ).mkString( ", " ) + ")"

  def matchesExtension( ext:String ) = extensions.contains( ext )
}

object MetaMimeType {

  val values = mutable.Buffer[MetaMimeType]()

  def add( id:String, label:String, includes:Seq[MetaMimeType] = Nil ) = {
    val v = MetaMimeType( id, label, includes )
    values += v
    v
  }

  def byId( id:String ) = values.find( _.id == id ) getOrElse null


  //val customIcons = List( "aep","ai","aiff","avi","bmp","dmg","doc","docm","docx","dot","dotx","eps","fla","htm","html","indd","mov","mp3","mp4","mpeg","pdf","pps","ppsm","ppsx","ppt","pptm","pptx","psd","qt","rar","rtf","tiff","txt","wav","xla","xlam","xls","xlsb","xlsm","xlsx","xlt","xltm","xltx","zip" ) // "gif","jpg","png" 

  val AdobeIllustrator    = add( "illustrator",  "Adobe Illustrator" )
  val BMP                 = add( "bmp",          "Bitmapped Image" )
  val GIF                 = add( "gif",          "Graphics Interchange Format" )
  val HTML                = add( "html",         "HTML" )
  val JPEG                = add( "jpeg",         "JPEG" )
  val MicrosoftExcel      = add( "msexcel",      "Microsoft Excel" )
  val MicrosoftPowerPoint = add( "mspowerpoint", "Microsoft PowerPoint" )
  val MicrosoftWord       = add( "msword",       "Microsoft Word" )
  val PDF                 = add( "pdf",          "Portable Document Format" )
  val PNG                 = add( "png",          "Portable Network Graphics" )
  val Text                = add( "text",         "Text" )
  val TIFF                = add( "tiff",         "Tagged Image Format" )
  val Video               = add( "video",        "Video / Movie" )
  val XML                 = add( "xml",          "XML - Extensible Markup Language" )
  val ZIP                 = add( "zip",          "ZIP Archive" )

  val Image               = add( "image",        "Image", includes = Seq( BMP, GIF, HTML, JPEG, PNG, TIFF ) )
}


case class MimeType( mimeType:String, meta:MetaMimeType, name:String, extensions:Seq[String] ) {

  // this is the primary/default extension
  def extension = extensions( 0 )

  val label =
    if ( name.notBlank ) name
    else                 extension.toUpperCase
}

object MimeType {

  import MetaMimeType._

  val types = Seq(
    MimeType( "application/illustrator",                                                   AdobeIllustrator,    "Adobe Illustrator",             Seq( "ai" ) ),
    MimeType( "application/msword",                                                        MicrosoftWord,       "Microsoft Word",                Seq( "doc", "dot" ) ),
    MimeType( "application/octet-stream",                                                  null,                null,                            Seq( "bin", "exe" ) ),
    MimeType( "application/oda",                                                           null,                null,                            Seq( "oda" ) ),
    MimeType( "application/postscript",                                                    null,                "PostScript",                    Seq( "ps", "eps" ) ),
    MimeType( "application/pdf",                                                           PDF,                 "Portable Document Format",      Seq( "pdf" ) ),
    MimeType( "application/rtf",                                                           null,                "Rich Text",                     Seq( "rtf" ) ),

    MimeType( "application/vnd.ms-excel",                                                  MicrosoftExcel,      "Microsoft Excel",               Seq( "xls" ) ),
    MimeType( "application/vnd.ms-excel",                                                  MicrosoftExcel,      "Microsoft Excel Application",   Seq( "xla" ) ),
    MimeType( "application/vnd.ms-excel",                                                  MicrosoftExcel,      "Microsoft Excel",               Seq( "xlt" ) ),
    MimeType( "application/vnd.ms-excel.addin.macroEnabled.12",                            MicrosoftExcel,      "Microsoft Excel",               Seq( "xlam" ) ),
    MimeType( "application/vnd.ms-excel.sheet.binary.macroEnabled.12",                     MicrosoftExcel,      "Microsoft Excel",               Seq( "xlsb" ) ),
    MimeType( "application/vnd.ms-excel.sheet.macroEnabled.12",                            MicrosoftExcel,      "Microsoft Excel",               Seq( "xlsm" ) ),
    MimeType( "application/vnd.ms-excel.template.macroEnabled.12",                         MicrosoftExcel,      "Microsoft Excel",               Seq( "xltm" ) ),
    MimeType( "application/vnd.ms-powerpoint",                                             MicrosoftPowerPoint, "Microsoft Powerpoint",          Seq( "ppt" ) ),
    MimeType( "application/vnd.ms-powerpoint",                                             MicrosoftPowerPoint, "Microsoft Powerpoint",          Seq( "pot" ) ),
    MimeType( "application/vnd.ms-powerpoint",                                             MicrosoftPowerPoint, "Microsoft Powerpoint",          Seq( "ppa" ) ),
    MimeType( "application/vnd.ms-powerpoint",                                             MicrosoftPowerPoint, "Microsoft Powerpoint",          Seq( "pps" ) ),
    MimeType( "application/vnd.ms-powerpoint.addin.macroEnabled.12",                       MicrosoftPowerPoint, "Microsoft Powerpoint",          Seq( "ppam" ) ),
    MimeType( "application/vnd.ms-powerpoint.presentation.macroEnabled.12",                MicrosoftPowerPoint, "Microsoft Powerpoint",          Seq( "pptm" ) ),
    MimeType( "application/vnd.ms-powerpoint.slideshow.macroEnabled.12",                   MicrosoftPowerPoint, "Microsoft Powerpoint",          Seq( "ppsm" ) ),
    MimeType( "application/vnd.ms-powerpoint.template.macroEnabled.12",                    MicrosoftPowerPoint, "Microsoft Powerpoint",          Seq( "potm" ) ),
    MimeType( "application/vnd.ms-project",                                                MicrosoftPowerPoint, "Microsoft Project",             Seq( "mpp" ) ),
    MimeType( "application/vnd.ms-word.document.macroEnabled.12",                          MicrosoftWord,       "Microsoft Word",                Seq( "docm" ) ),
    MimeType( "application/vnd.ms-word.template.macroEnabled.12",                          MicrosoftWord,       "Microsoft Word",                Seq( "dotm" ) ),
    MimeType( "application/vnd.oasis.opendocument.presentation",                           null,                null,                            Seq( "odp" ) ),
    MimeType( "application/vnd.oasis.opendocument.spreadsheet",                            null,                null,                            Seq( "ods" ) ),
    MimeType( "application/vnd.oasis.opendocument.text",                                   null,                null,                            Seq( "odt" ) ), 
    MimeType( "application/vnd.oasis.opendocument.graphics-template",                      null,                null,                            Seq( "otg" ) ),   
    MimeType( "application/vnd.oasis.opendocument.database",                               null,                null,                            Seq( "odb" ) ),
    MimeType( "application/vnd.oasis.opendocument.image",                                  null,                null,                            Seq( "odi" ) ),
    MimeType( "application/vnd.oasis.opendocument.chart",                                  null,                null,                            Seq( "odc" ) ),  
    MimeType( "application/vnd.oasis.opendocument.text-web",                               null,                null,                            Seq( "oth" ) ),
    MimeType( "application/vnd.oasis.opendocument.formula",                                null,                null,                            Seq( "odf" ) ),  
    MimeType( "application/vnd.oasis.opendocument.graphics",                               null,                null,                            Seq( "odg" ) ),
    MimeType( "application/vnd.oasis.opendocument.text-master",                            null,                null,                            Seq( "odm" ) ),  
    MimeType( "application/vnd.oasis.opendocument.text-template",                          null,                null,                            Seq( "ott" ) ),   
    MimeType( "application/vnd.oasis.opendocument.presentation-template",                  null,                null,                            Seq( "otp" ) ),
    MimeType( "application/vnd.oasis.opendocument.spreadsheet-template",                   null,                null,                            Seq( "ots" ) ), 
    MimeType( "application/vnd.openofficeorg.extension",                                   null,                null,                            Seq( "oxt" ) ),
    MimeType( "application/vnd.openxmlformats-officedocument.presentationml.presentation", null,                null,                            Seq( "pptx" ) ),
    MimeType( "application/vnd.openxmlformats-officedocument.presentationml.slideshow",    null,                null,                            Seq( "ppsx" ) ),
    MimeType( "application/vnd.openxmlformats-officedocument.presentationml.template",     null,                null,                            Seq( "potx" ) ),
    MimeType( "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",         null,                null,                            Seq( "xlsx" ) ),
    MimeType( "application/vnd.openxmlformats-officedocument.spreadsheetml.template",      null,                null,                            Seq( "xltx" ) ),
    MimeType( "application/vnd.openxmlformats-officedocument.wordprocessingml.document",   null,                null,                            Seq( "docx" ) ),
    MimeType( "application/vnd.openxmlformats-officedocument.wordprocessingml.template",   null,                null,                            Seq( "dotx" ) ),

    MimeType( "application/x-bcpio",                                                       null,                null,                            Seq( "bcpio" ) ),
    MimeType( "application/x-cpio",                                                        null,                null,                            Seq( "cpio" ) ),
    MimeType( "application/x-csh",                                                         null,                null,                            Seq( "csh" ) ),
    MimeType( "application/x-dvi",                                                         null,                "Digital Video Interface",       Seq( "dvi" ) ),
    MimeType( "application/x-gtar",                                                        null,                "GNU Tape Archive",              Seq( "gtar" ) ),
    MimeType( "application/x-hdf",                                                         null,                null,                            Seq( "hdf" ) ),
    MimeType( "application/x-latex",                                                       null,                "LaTeX",                         Seq( "latex" ) ),
    MimeType( "application/x-mif",                                                         null,                "MIF",                           Seq( "mif" ) ),
    MimeType( "application/x-netcdf",                                                      null,                null,                            Seq( "cdf", "nc" ) ),
    MimeType( "application/x-perl",                                                        null,                "Perl",                          Seq( "pl" ) ),
    MimeType( "application/x-sh",                                                          null,                null,                            Seq( "sh" ) ),
    MimeType( "application/x-shar",                                                        null,                null,                            Seq( "shar" ) ),
    MimeType( "application/x-stuffit",                                                     null,                "StuffIt Archive",               Seq( "sit" ) ),
    MimeType( "application/x-tar",                                                         null,                "Tape Archive",                  Seq( "tar" ) ),
    MimeType( "application/x-tcl",                                                         null,                "Tool Command Language",         Seq( "tcl" ) ),
    MimeType( "application/x-tex",                                                         null,                "TeX",                           Seq( "tex" ) ),
    MimeType( "application/x-texinfo",                                                     null,                "TeX Info",                      Seq( "texinfo", "texi" ) ),
    MimeType( "application/x-troff",                                                       null,                "troff",                         Seq( "tr", "roff", "t" ) ),
    MimeType( "application/x-troff-man",                                                   null,                "troff man",                     Seq( "man" ) ),
    MimeType( "application/x-troff-me",                                                    null,                "troff me",                      Seq( "me" ) ),
    MimeType( "application/x-troff-ms",                                                    null,                "troff ms",                      Seq( "ms" ) ),
    MimeType( "application/x-wais-source",                                                 null,                "WAIS",                          Seq( "src" ) ),
    MimeType( "application/x-zip-compressed",                                              ZIP,                 "ZIP",                           Seq( "zip" ) ),

    MimeType( "audio/x-aiff",                                                              null,                "Audio Interchange File Format", Seq( "aif", "aifc", "aiff" ) ),
    MimeType( "audio/basic",                                                               null,                "Basic Audio",                   Seq( "au", "snd" ) ),
    MimeType( "audio/x-wav",                                                               null,                "WAV Audio",                     Seq( "wav" ) ),

    MimeType( "image/bmp",                                                                 BMP,                 "BMP Image",                     Seq( "bmp" ) ),
    MimeType( "image/gif",                                                                 GIF,                 "GIF Image",                     Seq( "gif" ) ),
    MimeType( "image/jpeg",                                                                JPEG,                "JPEG Image",                    Seq( "jpeg", "jpg", "jpe" ) ),
    MimeType( "image/png",                                                                 PNG,                 "Portable Network Graphics",     Seq( "png" ) ),
    MimeType( "image/tiff",                                                                TIFF,                "Tagged Image File Format",      Seq( "tiff", "tif" ) ),
    MimeType( "image/x-cmu-raster",                                                        null,                "CMU Raster",                    Seq( "ras" ) ),
    MimeType( "image/x-portable-bitmap",                                                   null,                "Portable Bitmap",               Seq( "pbm" ) ),
    MimeType( "image/x-portable-graymap",                                                  null,                "Portable Grey Map",             Seq( "pgm" ) ),
    MimeType( "image/x-portable-anymap",                                                   null,                "Portable Any Map",              Seq( "pnm" ) ),
    MimeType( "image/x-portable-pixmap",                                                   null,                "Portable PixMap",               Seq( "ppm" ) ),
    MimeType( "image/x-rgb",                                                               null,                "RGB Image",                     Seq( "rgb" ) ),
    MimeType( "image/x-xbitmap",                                                           null,                "X Windows Bitmap",              Seq( "xbm" ) ),
    MimeType( "image/x-xpixmap",                                                           null,                "X PixMap",                      Seq( "xpm" ) ),
    MimeType( "image/x-xwindowdump",                                                       null,                "X Windows Dump",                Seq( "xwd" ) ),

    MimeType( "text/css",                                                                  null,                "CSS",                           Seq( "css" ) ),
    MimeType( "text/html",                                                                 HTML,                "HTML",                          Seq( "html", "htm" ) ),
    MimeType( "text/javascript",                                                           null,                "Javascript",                    Seq( "js" ) ),
    MimeType( "text/plain",                                                                Text,                "Text",                          Seq( "txt", "text" ) ),
    MimeType( "text/plain",                                                                null,                "Markdown",                      Seq( "markdown", "md", "mkd" ) ),
    MimeType( "text/richtext",                                                             null,                "Rich Text",                     Seq( "rtx" ) ),
    MimeType( "text/tab-separated-values",                                                 null,                "Tab Separated Values",          Seq( "tsv" ) ),
    MimeType( "text/x-sgml",                                                               null,                "SGML",                          Seq( "sgml", "sgm" ) ),
    MimeType( "text/xml",                                                                  XML,                 "XML",                           Seq( "xml" ) ),
    MimeType( "text/xml",                                                                  XML,                 "XSL",                           Seq( "xsl" ) ),

    MimeType( "video/mp4",                                                                 null,                "MP4",                           Seq( "mp4" ) ),
    MimeType( "video/mpeg",                                                                Video,               "MPEG Movie",                    Seq( "mpg", "mpe", "mpeg" ) ),
    MimeType( "video/quicktime",                                                           Video,               "QuickTime",                     Seq( "mov", "qt" ) ),
    MimeType( "video/x-sgi-movie",                                                         null,                "SGI Movie",                     Seq( "movie" ) ),
    MimeType( "video/x-msvideo",                                                           Video,               "Microsoft AVI",                 Seq( "avi" ) )
  )

  lazy val byMimeType:collection.Map[String,MimeType] = {
    val map = mutable.Map[String,MimeType]()

    for ( mt <- types )
      if ( !map.contains( mt.mimeType ) )
        map( mt.mimeType ) = mt

    map
  }

  lazy val byExtension:collection.Map[String,MimeType] =
    Map(
      ( for ( mt <- types;
              ext <- mt.extensions )
          yield ext -> mt ):_* )

  // TODO:  remove once the new way is tested
  val oldByExtension = Map( 
    "ai"       -> "application/illustrator",
    "aif"      -> "audio/x-aiff",
    "aifc"     -> "audio/x-aiff",
    "aiff"     -> "audio/x-aiff",
    "au"       -> "audio/basic",
    "avi"      -> "video/x-msvideo",
      
    "bcpio"    -> "application/x-bcpio",
    "bin"      -> "application/octet-stream",
    "bmp"      -> "image/bmp",

    "cdf"      -> "application/x-netcdf",
    "cpio"     -> "application/x-cpio",
    "csh"      -> "application/x-csh",

    "doc"      -> "application/msword",
    "docm"     -> "application/vnd.ms-word.document.macroEnabled.12",
    "docx"     -> "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    "dot"      -> "application/msword",
    "dotm"     -> "application/vnd.ms-word.template.macroEnabled.12",
    "dotx"     -> "application/vnd.openxmlformats-officedocument.wordprocessingml.template",
    "dvi"      -> "application/x-dvi",
    
    "eps"      -> "application/postscript",

    "gif"      -> "image/gif",
    "gtar"     -> "application/x-gtar",
    
    "hdf"      -> "application/x-hdf",
    "htm"      -> "text/html",
    "html"     -> "text/html",

    "jpe"      -> "image/jpeg",
    "jpeg"     -> "image/jpeg",
    "jpg"      -> "image/jpeg",
    "js"       -> "text/javascript",
    
    "latex"    -> "application/x-latex",

    "man"      -> "application/x-troff-man",
    "markdown" -> "text/plain",
    "md"       -> "text/plain",
    "me"       -> "application/x-troff-me",
    "mif"      -> "application/x-mif",
    "mkd"      -> "text/plain",
    "mov"      -> "video/quicktime",
    "movie"    -> "video/x-sgi-movie",

    "mp4"      -> "video/mp4",
    "mpe"      -> "video/mpeg",
    "mpeg"     -> "video/mpeg",
    "mpg"      -> "video/mpeg",
    "mpp"      -> "application/vnd.ms-project",
    "ms"       -> "application/x-troff-ms",
    
    "nc"       -> "application/x-netcdf",
    
    "oda"      -> "application/oda",
    "odb"      -> "application/vnd.oasis.opendocument.database",
    "odi"      -> "application/vnd.oasis.opendocument.image",
    "odc"      -> "application/vnd.oasis.opendocument.chart",  
    "odf"      -> "application/vnd.oasis.opendocument.formula",  
    "odg"      -> "application/vnd.oasis.opendocument.graphics",
    "odm"      -> "application/vnd.oasis.opendocument.text-master",  
    "odp"      -> "application/vnd.oasis.opendocument.presentation",
    "ods"      -> "application/vnd.oasis.opendocument.spreadsheet",
    "odt"      -> "application/vnd.oasis.opendocument.text", 
    "otg"      -> "application/vnd.oasis.opendocument.graphics-template",   
    "oth"      -> "application/vnd.oasis.opendocument.text-web",
    "ott"      -> "application/vnd.oasis.opendocument.text-template",   
    "otp"      -> "application/vnd.oasis.opendocument.presentation-template",
    "ots"      -> "application/vnd.oasis.opendocument.spreadsheet-template", 
    "oxt"      -> "application/vnd.openofficeorg.extension",
    
    "pbm"      -> "image/x-portable-bitmap",
    "pdf"      -> "application/pdf",
    "pgm"      -> "image/x-portable-graymap",
    "png"      -> "image/png",
    "pnm"      -> "image/x-portable-anymap",
    "pot"      -> "application/vnd.ms-powerpoint",
    "potm"     -> "application/vnd.ms-powerpoint.template.macroEnabled.12",
    "potx"     -> "application/vnd.openxmlformats-officedocument.presentationml.template",
    "ppa"      -> "application/vnd.ms-powerpoint",
    "ppam"     -> "application/vnd.ms-powerpoint.addin.macroEnabled.12",
    "ppm"      -> "image/x-portable-pixmap",
    "pps"      -> "application/vnd.ms-powerpoint",
    "ppt"      -> "application/vnd.ms-powerpoint",
    "ppsm"     -> "application/vnd.ms-powerpoint.slideshow.macroEnabled.12",
    "pptm"     -> "application/vnd.ms-powerpoint.presentation.macroEnabled.12",
    "ppsx"     -> "application/vnd.openxmlformats-officedocument.presentationml.slideshow",
    "pptx"     -> "application/vnd.openxmlformats-officedocument.presentationml.presentation",
    "ps"       -> "application/postscript",
    
    "qt"       -> "video/quicktime",
    
    "ras"      -> "image/x-cmu-raster",
    "rgb"      -> "image/x-rgb",
    "roff"     -> "application/x-troff",
    "rtf"      -> "application/rtf",
    "rtx"      -> "text/richtext",
    
    "sgm"      -> "text/x-sgml",
    "sgml"     -> "text/x-sgml",
    "sh"       -> "application/x-sh",
    "shar"     -> "application/x-shar",
    "snd"      -> "audio/basic",
    "src"      -> "application/x-wais-source",

    "t"        -> "application/x-troff",
    "tar"      -> "application/x-tar",
    "tcl"      -> "application/x-tcl",
    "tex"      -> "application/x-tex",
    "texinfo"  -> "application/x-texinfo",
    "texi"     -> "application/x-texinfo",
    "text"     -> "text/plain",
    "tif"      -> "image/tiff",
    "tiff"     -> "image/tiff",
    "tr"       -> "application/x-troff",
    "tsv"      -> "text/tab-separated-values",
    "txt"      -> "text/plain",
    
    "wav"      -> "audio/x-wav",
    
    "xbm"      -> "image/x-xbitmap",
    "xla"      -> "application/vnd.ms-excel",
    "xlam"     -> "application/vnd.ms-excel.addin.macroEnabled.12",
    "xls"      -> "application/vnd.ms-excel",
    "xlsb"     -> "application/vnd.ms-excel.sheet.binary.macroEnabled.12",
    "xlsm"     -> "application/vnd.ms-excel.sheet.macroEnabled.12",
    "xlsx"     -> "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    "xlt"      -> "application/vnd.ms-excel",
    "xltm"     -> "application/vnd.ms-excel.template.macroEnabled.12",
    "xltx"     -> "application/vnd.openxmlformats-officedocument.spreadsheetml.template",
    "xml"      -> "text/xml",
    "xpm"      -> "image/x-xpixmap",
    "xsl"      -> "text/xml",
    "xwd"      -> "image/x-xwindowdump",
    
    "zip"      -> "application/x-zip-compressed"
  )

  def isImage( mimeType:String ) =
    mimeType match {
    case "image/jpeg" | "image/gif" | "image/png" => true
    case _ => false
    }

  def isVideo( mimeType:String ) =
    mimeType match {
    case "video/mp4" => true
    case _ => false
    }
}

