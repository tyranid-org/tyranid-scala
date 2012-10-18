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


case class MimeType( mimeType:String, name:String, extensions:Seq[String] ) {

  // this is the primary/default extension
  def extension = extensions( 0 )

  val label =
    if ( name.notBlank ) name
    else                 extension.toUpperCase
}

object MimeType {

  val types = Seq(
    MimeType( "application/illustrator",                                                   "Adobe Illustrator",             Seq( "ai" ) ),
    MimeType( "application/msword",                                                        "Microsoft Word",                Seq( "doc", "dot" ) ),
    MimeType( "application/octet-stream",                                                  null,                            Seq( "bin", "exe" ) ),
    MimeType( "application/oda",                                                           null,                            Seq( "oda" ) ),
    MimeType( "application/postscript",                                                    "PostScript",                    Seq( "ps", "eps" ) ),
    MimeType( "application/pdf",                                                           "Portable Document Format",      Seq( "pdf" ) ),
    MimeType( "application/rtf",                                                           "Rich Text",                     Seq( "rtf" ) ),

    MimeType( "application/vnd.ms-excel",                                                  "Microsoft Excel",               Seq( "xls" ) ),
    MimeType( "application/vnd.ms-excel",                                                  "Microsoft Excel Application",   Seq( "xla" ) ),
    MimeType( "application/vnd.ms-excel",                                                  "Microsoft Excel",               Seq( "xlt" ) ),
    MimeType( "application/vnd.ms-excel.addin.macroEnabled.12",                            "Microsoft Excel",               Seq( "xlam" ) ),
    MimeType( "application/vnd.ms-excel.sheet.binary.macroEnabled.12",                     "Microsoft Excel",               Seq( "xlsb" ) ),
    MimeType( "application/vnd.ms-excel.sheet.macroEnabled.12",                            "Microsoft Excel",               Seq( "xlsm" ) ),
    MimeType( "application/vnd.ms-excel.template.macroEnabled.12",                         "Microsoft Excel",               Seq( "xltm" ) ),
    MimeType( "application/vnd.ms-powerpoint",                                             "Microsoft Powerpoint",          Seq( "ppt" ) ),
    MimeType( "application/vnd.ms-powerpoint",                                             "Microsoft Powerpoint",          Seq( "pot" ) ),
    MimeType( "application/vnd.ms-powerpoint",                                             "Microsoft Powerpoint",          Seq( "ppa" ) ),
    MimeType( "application/vnd.ms-powerpoint",                                             "Microsoft Powerpoint",          Seq( "pps" ) ),
    MimeType( "application/vnd.ms-powerpoint.addin.macroEnabled.12",                       "Microsoft Powerpoint",          Seq( "ppam" ) ),
    MimeType( "application/vnd.ms-powerpoint.presentation.macroEnabled.12",                "Microsoft Powerpoint",          Seq( "pptm" ) ),
    MimeType( "application/vnd.ms-powerpoint.slideshow.macroEnabled.12",                   "Microsoft Powerpoint",          Seq( "ppsm" ) ),
    MimeType( "application/vnd.ms-powerpoint.template.macroEnabled.12",                    "Microsoft Powerpoint",          Seq( "potm" ) ),
    MimeType( "application/vnd.ms-project",                                                "Microsoft Project",             Seq( "mpp" ) ),
    MimeType( "application/vnd.ms-word.document.macroEnabled.12",                          "Microsoft Word",                Seq( "docm" ) ),
    MimeType( "application/vnd.ms-word.template.macroEnabled.12",                          "Microsoft Word",                Seq( "dotm" ) ),
    MimeType( "application/vnd.oasis.opendocument.presentation",                           null,                            Seq( "odp" ) ),
    MimeType( "application/vnd.oasis.opendocument.spreadsheet",                            null,                            Seq( "ods" ) ),
    MimeType( "application/vnd.oasis.opendocument.text",                                   null,                            Seq( "odt" ) ), 
    MimeType( "application/vnd.oasis.opendocument.graphics-template",                      null,                            Seq( "otg" ) ),   
    MimeType( "application/vnd.oasis.opendocument.database",                               null,                            Seq( "odb" ) ),
    MimeType( "application/vnd.oasis.opendocument.image",                                  null,                            Seq( "odi" ) ),
    MimeType( "application/vnd.oasis.opendocument.chart",                                  null,                            Seq( "odc" ) ),  
    MimeType( "application/vnd.oasis.opendocument.text-web",                               null,                            Seq( "oth" ) ),
    MimeType( "application/vnd.oasis.opendocument.formula",                                null,                            Seq( "odf" ) ),  
    MimeType( "application/vnd.oasis.opendocument.graphics",                               null,                            Seq( "odg" ) ),
    MimeType( "application/vnd.oasis.opendocument.text-master",                            null,                            Seq( "odm" ) ),  
    MimeType( "application/vnd.oasis.opendocument.text-template",                          null,                            Seq( "ott" ) ),   
    MimeType( "application/vnd.oasis.opendocument.presentation-template",                  null,                            Seq( "otp" ) ),
    MimeType( "application/vnd.oasis.opendocument.spreadsheet-template",                   null,                            Seq( "ots" ) ), 
    MimeType( "application/vnd.openofficeorg.extension",                                   null,                            Seq( "oxt" ) ),
    MimeType( "application/vnd.openxmlformats-officedocument.presentationml.presentation", null,                            Seq( "pptx" ) ),
    MimeType( "application/vnd.openxmlformats-officedocument.presentationml.slideshow",    null,                            Seq( "ppsx" ) ),
    MimeType( "application/vnd.openxmlformats-officedocument.presentationml.template",     null,                            Seq( "potx" ) ),
    MimeType( "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",         null,                            Seq( "xlsx" ) ),
    MimeType( "application/vnd.openxmlformats-officedocument.spreadsheetml.template",      null,                            Seq( "xltx" ) ),
    MimeType( "application/vnd.openxmlformats-officedocument.wordprocessingml.document",   null,                            Seq( "docx" ) ),
    MimeType( "application/vnd.openxmlformats-officedocument.wordprocessingml.template",   null,                            Seq( "dotx" ) ),

    MimeType( "application/x-bcpio",                                                       null,                            Seq( "bcpio" ) ),
    MimeType( "application/x-cpio",                                                        null,                            Seq( "cpio" ) ),
    MimeType( "application/x-csh",                                                         null,                            Seq( "csh" ) ),
    MimeType( "application/x-dvi",                                                         "Digital Video Interface",       Seq( "dvi" ) ),
    MimeType( "application/x-gtar",                                                        "GNU Tape Archive",              Seq( "gtar" ) ),
    MimeType( "application/x-hdf",                                                         null,                            Seq( "hdf" ) ),
    MimeType( "application/x-latex",                                                       "LaTeX",                         Seq( "latex" ) ),
    MimeType( "application/x-mif",                                                         "MIF",                           Seq( "mif" ) ),
    MimeType( "application/x-netcdf",                                                      null,                            Seq( "cdf", "nc" ) ),
    MimeType( "application/x-perl",                                                        "Perl",                          Seq( "pl" ) ),
    MimeType( "application/x-sh",                                                          null,                            Seq( "sh" ) ),
    MimeType( "application/x-shar",                                                        null,                            Seq( "shar" ) ),
    MimeType( "application/x-stuffit",                                                     "StuffIt Archive",               Seq( "sit" ) ),
    MimeType( "application/x-tar",                                                         "Tape Archive",                  Seq( "tar" ) ),
    MimeType( "application/x-tcl",                                                         "Tool Command Language",         Seq( "tcl" ) ),
    MimeType( "application/x-tex",                                                         "TeX",                           Seq( "tex" ) ),
    MimeType( "application/x-texinfo",                                                     "TeX Info",                      Seq( "texinfo", "texi" ) ),
    MimeType( "application/x-troff",                                                       "troff",                         Seq( "tr", "roff", "t" ) ),
    MimeType( "application/x-troff-man",                                                   "troff man",                     Seq( "man" ) ),
    MimeType( "application/x-troff-me",                                                    "troff me",                      Seq( "me" ) ),
    MimeType( "application/x-troff-ms",                                                    "troff ms",                      Seq( "ms" ) ),
    MimeType( "application/x-wais-source",                                                 "WAIS",                          Seq( "src" ) ),
    MimeType( "application/x-zip-compressed",                                              "ZIP",                           Seq( "zip" ) ),

    MimeType( "audio/x-aiff",                                                              "Audio Interchange File Format", Seq( "aif", "aifc", "aiff" ) ),
    MimeType( "audio/basic",                                                               "Basic Audio",                   Seq( "au", "snd" ) ),
    MimeType( "audio/x-wav",                                                               "WAV Audio",                     Seq( "wav" ) ),

    MimeType( "image/gif",                                                                 "GIF Image",                     Seq( "gif" ) ),
    MimeType( "image/jpeg",                                                                "JPEG Image",                    Seq( "jpeg", "jpg", "jpe" ) ),
    MimeType( "image/png",                                                                 "Portable Network Graphics",     Seq( "png" ) ),
    MimeType( "image/tiff",                                                                "Tagged Image File Format",      Seq( "tiff", "tif" ) ),
    MimeType( "image/x-cmu-raster",                                                        "CMU Raster",                    Seq( "ras" ) ),
    MimeType( "image/x-portable-bitmap",                                                   "Portable Bitmap",               Seq( "pbm" ) ),
    MimeType( "image/x-portable-graymap",                                                  "Portable Grey Map",             Seq( "pgm" ) ),
    MimeType( "image/x-portable-anymap",                                                   "Portable Any Map",              Seq( "pnm" ) ),
    MimeType( "image/x-portable-pixmap",                                                   "Portable PixMap",               Seq( "ppm" ) ),
    MimeType( "image/x-rgb",                                                               "RGB Image",                     Seq( "rgb" ) ),
    MimeType( "image/x-xbitmap",                                                           "X Windows Bitmap",              Seq( "xbm" ) ),
    MimeType( "image/x-xpixmap",                                                           "X PixMap",                      Seq( "xpm" ) ),
    MimeType( "image/x-xwindowdump",                                                       "X Windows Dump",                Seq( "xwd" ) ),

    MimeType( "text/css",                                                                  "CSS",                           Seq( "css" ) ),
    MimeType( "text/html",                                                                 "HTML",                          Seq( "html", "htm" ) ),
    MimeType( "text/plain",                                                                "Text",                          Seq( "txt", "text" ) ),
    MimeType( "text/plain",                                                                "Markdown",                      Seq( "markdown", "md", "mkd" ) ),
    MimeType( "text/richtext",                                                             "Rich Text",                     Seq( "rtx" ) ),
    MimeType( "text/tab-separated-values",                                                 "Tab Separated Values",          Seq( "tsv" ) ),
    MimeType( "text/x-sgml",                                                               "SGML",                          Seq( "sgml", "sgm" ) ),
    MimeType( "text/xml",                                                                  "XML",                           Seq( "xml" ) ),
    MimeType( "text/xml",                                                                  "XSL",                           Seq( "xsl" ) ),

    MimeType( "video/mp4",                                                                 "MP4",                           Seq( "mp4" ) ),
    MimeType( "video/mpeg",                                                                "MPEG Movie",                    Seq( "mpg", "mpe", "mpeg" ) ),
    MimeType( "video/quicktime",                                                           "QuickTime",                     Seq( "mov", "qt" ) ),
    MimeType( "video/x-sgi-movie",                                                         "SGI Movie",                     Seq( "movie" ) ),
    MimeType( "video/x-msvideo",                                                           "Microsoft AVI",                 Seq( "avi" ) )
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

