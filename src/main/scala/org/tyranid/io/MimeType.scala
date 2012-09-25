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


import org.tyranid.Imp._




object MimeType {
  val byExtension = Map( 
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

