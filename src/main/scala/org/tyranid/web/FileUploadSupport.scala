package org.tyranid.web

import org.apache.commons.fileupload.servlet.ServletFileUpload
import org.apache.commons.fileupload.{FileItemFactory, FileItem}
import org.apache.commons.fileupload.disk.{DiskFileItem, DiskFileItemFactory}
import collection.JavaConversions._
import scala.util.DynamicVariable
import java.util.{List => JList, HashMap => JHashMap, Map => JMap}
import javax.servlet.http.{HttpServletRequestWrapper, HttpServletRequest, HttpServletResponse}
import javax.servlet.ServletContext
import collection.Iterable
import java.lang.String

object FileUploadSupport {
  case class BodyParams(fileParams: FileMultiParams, formParams: Map[String, List[String]])
  val BodyParamsKey = "org.tyranid.web.fileupload.bodyParams"

  def checkContext( web:WebContext ): WebContext = {
    val req = web.req
    val res = web.res
    
    if (ServletFileUpload.isMultipartContent(req)) {
      val bodyParams = extractMultipartParams(req)
      var mergedParams = bodyParams.formParams
      // Add the query string parameters
      req.getParameterMap.asInstanceOf[JMap[String, Array[String]]] foreach {
        case (name, values) =>
          val formValues = mergedParams.getOrElse(name, List.empty)
          mergedParams += name -> (values.toList ++ formValues)
      }
      
      web.copy( req = wrapRequest( req, mergedParams ) )
    } else
      web
  }
  
  private def extractMultipartParams( req: HttpServletRequest ): BodyParams =
    // First look for it cached on the request, because we can't parse it twice.  See GH-16.
    req.getAttribute( BodyParamsKey ).asInstanceOf[Option[BodyParams]] match {
      case Some(bodyParams) =>
        bodyParams
      case null | None =>
        val upload = newServletFileUpload
        val items = upload.parseRequest(req).asInstanceOf[JList[FileItem]]
        val bodyParams = items.foldRight(BodyParams(FileMultiParams(), Map.empty)) { (item, params) =>
          if (item.isFormField)
            BodyParams(params.fileParams, params.formParams + ((item.getFieldName, fileItemToString(req, item) :: params.formParams.getOrElse(item.getFieldName, List[String]()))))
          else
            BodyParams(params.fileParams + ((item.getFieldName, item +: params.fileParams.getOrElse(item.getFieldName, List[FileItem]()))), params.formParams)
          }
        req.setAttribute(BodyParamsKey, bodyParams )
        bodyParams
    }

  /**
   * Converts a file item to a string.
   *
   * Browsers tend to be sloppy about providing content type headers with
   * charset information to form-data parts.  Worse, browsers are
   * inconsistent about how they encode these parameters.
   *
   * The default implementation attempts to use the charset specified on
   * the request.  If that is unspecified, and it usually isn't, then it
   * falls back to the kernel's charset.
   */
  protected def fileItemToString(req: HttpServletRequest, item: FileItem): String = {
    val charset = item match {
      case diskItem: DiskFileItem =>
        // Why doesn't FileItem have this method???
        Option(diskItem.getCharSet())
      case _ =>
        None
    }
    item.getString(charset getOrElse "UTF-8" )
  }

  private def wrapRequest(req: HttpServletRequest, formMap: Map[String, Seq[String]]) =
    new HttpServletRequestWrapper(req) {
      override def getParameter(name: String) = { formMap.get(name) map { _.head } getOrElse null }
      override def getParameterNames = formMap.keysIterator
      override def getParameterValues(name: String) = formMap.get(name) map { _.toArray } getOrElse null
      override def getParameterMap = new JHashMap[String, Array[String]] ++ (formMap transform { (k, v) => v.toArray })
    }

  val maxFileSize = 1024 * 1000 * 1000 * 2 // 2G
  
  /**
   * Creates a new file upload handler to parse the request.  By default, it
   * creates a `ServletFileUpload` instance with the file item factory 
   * returned by the `fileItemFactory` method.  Override this method to
   * customize properties such as the maximum file size, progress listener,
   * etc.
   *
   * @return a new file upload handler.
   */
  protected def newServletFileUpload: ServletFileUpload = { 
    val sfu = new ServletFileUpload(fileItemFactory)
    sfu.setSizeMax( maxFileSize )
    sfu
  }

  /**
   * The file item factory used by the default implementation of 
   * `newServletFileUpload`.  By default, we use a DiskFileItemFactory.
   */
  /*
   * [non-scaladoc] This method predates newServletFileUpload.  If I had it 
   * to do over again, we'd have that instead of this.  Oops.
   */
  protected def fileItemFactory: FileItemFactory = new DiskFileItemFactory()

  /*
  protected def fileMultiParams:FileMultiParams = extractMultipartParams(request).fileParams

  protected val _fileParams = new collection.Map[String, FileItem] {
    def get(key: String) = fileMultiParams.get(key) flatMap { _.headOption }
    override def size = fileMultiParams.size
    override def iterator = fileMultiParams map { case(k, v) => (k, v.head) } iterator
    override def -(key: String) = Map() ++ this - key
    override def +[B1 >: FileItem](kv: (String, B1)) = Map() ++ this + kv
  }

  /** @return a Map, keyed on the names of multipart file upload parameters, of all multipart files submitted with the request */
  def fileParams = _fileParams
  */
}
