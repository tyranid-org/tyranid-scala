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
package org.tyranid.web

import org.apache.commons.fileupload.FileItem

/**
 * TODO This is a copy-and-paste abomination that needs to be merged with
 * [[org.scalatra.util.MultiMap]].  We can't properly genericize that this
 * close to a release.
 */
class FileMultiParams(wrapped: Map[String, Seq[FileItem]] = Map.empty) extends Map[String, Seq[FileItem]] {

  def get(key: String): Option[Seq[FileItem]] = {
    (wrapped.get(key) orElse wrapped.get(key + "[]"))
  }

  def get(key: Symbol): Option[Seq[FileItem]] = get(key.name)

  def +[B1 >: Seq[FileItem]](kv: (String, B1)) =
    new FileMultiParams(wrapped + kv.asInstanceOf[(String, Seq[FileItem])])

  def -(key: String) = new FileMultiParams(wrapped - key)

  def iterator = wrapped.iterator

  override def default(a: String): Seq[FileItem] = wrapped.default(a)
}

object FileMultiParams {
  def apply() = new FileMultiParams

  def apply[SeqType <: Seq[FileItem]](wrapped: Map[String, Seq[FileItem]]) =
    new FileMultiParams(wrapped)
}
