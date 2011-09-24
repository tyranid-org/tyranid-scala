/**
 * Copyright (c) 2008-2011 Tyranid <http://tyranid.org>
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

package org.tyranid.session

import scala.collection.mutable
import scala.xml.{ Node, NodeSeq, Unparsed }

import net.liftweb.http.SessionVar

import org.tyranid.Imp._
import org.tyranid.Bind
import org.tyranid.profile.User


trait SessionMeta {
  private object currentVar extends SessionVar[Session]( Bind.NewSession() )

  def apply():Session = currentVar.is

}

trait Session {

  private var userVar = Bind.NewUser()

  def user:User           = userVar
  def user_=( user:User ) = userVar = user

}

object Session extends SessionMeta {

}



