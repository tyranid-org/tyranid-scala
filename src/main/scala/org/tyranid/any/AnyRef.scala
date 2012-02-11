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

package org.tyranid.any


class AnyRefImp[T <: AnyRef]( ref:T ) {

  /**
   * Elvis operator.
   */
  def ?|( default: => T ):T =
    if ( ref != null ) ref
    else               default

  /**
   * null-safe toString().  Need a better name.
   */
  def safeString:String =
    if ( ref != null ) ref.toString
    else               ""
}

