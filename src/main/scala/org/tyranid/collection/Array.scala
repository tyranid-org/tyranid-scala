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

package org.tyranid.collection

import org.tyranid.Imp.spam

class ArrayImp[A]( array:Array[A] ) {

  /**
   * This returns an array with the new size as follows:
   *
   * newSize == size  ...  returns same array (i.e. does nothing)
   * newSize &gt;  size  ...  returns a copy of this array with trailing values null-padded
   * newSize &lt;  size  ...  returns a copy of this array with the size truncated to the newSize
   */
	def resize( newSize:Int ) =
    if ( array.size == newSize ) {
      array
    } else {
      // do it this ugly way since we don't *need* to create/pass along a Manifest
      val newArray = java.lang.reflect.Array.newInstance( array.getClass.getComponentType, newSize ).asInstanceOf[ Array[A] ]
      System.arraycopy( array, 0, newArray, 0, newSize min array.size )
      newArray
    }
}

