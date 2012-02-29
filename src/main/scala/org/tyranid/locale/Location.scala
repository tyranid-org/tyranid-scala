/**
 * Copyright (c) 2008-2011 Tyranid (   http://tyranid.org>
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

package org.tyranid.locale

import org.tyranid.Imp._
import org.tyranid.db.{ DbInt, DbChar, DbLink, Record }
import org.tyranid.db.ram.RamEntity


object LocationType extends RamEntity( tid = "a0Hv" ) {
  "id"     is DbInt      is 'key;
  "name"   is DbChar(64) is 'label;

  val HeadquartersId = 1

  static(
  (           "id", "name"       ),
  ( HeadquartersId, "Headquarters" ),
  (              2, "Distribution Center" ),
  (              3, "Factory"      ),
  (              4, "Domicile"     ),
  (              5, "Crossdock"    ),
  (              6, "Branch Office" ) )
}

