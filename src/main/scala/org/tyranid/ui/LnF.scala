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

package org.tyranid.ui

import org.tyranid.Imp._
import org.tyranid.db.{ DbChar, DbInt, DbText }
import org.tyranid.db.ram.RamEntity
import org.tyranid.db.tuple.{ Tuple, TupleView }



object LnF extends RamEntity( tid = "a0Uw" ) {
  type RecType = LnF
  override def convert( view:TupleView ) = new LnF( view )

  "_id"          is DbInt       is 'id;
  "name"         is DbText      is 'label;
  "code"         is DbChar( 2 ) ;
  "domainPrefix" is DbText      ;
  "domain"       is DbText      ;

  val SUPPLY_CHAIN = 1
  val RETAIL_BRAND = 2

  lazy val SupplyChain = getById( SUPPLY_CHAIN )
  lazy val RetailBrand = getById( RETAIL_BRAND )
  
  static { s =>
    s( "_id",        "name",         "code", "domainPrefix", "domain"         )
    s( SUPPLY_CHAIN, "Supply Chain", "sc",   "",             "freight-iq.com" ) 
    s( RETAIL_BRAND, "Retail Brand", "rb",   "rb",           "volerro.com"    )
  }

  def domainPrefix( appId:Long ) = byId( appId ).get.s( 'domainPrefix )

  // rb.volerro.com, rb-dev.volerro.com
  def byDomain( domain:String ): Tuple = {
    val lcDomain = domain.toLowerCase

    records.find { app =>
      val prefix = app.s( 'domainPrefix )
      prefix.notBlank && lcDomain.startsWith( prefix )
    } match {
    case Some( app ) => app
    case None        => records( 0 )
    }
  }
}

case class LnF( override val view:TupleView ) extends Tuple( view ) {

  def code = s( 'code )
  def domainPrefix = s( 'domainPrefix )
}

