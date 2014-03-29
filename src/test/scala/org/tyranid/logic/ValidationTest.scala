/**
 * Copyright (c) 2008-2014 Tyranid <http://tyranid.org>
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

package org.tyranid.logic

import org.scalatest.FunSuite

import org.tyranid.ImpT._
import org.tyranid.db.DbEmail
//import org.tyranid.Bind
//import org.tyranid.db.Scope
//import org.tyranid.db.tuple.Tuple
//import org.tyranid.profile.User

class ValidationSuite extends FunSuite {

  test( "validate email" ) {
    val data = Seq(
      ( "",            false ),
      ( "foo",         false ),
      ( "foo@com",     false ),
      ( "foo@bar.com", true )
    )

    for ( test <- data )
      assert( test._1.isEmail === test._2 )

    //Bind.NewUser = () => new Tuple with User

    //val user = User.current
    //val va = user.view( "email" )
    //val scope = Scope( user, Some( va ) )

    //val invalids =
      //for ( invalidOpt <- va.validations.map( validator => validator( scope ) );
            //invalid <- invalidOpt )
        //yield invalid

  //override val validations =
    //( ( scope:Scope ) => scope.s.filter( !_.matches( regex ) ).map( s => Invalid( scope, "is an invalid email address." ) ) ) ::
    //super.validations
  }
}

