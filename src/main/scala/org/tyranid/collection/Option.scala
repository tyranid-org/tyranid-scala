
package org.tyranid.collection


class OptionImp[A]( opt:Option[A] ) {

	def flatten[B]( getter: ( A ) => B, fallback:B ) = opt match {
		case Some( obj ) => getter( obj )
		case None        => fallback
	}
}

