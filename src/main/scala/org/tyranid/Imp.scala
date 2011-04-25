package org.tyranid

/**
 * IMPlicit IMPorts.
 */
object Imp {

	implicit def boolean( v:Boolean )     = new org.tyranid.logic.BooleanImp( v )
	implicit def option[A]( v:Option[A] ) = new org.tyranid.collection.OptionImp( v )
	implicit def string( v:String )       = new org.tyranid.text.StringImp( v )
}

