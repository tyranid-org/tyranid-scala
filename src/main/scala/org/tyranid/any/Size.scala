
package org.tyranid.any

import java.lang.reflect.{ Field, Modifier }

import scala.collection.mutable

import org.tyranid.Imp._


/**
 * adapted from java code by @author Kyrylo Holodnov
 */
object MemorySize {
	private var REFERENCE_SIZE:Int = 0
	private var HEADER_SIZE:Int = 0
	private val LONG_SIZE = 8
	private val INT_SIZE = 4
	private val BYTE_SIZE = 1
	private val BOOLEAN_SIZE = 1
	private val CHAR_SIZE = 2
	private val SHORT_SIZE = 2
	private val FLOAT_SIZE = 4
	private val DOUBLE_SIZE = 8
	private val ALIGNMENT = 8

  private val BooleanArray = new Array[Boolean]( 0 ).getClass
  private val ByteArray    = new Array[Byte]( 0 ).getClass
  private val CharArray    = new Array[Char]( 0 ).getClass
  private val DoubleArray  = new Array[Double]( 0 ).getClass
  private val FloatArray   = new Array[Float]( 0 ).getClass
  private val IntArray     = new Array[Int]( 0 ).getClass
  private val LongArray    = new Array[Long]( 0 ).getClass
  private val ShortArray   = new Array[Short]( 0 ).getClass

  try {
    if ( System.getProperties.get( "java.vm.name" ).toString.contains( "64" ) ) {
      // java.vm.name is something like
      // "Java HotSpot(TM) 64-Bit Server VM"
      REFERENCE_SIZE = 8
      HEADER_SIZE = 16
    } else {
      REFERENCE_SIZE = 4
      HEADER_SIZE = 8
    }
  } catch {
  case ex:Exception =>
    ex.printStackTrace
    throw new AssertionError( ex )
  }

	def sizeOf( o:AnyRef ):Long = {
		sizeOf(o, new mutable.HashSet[ObjectWrapper]())
	}

	private def sizeOf( o:AnyRef, visited:mutable.Set[ObjectWrapper] ):Long = {
		if ( o == null )
			return 0

		val objectWrapper = new ObjectWrapper(o)
		if ( visited.contains( objectWrapper ) )
			return 0 // We have reference graph with cycles.
		visited.add(objectWrapper)

		var size:Long = HEADER_SIZE
		var clazz:Class[_] = o.getClass

		if (clazz.isArray()) {
			if ( clazz == LongArray ) {
				size += o.asInstanceOf[Array[_]].length * LONG_SIZE
			} else if ( clazz == IntArray ) {
				size += o.asInstanceOf[Array[_]].length * INT_SIZE
			} else if ( clazz == ByteArray ) {
				size += o.asInstanceOf[Array[_]].length * BYTE_SIZE
			} else if ( clazz == BooleanArray ) {
				size += o.asInstanceOf[Array[_]].length * BOOLEAN_SIZE
			} else if ( clazz == CharArray ) {
				size += o.asInstanceOf[Array[_]].length * CHAR_SIZE
			} else if ( clazz == ShortArray )  {
				size += o.asInstanceOf[Array[_]].length * SHORT_SIZE
			} else if ( clazz == FloatArray ) {
				size += o.asInstanceOf[Array[_]].length * FLOAT_SIZE
			} else if ( clazz == DoubleArray ) {
				size += o.asInstanceOf[Array[_]].length * DOUBLE_SIZE
			} else {
				o.asInstanceOf[Array[AnyRef]].foreach { o =>
					size += sizeOf( o, visited ) + REFERENCE_SIZE
				}
			}
			size += INT_SIZE
		} else {
      for ( field <- o.getClass.getDeclaredFields;
            if !Modifier.isStatic( field.getModifiers ) ) {

				if ( !field.isAccessible )
					field.setAccessible( true )

        size += (
          field.getGenericType.toString match {
          case "long"    => LONG_SIZE
          case "int"     => INT_SIZE
          case "byte"    => BYTE_SIZE
          case "boolean" => BOOLEAN_SIZE
          case "char"    => CHAR_SIZE
          case "short"   => SHORT_SIZE
          case "float"   => FLOAT_SIZE
          case "double"  => DOUBLE_SIZE
          case _         => sizeOf(field.get(o), visited) + REFERENCE_SIZE
          }
        )
			}
		}

		if ((size % ALIGNMENT) != 0)
			size = ALIGNMENT * (size / ALIGNMENT + 1)

		size
	}
}

object ObjectWrapper {
  val cls = new ObjectWrapper( null ).getClass
}

case class ObjectWrapper( obj:AnyRef ) {

  override def equals( other:Any ):Boolean = {
    if ( other eq null )
      return false

    if ( other.asInstanceOf[AnyRef] eq this )
      return true

    other match {
    case otherOw:ObjectWrapper =>
      obj == otherOw.obj

    case _ =>
      false
    }
  }

  override def hashCode:Int = {
    var hash = 3
    hash = 47 * hash + System.identityHashCode(obj)
    hash
  }
}

