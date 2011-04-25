
package org.tyranid.logic

import scala.xml.NodeSeq

import _root_.net.liftweb.common.{ Box, Empty }
import _root_.net.liftweb.http.js.JsCmd
import _root_.net.liftweb.http.js.JsCmds.Noop

sealed trait Tern[+A] {
  def |[ B >: A ]( b: => B ): B
}

case class TernFirst[+A]( a: () => A ) extends Tern[A] {
  def |[ B >: A ]( b: => B ) = a()
}

case object TernSecond extends Tern[Nothing] {
  def |[B]( b: => B ) = b
}

class BooleanImp( bval:Boolean ) {

  /**
   * Monoids.
   *
   * Similar to ScalaZ's ?? operator, but ?? is annoying to use because it (the question mark) has the highest precedence.
   *
   * This uses "|" because it has the lowest precedence.
   * It uses "*" because, loosely(!) ...
   *
   *   true  * a:A = a
   *   false * a:A = Monoid[A].Zero
   *
   * Another way to read it is that "|" is usually a logical or binary operator, so it is "logical multiplication".
   *
   * This is hard-coded to the String-concatenation, NodeSeq-concatenation, monad-bind, etc. monoids because they were the only real/immediate need and I'd rather get Scalaz working than re-implement it.
   *
   * Example:
   *
   * <span>
   *  { val != 0 |* <span>another span</span> }
   * </span>
   */

  def |*   ( v: => String    ):String    = if ( bval ) v else ""
  def |*   ( v: => NodeSeq   ):NodeSeq   = if ( bval ) v else NodeSeq.Empty
  def |*   ( v: => JsCmd     ):JsCmd     = if ( bval ) v else Noop
  def |*[T]( v: => Option[T] ):Option[T] = if ( bval ) v else None
  def |*[T]( v: => Box[T]    ):Box[T]    = if ( bval ) v else Empty
  def |*[T]( v: => Unit      ):Unit      = if ( bval ) v

  /**
   * C's ternary operator.  It uses | instead of :.  Inspired by Lift's similar operator.
   *
   * Example:
   *
   * val str = i.isLess ? "less" | "greater"
   */
  def ?[A]( a: => A ) = if ( bval ) TernFirst( () => a ) else TernSecond
}

