package ua.t3hnar

/**
 * @author Yaroslav Klymko
 */
package object partialboolean {
  implicit def pb2pbExt[T](pb: T => Option[Boolean]): PbExtended[T] = new PbExtended(pb)

  type Pb[T] = T => Option[Boolean]

  trait PbTrait[T] extends Pb[T] {
    def apply(v: T): Option[Boolean]
  }

  trait PbSemantic[T] {
    def and(pb: Pb[T]): Pb[T]
    def or(pb: Pb[T]): Pb[T]
    def not: Pb[T]
    def `with`(pb: Pb[T]): Pb[T]
    def except(pb: Pb[T]): Pb[T]
  }

  object AsPb {
    def apply[T](pf: PartialFunction[T, Boolean]): Pb[T] = pf.lift
  }

  implicit def pb2ext[T](pb: Pb[T]) = new {
    def ext: PbExtended[T] = new PbExtended[T](pb)
  }

  class PbExtended[T](underlying: Pb[T]) extends PbTrait[T] with PbSemantic[T] {
    def apply(v: T) = underlying(v)

    def and(pb: Pb[T]): Pb[T] = new PbAnd(underlying, pb)
    def or(pb: Pb[T]): Pb[T] = new PbOr(underlying, pb)
    def not: Pb[T] = new PbNot(underlying)
    def `with`(pb: Pb[T]): Pb[T] = new PbWith(underlying, pb)
    def except(pb: Pb[T]): Pb[T] = new PbExcept(underlying, pb)
  }

  abstract class PbTuple[T](pb1: Pb[T], pb2: Pb[T]) extends PbTrait[T] {
    def applyPf(): PartialFunction[(Option[Boolean], Option[Boolean]), Boolean]
    def apply(v: T) = PartialFunction.condOpt(pb1(v) -> (pb2(v)))(applyPf())
  }

  trait Intersection[T] {
    self: PbTuple[T] =>

    def applyPf(): PartialFunction[(Option[Boolean], Option[Boolean]), Boolean] = {
      case (Some(v1), Some(v2)) => intersect(v1, v2)
      case (Some(v), None) => v
      case (None, Some(v)) => v
    }

    def intersect(v1: Boolean, v2: Boolean): Boolean
  }

  class PbAnd[T](pb1: Pb[T], pb2: Pb[T]) extends PbTuple(pb1, pb2) with Intersection[T] {
    def intersect(v1: Boolean, v2: Boolean) = v1 && v2
  }

  class PbOr[T](pb1: Pb[T], pb2: Pb[T]) extends PbTuple(pb1, pb2) with Intersection[T] {
    def intersect(v1: Boolean, v2: Boolean) = v1 || v2
  }

  class PbNot[T](pb: Pb[T]) extends PbTrait[T] {
    def apply(v: T) = pb(v).map(b => !b)
  }

  class PbWith[T](pb1: Pb[T], pb2: Pb[T]) extends PbTuple(pb1, pb2) {
    def applyPf() = {
      case (Some(v1), Some(v2)) => v1 && v2
    }
  }

  class PbExcept[T](pb1: Pb[T], pb2: Pb[T]) extends PbTuple(pb1, pb2) {
    def applyPf() = {
      case (Some(v1), Some(v2)) => v1 && !v2
      case (Some(v1), None) => v1
    }
  }
}