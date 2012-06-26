package ua.t3hnar.partialboolean

import org.specs2.mutable.SpecificationWithJUnit

/**
 * @author Yaroslav Klymko
 */
class PartialBooleanSpec extends SpecificationWithJUnit {
  "PartialBoolean" should {
    "and" >> {
      val pb1 = AsPb[Int] {
        case 1 => true
        case 2 => false
      }

      val pb2 = AsPb[Int] {
        case 2 => true
        case 3 => false
      }

      val func = pb1 and pb2

      func(0) must beNone
      func(1) must beSome(true)
      func(2) must beSome(false)
      func(3) must beSome(false)
    }

    "with" >> {
      val pb1 = AsPb[Int] {
        case 1 => true
        case 2 => false
        case 3 => true

      }

      val pb2 = AsPb[Int] {
        case 2 => true
        case 3 => true
        case 4 => true
      }

      val func = pb1 `with` pb2

      func(0) must beNone
      func(1) must beNone
      func(2) must beSome(false)
      func(3) must beSome(true)
      func(4) must beNone
    }

    "or" >> {
      val pb1 = AsPb[Int] {
        case 1 => true
        case 2 => false
      }

      val pb2 = AsPb[Int] {
        case 2 => true
        case 3 => false
      }

      val func = pb1 or pb2

      func(0) must beNone
      func(1) must beSome(true)
      func(2) must beSome(true)
      func(3) must beSome(false)
    }

    "not" >> {
      val pb = AsPb[Int] {
        case 1 => true
        case 2 => false
      }

      val func = pb.not

      func(0) must beNone
      func(1) must beSome(false)
      func(2) must beSome(true)
    }

    "except" >> {
      val pb1 = AsPb[Int] {
        case 1 => true
        case 2 => false
        case 3 => true
        case 4 => true
      }

      val pb2 = AsPb[Int] {
        case 2 => true
        case 3 => false
        case 4 => true
        case 5 => false
      }

      val func = pb1 except pb2

      func(0) must beNone
      func(1) must beSome(true)
      func(2) must beSome(false)
      func(3) must beSome(true)
      func(4) must beSome(false)
      func(5) must beNone
    }

    "and except with" >> {
      val pb1 = AsPb[Int] {
        case 1 => true
        case 2 => false
      }

      val pb2 = AsPb[Int] {
        case 2 => true
        case 3 => false
      }

      val pb3 = AsPb[Int] {
        case _ => true
      }

      val func = (pb1 `with` pb2) and pb3
      func(0) must beSome(true)
      func(1) must beSome(true)
      func(2) must beSome(false)
      func(3) must beSome(true)
      func(4) must beSome(true)
    }
  }
}
