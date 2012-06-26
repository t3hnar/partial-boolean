Partial Boolean
==============

Example
-------

```scala
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
```