package com.rklaehn.abc

import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop._
import spire.syntax.eq._

object ArraySetArbitrary {

  implicit val arbArraySet = Arbitrary {
    for {
      x ← Arbitrary.arbContainer[Vector, Int].arbitrary
    } yield
      ArraySet(x: _*)
  }
}

object ArraySetSampleCheck extends Properties("ArraySet") {
  import ArraySetArbitrary.arbArraySet

  def unaryOp(a: ArraySet[Int], r: ArraySet[Int], op: Boolean ⇒ Boolean): Boolean = {
    val samples = a.elements :+ Int.MinValue
    samples.forall { e ⇒
      r(e) == op(a(e))
    }
  }

  def binaryOp(a: ArraySet[Int], b: ArraySet[Int], r: ArraySet[Int], op: (Boolean, Boolean) ⇒ Boolean): Boolean = {
    val samples = (a.elements ++ b.elements).distinct :+ Int.MinValue
    samples.forall { e ⇒
      r(e) == op(a(e), b(e))
    }
  }

  property("and") = forAll { (x: ArraySet[Int], y: ArraySet[Int]) ⇒
    binaryOp(x, y, x intersect y, _ & _)
  }

  property("or") = forAll { (x: ArraySet[Int], y: ArraySet[Int]) ⇒
    binaryOp(x, y, x union y, _ | _)
  }

  property("xor") = forAll { (x: ArraySet[Int], y: ArraySet[Int]) ⇒
    binaryOp(x, y, x xor y, _ ^ _)
  }

  property("diff") = forAll { (x: ArraySet[Int], y: ArraySet[Int]) ⇒
    binaryOp(x, y, x diff y, (x,y) ⇒ x & !y)
  }

  property("subsetOf") = forAll { (x: ArraySet[Int], y: ArraySet[Int]) ⇒
    val r1 = x subsetOf y
    val r0 = (x union y) === y
    r0 == r1
  }

  property("intersects") = forAll { (x: ArraySet[Int], y: ArraySet[Int]) ⇒
    val r1 = x intersects y
    val r0 = !(x intersect y).isEmpty
    r0 == r1
  }

  property("toString") = forAll { x: ArraySet[Int] ⇒
    !x.toString.isEmpty
  }

  property("iterator") = forAll { x: ArraySet[Int] ⇒
    import spire.implicits._
    x.iterator.toArray === x.elements
  }

  property("isEmpty") = forAll { x: ArraySet[Int] ⇒
    if(x.isEmpty)
      x.asArraySeq.isEmpty
    else
      true
  }

  property("+") = forAll { (x: ArraySet[Int], y: Int) ⇒
    (x + y).contains(y)
  }

  property("-") = forAll { (x: ArraySet[Int], y: Int) ⇒
    !(x - y).contains(y)
  }

  property("filter") = forAll { (x: ArraySet[Int], y: ArraySet[Int]) ⇒
    (x diff y) === (x filter(e ⇒ !y.contains(e)))
  }
}