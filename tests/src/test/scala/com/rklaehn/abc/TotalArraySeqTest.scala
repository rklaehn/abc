package com.rklaehn.abc

import algebra.Eq
import algebra.ring.MultiplicativeMonoid
import cats.implicits._
import org.scalatest.FunSuite

class TotalArraySeqTest extends FunSuite {

  test("Hash") {
    def test[K: Hash](x: TotalArraySeq[K], y: TotalArraySeq[K]): Boolean =
      (Hash.hash(x) == Hash.hash(y)) && Eq.eqv(x, y)
    val x = ArraySeq(1).withDefault(2)
    val y = ArraySeq(1).withDefault(2)
    assert(test(x, y))
  }

  test("apply") {
    val x = ArraySeq(1).withDefault(2)
    assert(x(0) == 1)
    assert(x(1) == 2)
  }

  test("withDefault") {
    assert(ArraySeq(1).withDefault(1).withoutDefault.isEmpty)
  }

  test("equalsWrongType") {
    assert(ArraySeq(1).withDefault(0) != "foo")
  }

  test("noEquals") {
    val a = ArraySeq(NoEquals(1)).withDefault(NoEquals(1))
    val b = ArraySeq(NoEquals(1)).withDefault(NoEquals(1))

    assert(a.show == b.show)
    intercept[UnsupportedOperationException](a.toString)

    assert(Hash.hash(a) == Hash.hash(b))
    intercept[UnsupportedOperationException](a.hashCode)

    assert(Eq.eqv(a, b))
    intercept[UnsupportedOperationException](a.equals(b))
  }
}
