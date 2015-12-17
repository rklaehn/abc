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
}
