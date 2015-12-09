package com.rklaehn.abc

import org.scalatest.FunSuite

class ArraySeqTest extends FunSuite {

  test("length") {
    val t = ArraySeq(1, 2, 3)
    assert(t.length === 3)
  }

  test("apply") {
    val t = ArraySeq(1, 2, 3)
    assert(t(0) == 1)
    assert(t(1) == 2)
    assert(t(2) == 3)
    intercept[IndexOutOfBoundsException] {
      t(-1)
    }
    intercept[IndexOutOfBoundsException] {
      t(3)
    }
  }

  test("concat") {
    val t = ArraySeq(1, 2, 3)
    val u = ArraySeq(4,5,6)
    assert(t concat ArraySeq.empty[Int] eq t)
    assert(ArraySeq.empty[Int] concat t eq t)
    assert((t concat u).length === 6)
  }
}
