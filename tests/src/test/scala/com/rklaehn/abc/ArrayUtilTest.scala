package com.rklaehn.abc

import org.scalatest.FunSuite
 import cats.kernel.instances.all._

class ArrayUtilTest extends FunSuite {

  test("vectorCompare") {
    val a = Array(0, 0)
    val b = Array(0, 0, 1, 0)
    assert(ArrayUtil.vectorCompare(a, 1, b, 1) > 0)
    assert(ArrayUtil.vectorCompare(b, 1, a, 1) < 0)
  }
}
