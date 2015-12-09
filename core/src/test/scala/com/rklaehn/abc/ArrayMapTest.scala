package com.rklaehn.abc

import org.scalatest.FunSuite

class ArrayMapTest extends FunSuite {

  test("apply") {
    intercept[NoSuchElementException] {
      ArrayMap(1 → 1).apply(2)
    }
  }
}
