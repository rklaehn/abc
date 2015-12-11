package com.rklaehn.abc

import algebra.std.all._
import algebra.Eq
import org.scalatest.FunSuite

class ArrayMapTest extends FunSuite {

  test("instances") {
    implicitly[Eq[ArrayMap[Int, Int]]]
  }

  test("apply") {
    intercept[NoSuchElementException] {
      ArrayMap(1 → 1).apply(2)
    }
  }

  test("toString") {
    assert(!ArrayMap.empty[Int, Int].toString.isEmpty)
  }
}
