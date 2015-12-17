package com.rklaehn.abc

import org.scalatest.FunSuite
import algebra.std.all._

class SubsetOfTest extends FunSuite {

  test("subsetOf") {
    val a = ArraySet(0 until 1000 :_*)
    val b = ArraySet(500 until 1500 :_*)
    assert(! a.subsetOf(b))
  }
}
