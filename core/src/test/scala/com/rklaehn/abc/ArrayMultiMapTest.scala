package com.rklaehn.abc

import algebra.Eq
import org.scalatest.FunSuite

class ArrayMultiMapTest extends FunSuite {

  test("apply") {
    assert(Eq.eqv(ArrayMultiMap.fromEntries(1 → 2, 1 → 3), ArrayMultiMap(1 → ArraySet(1, 3))))
  }
}
