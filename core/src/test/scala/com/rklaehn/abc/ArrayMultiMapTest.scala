package com.rklaehn.abc

import algebra.Eq
import org.scalatest.FunSuite

class ArrayMultiMapTest extends FunSuite {

  test("instances") {
    implicitly[Eq[ArrayMultiMap[Int, Int]]]
  }

  test("apply") {
    assert(Eq.eqv(ArrayMultiMap.fromEntries(1 → 2, 1 → 3, 2 → 1), ArrayMultiMap(1 → ArraySet(2, 3), 2 → ArraySet(1))))
    assert(Eq.eqv(ArrayMultiMap.fromEntries[Int, Int](), ArrayMultiMap.apply[Int, Int]()))
  }

  test("except") {
    val a = ArrayMultiMap.fromEntries(1 → 2, 1 → 3, 2 → 1).except(ArrayMultiMap.fromEntries(1 → 3, 2 → 1))
    val b = ArrayMultiMap.fromEntries(1 → 2)
    assert(Eq.eqv(a, b))
  }

  test("inverse") {
    val a = ArrayMultiMap.fromEntries(1 → 1, 2 → 1)
    val ainv = a.inverse
    val b = ArrayMultiMap.fromEntries(1 → 1, 1 → 2)
    assert(Eq.eqv(ainv, b))
  }

  test("toString") {
    assert(!ArrayMultiMap.empty[Int, Int].toString.isEmpty)
  }
}
