package com.rklaehn.abc

import algebra.Eq
import cats.implicits._
import org.scalatest.FunSuite

class ArrayBiMapTest extends FunSuite {

  test("instances") {
    implicitly[Eq[ArrayBiMap[Int, Int]]]
  }

  test("creation") {
    assert(Eq.eqv(ArrayBiMap[Int, Int](), ArrayBiMap.empty[Int, Int]))
    assert(Eq.eqv(ArrayBiMap(1 → 2, 2 → 1), ArrayBiMap(1 → 2) merge ArrayBiMap(2 → 1)))
  }

  test("swap") {
    assert(Eq.eqv(ArrayBiMap(1 → 2, 2 → 3), ArrayBiMap(2 → 1, 3 → 2).swap))
  }

  test("exceptKeys") {
    assert(Eq.eqv(ArrayBiMap(1 → 2, 2 → 3).exceptKeys(ArraySet(1)), ArrayBiMap(2 → 3)))
  }

  test("equals") {
    val a = ArrayBiMap(1 → 2, 2 → 3)
    val b = ArrayBiMap(2 → 3, 1 → 2)
    assert(a == b)
    assert(a != "foo")
  }

  test("hashCode") {
    val a = ArrayBiMap(1 → 2, 2 → 3)
    val b = ArrayBiMap(2 → 3, 1 → 2)
    assert(a.hashCode == b.hashCode)
  }

  test("toString") {
    val a = ArrayBiMap(1 → 2, 2 → 3)
    assert(a.toString == a.show)
  }
}
