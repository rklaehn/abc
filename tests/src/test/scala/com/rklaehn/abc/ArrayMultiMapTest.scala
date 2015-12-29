package com.rklaehn.abc

import cats.implicits._
import algebra.Eq
import cats.Show
import org.scalatest.FunSuite

class ArrayMultiMapTest extends FunSuite {

  test("instances") {
    implicitly[Eq[ArrayMultiMap[Int, Int]]]
    implicitly[Show[ArrayMultiMap[Int, Int]]]
  }

  test("apply") {
    assert(Eq.eqv(ArrayMultiMap.fromEntries(1 → 2, 1 → 3, 2 → 1), ArrayMultiMap(1 → ArraySet(2, 3), 2 → ArraySet(1))))
    assert(Eq.eqv(ArrayMultiMap.fromEntries[Int, Int](), ArrayMultiMap.apply[Int, Int]()))
    assert(Eq.eqv(ArrayMultiMap(1 → ArraySet.empty[Int]), ArrayMultiMap.empty[Int, Int]))
  }

  test("entries") {
    assert(ArrayMultiMap.fromEntries(1 → 2, 1 → 3, 2 → 3).entries.toSet == Set(1 → 2, 1 → 3, 2 → 3))
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
    assert(!ArrayMultiMap.empty[Int, Int].show.isEmpty)
  }

  test("hashCode") {
    val x = ArrayMultiMap.empty[Int, Int]
    assert(x.hashCode == Hash.hash(x))
  }

  test("equals") {
    val a = ArrayMultiMap.fromEntries(1 → 1, 2 → 1)
    assert(a == a.inverse.inverse)
    assert(a != "foo")
  }
}
