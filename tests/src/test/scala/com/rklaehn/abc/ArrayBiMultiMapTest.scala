package com.rklaehn.abc

import algebra.Eq
 import cats.kernel.instances.all._
import org.scalatest.FunSuite

class ArrayBiMultiMapTest extends FunSuite {

  test("instances") {
    implicitly[Eq[ArrayBiMultiMap[Int, Int]]]
  }

  test("toString") {
    assert(!ArrayBiMultiMap.empty[Int, Int].toString.isEmpty)
  }

  test("creation") {
    assert(Eq.eqv(ArrayBiMultiMap(1 → 2, 1 → 3, 4 → 2, 4 → 3).kv, ArrayMultiMap(1 → ArraySet(2, 3), 4 → ArraySet(2, 3))))
    assert(Eq.eqv(ArrayBiMultiMap(1 → 2, 1 → 3, 4 → 2, 4 → 3), ArrayBiMultiMap(1 → 3, 4 → 2, 4 → 3, 1 → 2)))
    assert(Eq.eqv(ArrayBiMultiMap.singleton(1, 2), ArrayBiMultiMap(1 → 2)))
    assert(Eq.eqv(ArrayBiMultiMap.singleton(1, 2), ArrayBiMultiMap.fromMultiMap(ArrayMultiMap.fromEntries(1 → 2))))
  }

  test("merge") {
    assert(Eq.eqv(ArrayBiMultiMap.singleton(1, 2) merge ArrayBiMultiMap(1 → 3), ArrayBiMultiMap(1 → 2, 1 → 3)))
  }

  test("exceptKeys") {
    assert(Eq.eqv(ArrayBiMultiMap(1 → 2, 1 → 3).exceptKeys(ArraySet(1)), ArrayBiMultiMap.empty[Int, Int]))
  }

  test("exceptValues") {
    assert(Eq.eqv(ArrayBiMultiMap(1 → 2, 1 → 3).exceptValues(ArraySet(2)), ArrayBiMultiMap(1 → 3)))
  }

  test("except") {
    assert(Eq.eqv(ArrayBiMultiMap(1 → 2, 1 → 3).except(ArrayBiMultiMap(1 → 2)), ArrayBiMultiMap(1 → 3)))
  }

  test("swap") {
    val a = ArrayBiMultiMap(1 → 2, 1 → 3, 4 → 2, 4 → 3)
    assert(a.kv eq a.swap.vk)
    assert(a.vk eq a.swap.kv)
  }

  test("equals") {
    val a = ArrayBiMultiMap(1 → 2, 1 → 3, 4 → 2, 4 → 3)
    val b = ArrayBiMultiMap(1 → 2, 1 → 3, 4 → 2, 4 → 3)
    assert(a == b)
    assert(a != "foo")
  }
}
