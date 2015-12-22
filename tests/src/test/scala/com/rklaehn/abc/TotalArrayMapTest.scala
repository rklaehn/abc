package com.rklaehn.abc

import algebra.{Order, Eq}
import algebra.ring.MultiplicativeMonoid
import cats.implicits._
import org.scalatest.FunSuite

class TotalArrayMapTest extends FunSuite {

  test("show") {
    assert(!ArrayMap(1 -> 2).withDefault(3).show.isEmpty)
  }

  test("toString") {
    assert(!ArrayMap(1 -> 2).withDefault(3).toString.isEmpty)
  }

  test("keys") {
    assert(Eq.eqv(ArrayMap(1 -> 2).withDefault(0).keys, ArrayMap(1 -> 2).keys))
  }

  test("iterator") {
    assert(ArrayMap(1 -> 2).withDefault(0).iterator.toSeq == ArrayMap(1 -> 2).iterator.toSeq)
  }

  test("apply") {
    assert(ArrayMap(1 -> 2).withDefault(3).apply(1) == 2)
    assert(ArrayMap(1 -> 2).withDefault(3).apply(-1) == 3)
  }

  test("Hash") {
    def test[K: Hash, V: Hash](x: TotalArrayMap[K, V], y: TotalArrayMap[K, V]): Boolean =
      (Hash.hash(x) == Hash.hash(y)) && Eq.eqv(x, y)
    val x = ArrayMap(1 -> 2).withDefault(3)
    val y = ArrayMap(1 -> 2).withDefault(3)
    assert(test(x, y))
  }

  test("mapValues") {
    // this checks that mapping to the default value will remove the mapping
    assert(ArrayMap(1 -> 2).withDefault(0).mapValues(x => 1).size == 0)
  }

  test("withoutDefault") {
    assert(Eq.eqv(
      ArrayMap(1 -> 2).withDefault(3).withoutDefault,
      ArrayMap(1 -> 2)
    ))
    // making a value the default value filters out all mappings with that value
    assert(Eq.eqv(
      ArrayMap(1 -> 2).withDefault(2).withoutDefault,
      ArrayMap.empty[Int, Int]
    ))
  }

  test("multiply") {
    // this test is necessary because the arbitary will rarely choose a default value of 1 for lhs and rhs
    val a = ArrayMap(1 -> 2).withDefault(1)
    val b = ArrayMap(1 -> 2).withDefault(1)
    val c = MultiplicativeMonoid.times(a, b)
    assert(c(0) == 1)
    assert(c(1) == 4)
  }

  test("order") {
    val a = ArrayMap(1 → 1, 2 → 1, 3 → 1).withDefault(0)
    val b = ArrayMap(1 → 1, 2 → 1, 3 → 2).withDefault(0)
    assert(Order.compare(a, b) < 0)
  }

  test("equalsWrongType") {
    assert(ArrayMap(1 -> 1).withDefault(0) != "foo")
  }

  test("noEquals") {
    val a = ArrayMap(NoEquals(1) -> NoEquals(1)).withDefault(NoEquals(1))
    val b = ArrayMap(NoEquals(1) -> NoEquals(1)).withDefault(NoEquals(1))

    assert(a.show == b.show)
    intercept[UnsupportedOperationException](a.toString)

    assert(Hash.hash(a) == Hash.hash(b))
    intercept[UnsupportedOperationException](a.hashCode)

    assert(Eq.eqv(a, b))
    intercept[UnsupportedOperationException](a.equals(b))
  }
}
