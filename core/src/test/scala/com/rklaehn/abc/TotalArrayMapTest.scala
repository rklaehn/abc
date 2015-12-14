package com.rklaehn.abc

import algebra.Eq
import cats.implicits._
import org.scalatest.FunSuite

class TotalArrayMapTest extends FunSuite {

  test("show") {
    assert(!ArrayMap(1 -> 2).withDefaultValue(3).show.isEmpty)
  }

  test("toString") {
    assert(!ArrayMap(1 -> 2).withDefaultValue(3).toString.isEmpty)
  }

  test("keys") {
    assert(Eq.eqv(ArrayMap(1 -> 2).withDefaultValue(0).keys, ArrayMap(1 -> 2).keys))
  }

  test("iterator") {
    assert(ArrayMap(1 -> 2).withDefaultValue(0).iterator.toSeq == ArrayMap(1 -> 2).iterator.toSeq)
  }

  test("apply") {
    assert(ArrayMap(1 -> 2).withDefaultValue(3).apply(1) == 2)
    assert(ArrayMap(1 -> 2).withDefaultValue(3).apply(-1) == 3)
  }

  test("Hash") {
    def test[K: Hash, V: Hash](x: TotalArrayMap[K, V], y: TotalArrayMap[K, V]): Boolean =
      (Hash.hash(x) == Hash.hash(y)) && Eq.eqv(x, y)
    val x = ArrayMap(1 -> 2).withDefaultValue(3)
    val y = ArrayMap(1 -> 2).withDefaultValue(3)
    assert(test(x, y))
  }

  test("mapValues") {
    // this checks that mapping to the default value will remove the mapping
    assert(ArrayMap(1 -> 2).withDefaultValue(0).mapValues(x => 1).size == 0)
  }

  test("withoutDefault") {
    assert(Eq.eqv(
      ArrayMap(1 -> 2).withDefaultValue(3).withoutDefault,
      ArrayMap(1 -> 2)
    ))
    // making a value the default value filters out all mappings with that value
    assert(Eq.eqv(
      ArrayMap(1 -> 2).withDefaultValue(2).withoutDefault,
      ArrayMap.empty[Int, Int]
    ))
  }
}
