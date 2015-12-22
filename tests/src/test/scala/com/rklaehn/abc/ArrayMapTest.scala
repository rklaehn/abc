package com.rklaehn.abc

import algebra.ring.AdditiveGroup
import cats.Show
import cats.implicits._
import algebra.Eq
import org.scalatest.FunSuite

class ArrayMapTest extends FunSuite {

  test("instances") {
    implicitly[Eq[ArrayMap[Int, Int]]]
    implicitly[Show[ArrayMap[Int, Int]]]
    implicitly[Hash[ArrayMap[Int, Int]]]
  }

  test("apply") {
    intercept[NoSuchElementException] {
      ArrayMap(1 → 1).apply(2)
    }
  }

  test("toString") {
    assert(!ArrayMap.empty[Int, Int].toString.isEmpty)
  }

  test("show") {
    assert(!ArrayMap.empty[Int, Int].show.isEmpty)
  }

  test("equalsWrongType") {
    assert(ArrayMap(1 -> 1) != "foo")
  }

  test("noEquals") {
    val a = ArrayMap(NoEquals(1) -> NoEquals(1))
    val b = ArrayMap(NoEquals(1) -> NoEquals(1))

    assert(a.show == b.show)
    intercept[UnsupportedOperationException](a.toString)

    assert(Hash.hash(a) == Hash.hash(b))
    intercept[UnsupportedOperationException](a.hashCode)

    assert(Eq.eqv(a, b))
    intercept[UnsupportedOperationException](a.equals(b))
  }
}
