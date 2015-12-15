package com.rklaehn.abc

import algebra.Eq
import cats.implicits._
import org.scalatest.FunSuite

class MiscTest extends FunSuite {
  test("arrayHash") {
    assert(Eq.neqv(Array(1,2), Array(1,3))(Hash[Array[Int]]))
  }
}
class ArraySetTest extends FunSuite {

  test("instances") {
    implicitly[Eq[ArraySet[Int]]]
  }

  test("toString") {
    assert(!ArraySet.empty[Int].toString.isEmpty)
  }

  test("show") {
    assert(!ArraySet.empty[Int].show.isEmpty)
  }
}

class NegatableArraySetTest extends FunSuite {

  test("show") {
    assert(!NegatableArraySet.empty[Int].show.isEmpty)
    assert(!NegatableArraySet.all[Int].show.isEmpty)
    assert(!NegatableArraySet(1).show.isEmpty)
    assert(!NegatableArraySet(1).negate.show.isEmpty)
  }

  test("Eq") {
    def eqv[T: Eq](a: NegatableArraySet[T], b: NegatableArraySet[T]) =
      Eq.eqv(a, b)
    assert(eqv(NegatableArraySet(1), NegatableArraySet(1)))
    assert(!eqv(NegatableArraySet(1), NegatableArraySet(2)))
  }
}