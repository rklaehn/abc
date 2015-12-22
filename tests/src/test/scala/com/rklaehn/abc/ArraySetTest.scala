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

  test("equalsWrongType") {
    assert(ArraySet(1) != "foo")
  }

  test("noEquals") {
    val a = ArraySet(NoEquals(1))
    val b = ArraySet(NoEquals(1))

    assert(a.show == b.show)
    intercept[UnsupportedOperationException](a.toString)

    assert(Hash.hash(a) == Hash.hash(b))
    intercept[UnsupportedOperationException](a.hashCode)

    assert(Eq.eqv(a, b))
    intercept[UnsupportedOperationException](a.equals(b))
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

  test("Hash") {
    implicitly[Hash[NegatableArraySet[Int]]]
    def test[K: Hash](x: NegatableArraySet[K], y: NegatableArraySet[K]): Boolean =
      (Hash.hash(x) == Hash.hash(y)) && Eq.eqv(x, y)
    val x = ArraySet(1, 2).asNegatable
    val y = ArraySet(1, 2).asNegatable
    assert(test(x, y))
  }

  test("equalsWrongType") {
    assert(ArraySet(1).asNegatable != "foo")
  }

  test("noEquals") {
    val a = ArraySet(NoEquals(1)).asNegatable
    val b = ArraySet(NoEquals(1)).asNegatable

    assert(a.show == b.show)
    intercept[UnsupportedOperationException](a.toString)

    assert(Hash.hash(a) == Hash.hash(b))
    intercept[UnsupportedOperationException](a.hashCode)

    assert(Eq.eqv(a, b))
    intercept[UnsupportedOperationException](a.equals(b))
  }
}