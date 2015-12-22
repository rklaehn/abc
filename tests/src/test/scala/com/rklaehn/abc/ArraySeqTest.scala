package com.rklaehn.abc

import algebra.{Monoid, Eq}
import cats.implicits._
import org.scalatest.FunSuite

class ArraySeqTest extends FunSuite {

  test("instances") {
    case class Foo(x: Int)
    implicit val x: Eq[Foo] = Eq.by(_.x)
    assert(Eq.eqv(ArraySeq(Foo(1)), ArraySeq(Foo(1))))
  }

  test("length") {
    val t = ArraySeq(1, 2, 3)
    assert(t.length == 3)
  }

  test("apply") {
    val t = ArraySeq(1, 2, 3)
    assert(t(0) == 1)
    assert(t(1) == 2)
    assert(t(2) == 3)
    intercept[IndexOutOfBoundsException] {
      t(-1)
    }
    intercept[IndexOutOfBoundsException] {
      t(3)
    }
  }

  test("concat") {
    val t = ArraySeq(1, 2, 3)
    val u = ArraySeq(4,5,6)
    assert(t concat ArraySeq.empty[Int] eq t)
    assert(ArraySeq.empty[Int] concat t eq t)
    assert((t concat u).length == 6)
  }

  test("toString") {
    assert(!ArraySeq.empty[Int].toString.isEmpty)
  }

  test("show") {
    assert(!ArraySeq.empty[Int].show.isEmpty)
  }

  test("monoid") {
    assert(Eq.eqv(
      Monoid.combineAll(Seq(ArraySeq(1), ArraySeq(2))),
      Monoid.combine(ArraySeq(1),ArraySeq(2))
      ))
  }

  test("equalsWrongType") {
    assert(ArraySeq(1) != "foo")
  }

  test("noEquals") {
    val a = ArraySeq(NoEquals(1))
    val b = ArraySeq(NoEquals(1))

    assert(a.show == b.show)
    intercept[UnsupportedOperationException](a.toString)

    assert(Hash.hash(a) == Hash.hash(b))
    intercept[UnsupportedOperationException](a.hashCode)

    assert(Eq.eqv(a, b))
    intercept[UnsupportedOperationException](a.equals(b))
  }
}
