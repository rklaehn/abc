package com.rklaehn.abc

import algebra.Eq
import cats.implicits._
import org.scalatest.FunSuite

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