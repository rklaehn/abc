package com.rklaehn.abc

import algebra.laws._
import org.scalacheck.Arbitrary
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

class ArraySetLawCheck extends FunSuite with Discipline {

  implicit def arraySetArbitrary[T: Arbitrary: OrderedArrayTag]: Arbitrary[ArraySet[T]] = Arbitrary {
    for {
      list <- Arbitrary.arbitrary[List[T]]
    } yield
      ArraySet(list: _*)
  }

  checkAll("OrderLaws[ArraySet[Int]].partialOrder", OrderLaws[ArraySet[Int]].partialOrder)
  checkAll("RingLaws[ArraySet[Int]].semiring", RingLaws[ArraySet[Int]].semiring)
}
