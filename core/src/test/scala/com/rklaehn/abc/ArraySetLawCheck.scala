package com.rklaehn.abc

import algebra.laws._
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline
import arb._

class ArraySetLawCheck extends FunSuite with Discipline {

  checkAll("OrderLaws[ArraySet[Int]].partialOrder", OrderLaws[ArraySet[Int]].partialOrder)
  checkAll("RingLaws[ArraySet[Int]].semiring", RingLaws[ArraySet[Int]].semiring)
}
