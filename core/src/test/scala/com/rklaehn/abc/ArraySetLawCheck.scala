package com.rklaehn.abc

import algebra.Eq
import algebra.laws._
import algebra.std.all._
import org.scalacheck.Arbitrary
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.typelevel.discipline.scalatest.Discipline
import arb._

import scala.util.hashing.Hashing

class ArraySetLawCheck extends FunSuite with Discipline {

  def checkLaws[T: OrderedArrayTag: Arbitrary](): Unit = {
    val name = OrderedArrayTag[T].classTag.runtimeClass.getSimpleName
    checkAll(s"OrderLaws[ArraySet[$name]].partialOrder", OrderLaws[ArraySet[T]].partialOrder)
    checkAll(s"RingLaws[ArraySet[$name]].semiring", RingLaws[ArraySet[T]].semiring)
  }
  checkLaws[Byte]()
  checkLaws[Short]()
  checkLaws[Int]()
  checkLaws[Long]()
  checkLaws[Float]()
  checkLaws[Double]()
  checkLaws[Boolean]()
  checkLaws[Char]()
  checkLaws[String]()
}

//class ArraySetCheck extends FunSuite with Checkers {
//
//  val h = implicitly[Hashing[ArraySet[Int]]]
//  test("foo") {
//    check { xs: Vector[Int] ⇒
//      h.hash(ArraySet(xs: _*)) == h.hash(ArraySet(xs.reverse: _*))
//    }
//  }
//}