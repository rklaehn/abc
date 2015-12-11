package com.rklaehn.abc

import algebra.{Order, Eq}
import algebra.laws._
import algebra.std.all._
import org.scalacheck.Arbitrary
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.typelevel.discipline.scalatest.Discipline
import arb._

import scala.reflect.ClassTag

class ArraySetLawCheck extends FunSuite with Discipline {

  def checkLaws[T: Order: ClassTag: Arbitrary](): Unit = {
    val name = implicitly[ClassTag[T]].runtimeClass.getSimpleName
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

class ArraySetCheck extends FunSuite with Checkers {

  def checkHashing[T: Order: ClassTag: Arbitrary](): Unit = {
    val name = implicitly[ClassTag[T]].runtimeClass.getSimpleName
    test(s"hashConsistency $name") {
      check { xs: Vector[T] ⇒
        Eq.eqv(ArraySet(xs: _*), ArraySet(xs.reverse: _*)) &&
        Hash.hash(ArraySet(xs: _*)) == Hash.hash(ArraySet(xs.reverse: _*))
      }
    }
  }
  checkHashing[Byte]()
  checkHashing[Short]()
  checkHashing[Int]()
  checkHashing[Long]()
  checkHashing[Float]()
  checkHashing[Double]()
  checkHashing[Boolean]()
  checkHashing[Char]()
  checkHashing[String]()
}