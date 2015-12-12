package com.rklaehn.abc

import algebra.ring.{AdditiveSemigroup, AdditiveMonoid}
import algebra.{Monoid, Semigroup, Eq, Order}
import algebra.laws._
import algebra.std.all._
import org.scalacheck.Arbitrary
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.typelevel.discipline.scalatest.Discipline
import arb._

import scala.reflect.ClassTag

trait Helpers {

  def typeName[K: ClassTag] = implicitly[ClassTag[K]].runtimeClass.getSimpleName

  def scope[T](f: ⇒ T) = f
}

class ArrayMapLawCheck extends FunSuite with Discipline with Helpers {

  def checkEqLaws[K: Order: ClassTag: Arbitrary, V: Eq: ClassTag: Arbitrary](): Unit = {
    val keyName = typeName[K]
    val valueName = typeName[V]
    checkAll(s"OrderLaws[ArrayMap[$keyName,$valueName]].eqv", OrderLaws[ArrayMap[K, V]].eqv)
  }

  def checkMonoidLaws[K: Order: ClassTag: Arbitrary, V: Eq: Semigroup: ClassTag: Arbitrary](): Unit = {
    val keyName = typeName[K]
    val valueName = typeName[V]
    checkAll(s"GroupLaws[ArrayMap[$keyName,$valueName]].monoid", GroupLaws[ArrayMap[K, V]].monoid)
  }
  checkEqLaws[Byte, Byte]()
  checkEqLaws[Short, Short]()
  checkEqLaws[Int, Int]()
  checkEqLaws[Long, Long]()
  checkEqLaws[Float, Float]()
  checkEqLaws[Double, Double]()
  checkEqLaws[Boolean, Boolean]()
  checkEqLaws[Char, Char]()
  checkEqLaws[String, String]()
  scope {
    implicit def monoidFromAdditiveMonoid[T: AdditiveSemigroup]: Semigroup[T] = AdditiveSemigroup[T].additive
    checkMonoidLaws[Byte, Byte]()
    checkMonoidLaws[Short, Short]()
    checkMonoidLaws[Int, Int]()
    checkMonoidLaws[Long, Long]()
    checkMonoidLaws[Float, Float]()
    checkMonoidLaws[Double, Double]()
    checkMonoidLaws[Boolean, Boolean]()
  }
  checkMonoidLaws[String, String]()
}

class ArraySeqLawCheck extends FunSuite with Discipline with Helpers {

  def checkLaws[T: Order: ClassTag: Arbitrary](): Unit = {
    val name = typeName[T]
    checkAll(s"OrderLaws[ArraySet[$name]].eqv", OrderLaws[ArraySeq[T]].eqv)
    checkAll(s"GroupLaws[ArraySeq[$name]].monoid", GroupLaws[ArraySeq[T]].monoid)
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

class ArraySetLawCheck extends FunSuite with Discipline with Helpers {

  def checkLaws[T: Order: ClassTag: Arbitrary](): Unit = {
    val name = typeName[T]
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

class ArraySetCheck extends FunSuite with Checkers with Helpers {

  def checkHashing[T: Order: Hash: ClassTag: Arbitrary](): Unit = {
    val name = typeName[T]
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