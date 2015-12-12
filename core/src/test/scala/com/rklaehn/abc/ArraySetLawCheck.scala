package com.rklaehn.abc

import algebra.ring.{AdditiveGroup, AdditiveSemigroup, AdditiveMonoid}
import algebra._
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

  def checkAdditiveMonoidLaws[K: Order: ClassTag: Arbitrary, V: Eq: AdditiveSemigroup: ClassTag: Arbitrary](): Unit = {
    val keyName = typeName[K]
    val valueName = typeName[V]
    checkAll(s"GroupLaws[ArrayMap[$keyName,$valueName]].additiveMonoid", GroupLaws[ArrayMap[K, V]].additiveMonoid)
  }

//  def checkGroupLaws[K: Order: ClassTag: Arbitrary, V: Eq: Group: ClassTag: Arbitrary](): Unit = {
//    val keyName = typeName[K]
//    val valueName = typeName[V]
//    checkAll(s"GroupLaws[ArrayMap[$keyName,$valueName]].group", GroupLaws[ArrayMap[K, V]].group)
//  }
  checkEqLaws[Byte, Byte]()
  checkEqLaws[Short, Short]()
  checkEqLaws[Int, Int]()
  checkEqLaws[Long, Long]()
  checkEqLaws[Float, Float]()
  checkEqLaws[Double, Double]()
  checkEqLaws[Boolean, Boolean]()
  checkEqLaws[Char, Char]()
  checkEqLaws[String, String]()
  checkAdditiveMonoidLaws[Byte, Byte]()
  checkAdditiveMonoidLaws[Short, Short]()
  checkAdditiveMonoidLaws[Int, Int]()
  checkAdditiveMonoidLaws[Long, Long]()
  checkAdditiveMonoidLaws[Float, Float]()
  checkAdditiveMonoidLaws[Double, Double]()
  checkAdditiveMonoidLaws[Boolean, Boolean]()
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
  scope {
    implicit def groupFromAdditiveGroup[T: AdditiveGroup]: Group[T] = AdditiveGroup[T].additive
//    checkGroupLaws[Byte, Byte]()
//    checkGroupLaws[Short, Short]()
//    checkGroupLaws[Int, Int]()
//    checkGroupLaws[Long, Long]()
//    checkGroupLaws[Float, Float]()
//    checkGroupLaws[Double, Double]()
//
//    val g = algebra.std.map.mapGroup[Int, Int]
//    val m = Map(1 → 2)
//    val r = g.combine(m, g.inverse(m))
//    val r1 = g.empty
//    val same = Eq.eqv(r, r1)
//    val eqv = Eq[Map[Int, Int]]
//    val same2 = eqv.eqv(r, r1)
//    val same3 = Eq.eqv(Map(1 → 0), Map.empty[Int, Int])
//    checkAll(s"GroupLaws[Map[Int, Int]].group", GroupLaws[Map[Int, Int]].group)
  }
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