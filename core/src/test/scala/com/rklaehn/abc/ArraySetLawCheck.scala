package com.rklaehn.abc

import algebra.ring._
import algebra._
import algebra.laws._
import algebra.std.Rat
import algebra.std.all._
import cats.laws.FoldableLaws
import cats.laws.discipline.FoldableTests
import org.scalacheck.Arbitrary
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.typelevel.discipline.Predicate
import org.typelevel.discipline.scalatest.Discipline
import arb._

trait Helpers {

  def typeName[K: ClassTag] = implicitly[ClassTag[K]].runtimeClass.getSimpleName

  def scope[T](f: ⇒ T) = f
}

class ArrayTotalMapLawCheck extends FunSuite with Discipline with Helpers {

  implicit val nonZeroRatArbitrary =
    Arbitrary(for {
      (n, d) <- Arbitrary.arbitrary[(BigInt, BigInt)]
    } yield {
      val n1 = if(n.signum != 0) n else BigInt(1)
      val d1 = if(d.signum != 0) d else BigInt(1)
      Rat(n1, d1)
    })

  implicit def allNonZero[K, V: AdditiveMonoid : Eq] = Predicate { x: TotalArrayMap[K, V] ⇒
    !AdditiveMonoid.isZero(x.default) && !x.values.elements.exists(e ⇒ AdditiveMonoid.isZero(e))
  }

  def checkEqLaws[K: Order: ClassTag: Arbitrary, V: Eq: ClassTag: Arbitrary](): Unit = {
    val keyName = typeName[K]
    val valueName = typeName[V]
    checkAll(s"OrderLaws[TotalArrayMap[$keyName,$valueName]].eqv", OrderLaws[TotalArrayMap[K, V]].eqv)
  }

  def checkMonoidLaws[K: Order: ClassTag: Arbitrary, V: Eq: Monoid: ClassTag: Arbitrary](): Unit = {
    val keyName = typeName[K]
    val valueName = typeName[V]
    checkAll(s"GroupLaws[TotalArrayMap[$keyName,$valueName]].monoid", GroupLaws[TotalArrayMap[K, V]].monoid)
  }

  def checkGroupLaws[K: Order: ClassTag: Arbitrary, V: Eq: Group: ClassTag: Arbitrary](): Unit = {
    val keyName = typeName[K]
    val valueName = typeName[V]
    checkAll(s"GroupLaws[TotalArrayMap[$keyName,$valueName]].group", GroupLaws[TotalArrayMap[K, V]].group)
  }

  def checkAdditiveMonoidLaws[K: Order: ClassTag: Arbitrary, V: Eq: AdditiveMonoid: ClassTag: Arbitrary](): Unit = {
    val keyName = typeName[K]
    val valueName = typeName[V]
    checkAll(s"GroupLaws[TotalArrayMap[$keyName,$valueName]].additiveMonoid", GroupLaws[TotalArrayMap[K, V]].additiveMonoid)
  }

  def checkAdditiveGroupLaws[K: Order: ClassTag: Arbitrary, V: Eq: AdditiveGroup: ClassTag: Arbitrary](): Unit = {
    val keyName = typeName[K]
    val valueName = typeName[V]
    checkAll(s"GroupLaws[TotalArrayMap[$keyName,$valueName]].additiveMonoid", GroupLaws[TotalArrayMap[K, V]].additiveGroup)
  }

  def checkMultiplicativeMonoidLaws[K: Order: ClassTag: Arbitrary, V: Eq: AdditiveMonoid: MultiplicativeMonoid: ClassTag: Arbitrary](): Unit = {
    val keyName = typeName[K]
    val valueName = typeName[V]
    checkAll(s"RingLaws[TotalArrayMap[$keyName,$valueName]].multiplicativeMonoid", RingLaws[TotalArrayMap[K, V]].multiplicativeMonoid)
  }

  def checkMultiplicativeGroupLaws[K: Order: ClassTag: Arbitrary, V: Eq: AdditiveMonoid: MultiplicativeGroup: ClassTag: Arbitrary](): Unit = {
    val keyName = typeName[K]
    val valueName = typeName[V]
    checkAll(s"RingLaws[TotalArrayMap[$keyName,$valueName]].multiplicativeGroup", RingLaws[TotalArrayMap[K, V]].multiplicativeGroup)
  }

  def checkSemiringLaws[K: Order: ClassTag: Arbitrary, V: Eq: Semiring: ClassTag: Arbitrary](): Unit = {
    val keyName = typeName[K]
    val valueName = typeName[V]
    checkAll(s"RingLaws[TotalArrayMap[$keyName,$valueName]].multiplicativeGroup", RingLaws[TotalArrayMap[K, V]].semiring)
  }

  def checkRngLaws[K: Order: ClassTag: Arbitrary, V: Eq: Rng: ClassTag: Arbitrary](): Unit = {
    val keyName = typeName[K]
    val valueName = typeName[V]
    checkAll(s"RingLaws[TotalArrayMap[$keyName,$valueName]].rng", RingLaws[TotalArrayMap[K, V]].rng)
  }

  checkEqLaws[Byte, Byte]()
  checkEqLaws[Short, Short]()
  checkEqLaws[Int, Int]()
  checkEqLaws[Float, Float]()
  checkEqLaws[Double, Double]()
  checkEqLaws[Boolean, Boolean]()
  checkEqLaws[Char, Char]()
  checkEqLaws[String, String]()

  checkAdditiveMonoidLaws[Byte, Byte]()
  checkAdditiveMonoidLaws[Short, Short]()
  checkAdditiveMonoidLaws[Int, Int]()
  checkAdditiveMonoidLaws[Boolean, Boolean]()
  checkAdditiveGroupLaws[Byte, Byte]()
  checkAdditiveGroupLaws[Short, Short]()
  checkAdditiveGroupLaws[Int, Int]()

  checkMultiplicativeMonoidLaws[Int, Rat]()
  checkMultiplicativeGroupLaws[Int, Rat]()
  scope {
    implicit def monoidFromAdditiveMonoid[T: AdditiveMonoid]: Monoid[T] = AdditiveMonoid[T].additive
    checkMonoidLaws[Byte, Byte]()
    checkMonoidLaws[Short, Short]()
    checkMonoidLaws[Int, Int]()
    checkMonoidLaws[Boolean, Boolean]()
  }
  scope {
    implicit def groupFromAdditiveGroup[T: AdditiveGroup]: Group[T] = AdditiveGroup[T].additive
    checkGroupLaws[Byte, Byte]()
    checkGroupLaws[Short, Short]()
    checkGroupLaws[Int, Int]()
  }
  checkMonoidLaws[String, String]()
  checkSemiringLaws[Int, Byte]()
  checkSemiringLaws[Int, Short]()
  checkSemiringLaws[Int, Int]()

  checkRngLaws[Int, Byte]()
  checkRngLaws[Int, Short]()
  checkRngLaws[Int, Int]()
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
  checkEqLaws[Byte, Byte]()
  checkEqLaws[Short, Short]()
  checkEqLaws[Int, Int]()
  checkEqLaws[Float, Float]()
  checkEqLaws[Double, Double]()
  checkEqLaws[Boolean, Boolean]()
  checkEqLaws[Char, Char]()
  checkEqLaws[String, String]()
  checkAdditiveMonoidLaws[Byte, Byte]()
  checkAdditiveMonoidLaws[Short, Short]()
  checkAdditiveMonoidLaws[Int, Int]()
  checkAdditiveMonoidLaws[Float, Float]()
  checkAdditiveMonoidLaws[Double, Double]()
  checkAdditiveMonoidLaws[Boolean, Boolean]()
  scope {
    implicit def monoidFromAdditiveMonoid[T: AdditiveSemigroup]: Semigroup[T] = AdditiveSemigroup[T].additive
    checkMonoidLaws[Byte, Byte]()
    checkMonoidLaws[Short, Short]()
    checkMonoidLaws[Int, Int]()
    checkMonoidLaws[Float, Float]()
    checkMonoidLaws[Double, Double]()
    checkMonoidLaws[Boolean, Boolean]()
  }
  checkMonoidLaws[String, String]()
}

class ArraySeqLawCheck extends FunSuite with Discipline with Helpers {

  def checkLaws[T: Order: ClassTag: Arbitrary](): Unit = {
    val name = typeName[T]
    checkAll(s"OrderLaws[ArraySeq[$name]].eqv", OrderLaws[ArraySeq[T]].eqv)
    checkAll(s"OrderLaws[ArraySeq[$name]].order", OrderLaws[ArraySeq[T]].order)
    checkAll(s"GroupLaws[ArraySeq[$name]].monoid", GroupLaws[ArraySeq[T]].monoid)
  }
  checkLaws[Byte]()
  checkLaws[Short]()
  checkLaws[Int]()
  checkLaws[Float]()
  checkLaws[Double]()
  checkLaws[Boolean]()
  checkLaws[Char]()
  checkLaws[String]()
  scope {
    implicit val intMonoid = AdditiveMonoid[Int].additive
    checkAll(s"FoldableTests[ArraySeq[T]]", FoldableTests[ArraySeq].foldable[Int, Int])
  }
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
  checkLaws[Float]()
  checkLaws[Double]()
  checkLaws[Boolean]()
  checkLaws[Char]()
  checkLaws[String]()
  scope {
    implicit val intMonoid = AdditiveMonoid[Int].additive
    checkAll(s"FoldableTests[ArraySet[T]]", FoldableTests[ArraySet].foldable[Int, Int])
  }
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
  checkHashing[Float]()
  checkHashing[Double]()
  checkHashing[Boolean]()
  checkHashing[Char]()
  checkHashing[String]()
}