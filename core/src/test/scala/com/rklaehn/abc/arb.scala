package com.rklaehn.abc

import algebra.{Eq, Order}
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._

import scala.reflect.ClassTag

object arb {

  implicit def arbArraySeq[T: Arbitrary: ClassTag] = Arbitrary {
    for {
      xs ← arbitrary[IndexedSeq[T]]
    } yield
    ArraySeq(xs: _*)
  }

  implicit def arbArraySet[T: Arbitrary: Order: ClassTag] = Arbitrary {
    for {
      xs ← arbitrary[IndexedSeq[T]]
    } yield
    ArraySet(xs: _*)
  }

  implicit def arbArrayMap[K: Arbitrary: Order: ClassTag, V: Arbitrary: ClassTag] = Arbitrary {
    for {
      xs ← arbitrary[IndexedSeq[(K, V)]]
    } yield
    ArrayMap(xs: _*)
  }

  implicit def arbArrayTotalMap[K: Arbitrary: Order: ClassTag, V: Arbitrary: ClassTag: Eq] = Arbitrary {
    for {
      m ← arbitrary[ArrayMap[K, V]]
      default ← arbitrary[V]
    } yield
      m.withDefaultValue(default)
  }

  implicit def arbArrayMultiMap[K: Arbitrary: Order: ClassTag, V: Arbitrary: Order: ClassTag] = Arbitrary {
    for {
      xs ← arbitrary[IndexedSeq[(K, ArraySet[V])]]
    } yield
    ArrayMultiMap(xs: _*)
  }

  implicit def arbNegatableArraySet[T: Arbitrary: Order: ClassTag] = Arbitrary {
    for {
      xs ← arbitrary[Vector[T]]
      n ← arbitrary[Boolean]
    } yield
      NegatableArraySet(xs :_*) xor NegatableArraySet.fromBoolean(n)
  }
}
