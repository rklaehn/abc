package com.rklaehn.abc

import algebra.{Eq, Order}
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._

object arb {

  implicit def arbArraySeq[T: Arbitrary: ClassTag] = Arbitrary {
    for {
      xs ← arbitrary[IndexedSeq[T]]
    } yield
      ArraySeq(xs: _*)
  }

  implicit def arbTotalArraySeq[K: Arbitrary: Eq: ClassTag] = Arbitrary {
    for {
      m ← arbitrary[ArraySeq[K]]
      default ← arbitrary[K]
    } yield
      m.withDefault(default)
  }

  implicit def arbArraySet[T: Arbitrary: Order: ClassTag] = Arbitrary {
    for {
      xs ← arbitrary[IndexedSeq[T]]
    } yield
      ArraySet(xs: _*)
  }

  implicit def arbNegatableArraySet[T: Arbitrary: Order: ClassTag] = Arbitrary {
    for {
      xs ← arbitrary[Vector[T]]
      n ← arbitrary[Boolean]
    } yield
      NegatableArraySet(xs :_*) xor NegatableArraySet.fromBoolean(n)
  }

  implicit def arbArrayMap[K: Arbitrary: Order: ClassTag, V: Arbitrary: ClassTag] = Arbitrary {
    for {
      xs ← arbitrary[IndexedSeq[(K, V)]]
    } yield
      ArrayMap(xs: _*)
  }

  implicit def arbTotalArrayMap[K: Arbitrary: Order: ClassTag, V: Arbitrary: ClassTag: Eq] = Arbitrary {
    for {
      m ← arbitrary[ArrayMap[K, V]]
      default ← arbitrary[V]
    } yield
      m.withDefault(default)
  }

//
//  implicit def arbArrayMultiMap[K: Arbitrary: Order: ClassTag, V: Arbitrary: Order: ClassTag] = Arbitrary {
//    for {
//      xs ← arbitrary[IndexedSeq[(K, ArraySet[V])]]
//    } yield
//      ArrayMultiMap(xs: _*)
//  }
}

