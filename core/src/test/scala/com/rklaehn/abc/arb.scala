package com.rklaehn.abc

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

  implicit def arbArraySet[T: Arbitrary: OrderedArrayTag] = Arbitrary {
    for {
      xs ← arbitrary[IndexedSeq[T]]
    } yield
    ArraySet(xs: _*)
  }

  implicit def arbArrayMap[K: Arbitrary: OrderedArrayTag, V: Arbitrary: ArrayTag] = Arbitrary {
    for {
      xs ← arbitrary[IndexedSeq[(K, V)]]
    } yield
    ArrayMap(xs: _*)
  }

  implicit def arbArrayMultiMap[K: Arbitrary: OrderedArrayTag, V: Arbitrary: OrderedArrayTag] = Arbitrary {
    for {
      xs ← arbitrary[IndexedSeq[(K, ArraySet[V])]]
    } yield
    ArrayMultiMap(xs: _*)
  }

  implicit def arbNegatableArraySet[T: Arbitrary: OrderedArrayTag] = Arbitrary {
    for {
      xs ← arbitrary[Vector[T]]
      n ← arbitrary[Boolean]
    } yield
      NegatableArraySet(xs :_*) xor NegatableArraySet.fromBoolean(n)
  }
}
