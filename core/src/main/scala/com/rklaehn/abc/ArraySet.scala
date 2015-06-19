package com.rklaehn.abc

import language.implicitConversions
import scala.util.hashing.Hashing
import scala.{ specialized => sp }
import spire.algebra.Order
import spire.implicits._

import scala.reflect.ClassTag

final class ArraySet[@sp(Int, Long, Double, AnyRef) T] private[abc] (private[abc] val elements: Array[T])(implicit f: ArraySet.Family[T]) { self ⇒
  import f._

  def asSet: Set[T] = new Set[T] {

    def contains(elem: T) = self.apply(elem)

    def +(elem: T) = self.union(ArraySet.singleton(elem)).asSet

    def -(elem: T) = self.diff(ArraySet.singleton(elem)).asSet

    def iterator = elements.iterator
  }

  def asArraySeq: ArraySeq[T] =
    new ArraySeq[T](elements)(f.tSeqFamily)

  def apply(e: T): Boolean =
    SetUtils.contains(elements, e)

  def subsetOf(that: ArraySet[T]): Boolean =
    SetUtils.subsetOf(that.elements, this.elements)

  def intersects(that: ArraySet[T]): Boolean =
    SetUtils.intersects(this.elements, that.elements)

  def union(that: ArraySet[T]): ArraySet[T] =
    new ArraySet[T](SetUtils.union(this.elements, that.elements))

  def intersection(that: ArraySet[T]): ArraySet[T] =
    new ArraySet[T](SetUtils.intersection(this.elements, that.elements))

  def diff(that: ArraySet[T]): ArraySet[T] =
    new ArraySet[T](SetUtils.diff(this.elements, that.elements))

  def xor(that: ArraySet[T]): ArraySet[T] =
    new ArraySet[T](SetUtils.xor(this.elements, that.elements))

  override def equals(that: Any) = that match {
    case that: ArraySet[T] => this.elements === that.elements
    case _ => false
  }

  override def hashCode: Int = ArrayHashing.arrayHashCode(elements)

  override def toString: String = elements.mkString("Set(", ",", ")")
}

object ArraySet {

  trait Family[@sp(Int, Long, Double, AnyRef) T] {

    def empty: ArraySet[T]

    implicit def tOrder: Order[T]

    implicit def tHashing: Hashing[T]

    def tSeqFamily: ArraySeq.Family[T]
  }

  implicit def genericFamily[@sp(Int, Long, Double, AnyRef) T: Order: ClassTag: Hashing] = new GenericFamily(Array.empty[T])

  private[abc] class GenericFamily[@sp(Int, Long, Double, AnyRef) T](ea: Array[T])(implicit val tOrder: Order[T], val tHashing: Hashing[T]) extends Family[T] {

    val empty = new ArraySet[T](ea)(this)

    val tSeqFamily = new ArraySeq.GenericFamily[T](empty.elements)
  }

  def empty[@sp(Int, Long, Double, AnyRef) T: Family]: ArraySet[T] = implicitly[Family[T]].empty

  def apply[@sp(Int, Long, Double, AnyRef) T](elements: T*)(implicit f: Family[T]): ArraySet[T] = {
    import f._
    val reducer = Reducer.create[ArraySet[T]](_ union _)
    for (e <- elements)
      reducer(singleton(e))
    reducer.result().getOrElse(f.empty)
  }

  def singleton[@sp(Int, Long, Double, AnyRef) T: Family](e: T): ArraySet[T] = new ArraySet[T](singletonArray(e))
}
