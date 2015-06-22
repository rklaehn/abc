package com.rklaehn.abc

import language.implicitConversions
import scala.util.hashing.Hashing
import scala.{ specialized => sp }
import spire.algebra.{Eq, Order}
import spire.implicits._

import scala.reflect.ClassTag

final class ArraySet[@sp(Int, Long, Double) T] private[abc] (private[abc] val elements: Array[T])(implicit tArrayTag: OrderedArrayTag[T]) { self ⇒
  import tArrayTag._

  def asSet: Set[T] = new Set[T] {

    def contains(elem: T) = self.apply(elem)

    def +(elem: T) = self.union(ArraySet.singleton(elem)).asSet

    def -(elem: T) = self.diff(ArraySet.singleton(elem)).asSet

    def iterator = elements.iterator
  }

  def asArraySeq: ArraySeq[T] =
    new ArraySeq[T](elements)(tArrayTag)

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

  def filter(p: T => Boolean): ArraySet[T] =
    new ArraySet[T](this.elements.filter(p))

  def xor(that: ArraySet[T]): ArraySet[T] =
    new ArraySet[T](SetUtils.xor(this.elements, that.elements))

  def isEmpty: Boolean = elements.isEmpty

  override def equals(that: Any) = that match {
    case that: ArraySet[T] => this.elements === that.elements
    case _ => false
  }

  override def hashCode: Int = ArrayHashing.arrayHashCode(elements)

  override def toString: String = elements.mkString("Set(", ",", ")")
}

object ArraySet {

  implicit def eqv[T]: Eq[ArraySet[T]] =
    spire.optional.genericEq.generic[ArraySet[T]]

  def empty[@sp(Int, Long, Double) T: OrderedArrayTag]: ArraySet[T] =
    new ArraySet[T](implicitly[ArrayTag[T]].empty)

  def apply[@sp(Int, Long, Double) T: OrderedArrayTag](elements: T*): ArraySet[T] = {
    val reducer = Reducer.create[ArraySet[T]](_ union _)
    for (e <- elements)
      reducer(singleton(e))
    reducer.result().getOrElse(empty)
  }

  def singleton[@sp(Int, Long, Double) T: OrderedArrayTag](e: T): ArraySet[T] =
    new ArraySet[T](implicitly[ArrayTag[T]].singleton(e))
}
