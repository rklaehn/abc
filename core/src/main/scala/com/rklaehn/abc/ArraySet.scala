package com.rklaehn.abc

import language.implicitConversions
import scala.collection.{mutable, SetLike}
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.util.hashing.Hashing
import scala.{ specialized => sp }
import spire.algebra.{Eq, Order}
import spire.implicits._

import scala.reflect.ClassTag

final class ArraySet[@sp(Int, Long, Double) T] private[abc] (private[abc] val elements: Array[T])(implicit tArrayTag: OrderedArrayTag[T])
  extends Set[T] with SetLike[T, ArraySet[T]]
{ self ⇒
  import tArrayTag._

  def contains(elem: T) = self.apply(elem)

  def +(elem: T) = self.union(ArraySet.singleton(elem))

  def -(elem: T) = self.diff(ArraySet.singleton(elem))

  def iterator = elements.iterator

  override def empty = ArraySet.empty[T]

  def asArraySeq: ArraySeq[T] =
    new ArraySeq[T](elements)(tArrayTag)

  override def apply(e: T): Boolean =
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

  override def filter(p: T => Boolean): ArraySet[T] =
    new ArraySet[T](this.elements.filter(p))

  def xor(that: ArraySet[T]): ArraySet[T] =
    new ArraySet[T](SetUtils.xor(this.elements, that.elements))

  override def isEmpty: Boolean = elements.isEmpty

  override def equals(that: Any) = that match {
    case that: ArraySet[T] => this.elements === that.elements
    case _ => false
  }

  override def hashCode: Int = ArrayHashing.arrayHashCode(elements)

  override def toString: String = elements.mkString("Set(", ",", ")")
}

object ArraySet {

  implicit def cbf[CC, @sp(Int, Long, Double) U: OrderedArrayTag]: CanBuildFrom[CC, U, ArraySet[U]] = new CanBuildFrom[CC, U, ArraySet[U]] {
    def apply(from: CC) = apply()

    def apply(): mutable.Builder[U, ArraySet[U]] = new ArraySetBuilder[U]
  }

  private[this] class ArraySetBuilder[@sp(Int, Long, Double) T](implicit tag: OrderedArrayTag[T]) extends scala.collection.mutable.Builder[T, ArraySet[T]] {

    private[this] def union(a: Array[T], b: Array[T]) = {
      SetUtils.union(a, b)(tag.order)
    }

    private[this] var reducer = Reducer.create[Array[T]](union)

    def +=(elem: T) = {
      reducer.apply(tag.singleton(elem))
      this
    }

    def clear() = {
      reducer = Reducer.create[Array[T]](union)
    }

    def result() = {
      reducer.result.map(x ⇒ new ArraySet(x)).getOrElse(empty)
    }
  }

  implicit def eqv[T]: Eq[ArraySet[T]] =
    spire.optional.genericEq.generic[ArraySet[T]]

  def empty[@sp(Int, Long, Double) T: OrderedArrayTag]: ArraySet[T] =
    new ArraySet[T](implicitly[ArrayTag[T]].empty)

  def singleton[@sp(Int, Long, Double) T: OrderedArrayTag](e: T): ArraySet[T] =
    new ArraySet[T](implicitly[ArrayTag[T]].singleton(e))

  def apply[@sp(Int, Long, Double) T: OrderedArrayTag](elements: T*): ArraySet[T] = {
    val b = new ArraySetBuilder[T]
    b ++= elements
    b.result()
  }

}
