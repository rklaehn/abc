package com.rklaehn.abc

import language.implicitConversions
import scala.collection.immutable.SortedSet
import scala.collection.{GenSet, SortedSetLike, mutable}
import scala.collection.generic.CanBuildFrom
import scala.{ specialized => sp }
import spire.algebra.{Eq, Order}
import spire.implicits._

import scala.reflect.ClassTag

final class ArraySet[@sp(Int, Long, Double) T] private[abc] (private[abc] val elements: Array[T])(implicit tArrayTag: OrderedArrayTag[T])
  extends SortedSet[T] with SortedSetLike[T, ArraySet[T]]
{ self ⇒
  import tArrayTag._

  implicit def ordering = Order.ordering(order)

  def rangeImpl(from: Option[T], until: Option[T]) = ???

  def keysIteratorFrom(start: T) = {
    val index = tArrayTag.binarySearch(elements, 0, elements.length, start)
    if(index >= 0)
      iterator.drop(index)
    else
      iterator.drop(-index -1)
  }

  def contains(elem: T) = self.apply(elem)

  def +(elem: T) = self.union(ArraySet.singleton(elem))

  def -(elem: T) = self.diff(ArraySet.singleton(elem))

  def iterator = elements.iterator

  override def empty = ArraySet.empty[T]

  def asArraySeq: ArraySeq[T] =
    new ArraySeq[T](elements)(tArrayTag)

  override def apply(e: T): Boolean =
    tArrayTag.binarySearch(elements, 0, elements.length, e) >= 0

  def subsetOf(that: ArraySet[T]): Boolean =
    SetUtils.subsetOf(this.elements, that.elements)

  override def union(that: GenSet[T]) = that match {
    case that: ArraySet[T] ⇒ this.union(that)
    case _ ⇒ super.union(that)
  }

  override def diff(that: GenSet[T]) = that match {
    case that: ArraySet[T] ⇒ this.diff(that)
    case _ ⇒ super.diff(that)
  }

  override def intersect(that: GenSet[T]) = that match {
    case that: ArraySet[T] ⇒ this.intersect(that)
    case _ ⇒ super.intersect(that)
  }

  override def subsetOf(that: GenSet[T]) = that match {
    case that: ArraySet[T] ⇒ this.subsetOf(that)
    case _ ⇒ super.subsetOf(that)
  }

  def intersects(that: ArraySet[T]): Boolean =
    SetUtils.intersects(this.elements, that.elements)

  def union(that: ArraySet[T]): ArraySet[T] =
    new ArraySet[T](SetUtils.union(this.elements, that.elements))

  def intersect(that: ArraySet[T]): ArraySet[T] =
    new ArraySet[T](SetUtils.intersection(this.elements, that.elements))

  def diff(that: ArraySet[T]): ArraySet[T] =
    new ArraySet[T](SetUtils.diff(this.elements, that.elements))

  override def filter(p: T => Boolean): ArraySet[T] =
    new ArraySet[T](this.elements.filter(p))

  def xor(that: ArraySet[T]): ArraySet[T] =
    new ArraySet[T](SetUtils.xor(this.elements, that.elements))

  override def isEmpty: Boolean = elements.isEmpty

  override def equals(that: Any) = that match {
    case that: ArraySet[T] => tArrayTag.eqv(this.elements, that.elements)
    case _ => false
  }

  override def hashCode: Int = tArrayTag.hash(this.elements)

  override def toString: String = elements.mkString("Set(", ",", ")")
}

object ArraySet {

  implicit def cbf[CC, @sp(Int, Long, Double) U: OrderedArrayTag]: CanBuildFrom[CC, U, ArraySet[U]] = new CanBuildFrom[CC, U, ArraySet[U]] {
    def apply(from: CC) = apply()

    def apply(): mutable.Builder[U, ArraySet[U]] = new ArraySetBuilder[U]
  }

  private[this] class ArraySetBuilder[@sp(Int, Long, Double) T](implicit tag: OrderedArrayTag[T]) extends scala.collection.mutable.Builder[T, ArraySet[T]] {

    private[this] def union(a: Array[T], b: Array[T]) = {
      SetUtils.union(a, b)
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
    new ArraySet[T](ArrayTag[T].empty)

  def singleton[@sp(Int, Long, Double) T: OrderedArrayTag](e: T): ArraySet[T] =
    new ArraySet[T](ArrayTag[T].singleton(e))

  def apply[@sp(Int, Long, Double) T: OrderedArrayTag](elements: T*): ArraySet[T] = {
    val b = new ArraySetBuilder[T]
    b ++= elements
    b.result()
  }

}
