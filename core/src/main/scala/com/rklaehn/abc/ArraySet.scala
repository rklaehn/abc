package com.rklaehn.abc

import com.rklaehn.abc.ArraySet.AsCollection

import language.implicitConversions
import scala.collection.{GenSet, SortedSetLike, mutable}
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.SortedSet
import scala.{ specialized => sp }
import spire.algebra.{Order, Eq}

final class ArraySet[@sp(Int, Long, Double) T] private[abc] (private[abc] val elements: Array[T]) { self ⇒

  def asCollection(implicit tArrayTag: OrderedArrayTag[T]): SortedSet[T] = AsCollection.wrap(this)

  def contains(elem: T)(implicit tArrayTag: OrderedArrayTag[T]) = self.apply(elem)

  def +(elem: T)(implicit tArrayTag: OrderedArrayTag[T]) = self.union(ArraySet.singleton(elem))

  def -(elem: T)(implicit tArrayTag: OrderedArrayTag[T]) = self.diff(ArraySet.singleton(elem))

  def iterator = elements.iterator

  def asArraySeq: ArraySeq[T] =
    new ArraySeq[T](elements)

  def apply(e: T)(implicit tArrayTag: OrderedArrayTag[T]): Boolean =
    tArrayTag.binarySearch(elements, 0, elements.length, e) >= 0

  def subsetOf(that: ArraySet[T])(implicit tArrayTag: OrderedArrayTag[T]): Boolean =
    SetUtils.subsetOf(this.elements, that.elements)

  def intersects(that: ArraySet[T])(implicit tArrayTag: OrderedArrayTag[T]): Boolean =
    SetUtils.intersects(this.elements, that.elements)

  def union(that: ArraySet[T])(implicit tArrayTag: OrderedArrayTag[T]): ArraySet[T] =
    new ArraySet[T](SetUtils.union(this.elements, that.elements))

  def intersect(that: ArraySet[T])(implicit tArrayTag: OrderedArrayTag[T]): ArraySet[T] =
    new ArraySet[T](SetUtils.intersection(this.elements, that.elements))

  def diff(that: ArraySet[T])(implicit tArrayTag: OrderedArrayTag[T]): ArraySet[T] =
    new ArraySet[T](SetUtils.diff(this.elements, that.elements))

  def filter(p: T => Boolean): ArraySet[T] =
    new ArraySet[T](this.elements.filter(p))

  def xor(that: ArraySet[T])(implicit tArrayTag: OrderedArrayTag[T]): ArraySet[T] =
    new ArraySet[T](SetUtils.xor(this.elements, that.elements))

  def isEmpty: Boolean = elements.isEmpty

  override def toString: String = elements.mkString("Set(", ",", ")")
}

object ArraySet {

  private final class AsCollection[T](val underlying: ArraySet[T])(implicit tArrayTag: OrderedArrayTag[T]) extends SortedSet[T] with SortedSetLike[T, AsCollection[T]] {
    import AsCollection.wrap
    implicit def ordering = Order.ordering(tArrayTag.order)

    def +(elem: T) = wrap(underlying + elem)

    def -(elem: T) = wrap(underlying - elem)

    def contains(elem: T) = underlying contains elem

    def iterator = underlying.iterator

    def rangeImpl(from: Option[T], until: Option[T]) = ???

    def keysIteratorFrom(start: T) = ???

    override def union(that: GenSet[T]) = that match {
      case that: AsCollection[T] ⇒ wrap(underlying union that.underlying)
      case _ ⇒ super.union(that)
    }

    override def diff(that: GenSet[T]) = that match {
      case that: AsCollection[T] ⇒ wrap(underlying diff that.underlying)
      case _ ⇒ super.diff(that)
    }

    override def intersect(that: GenSet[T]) = that match {
      case that: AsCollection[T] ⇒ wrap(underlying intersect that.underlying)
      case _ ⇒ super.intersect(that)
    }

    override def subsetOf(that: GenSet[T]) = that match {
      case that: AsCollection[T] ⇒ underlying subsetOf that.underlying
      case _ ⇒ super.subsetOf(that)
    }

    override def filter(p: T => Boolean) = new AsCollection(underlying.filter(p))

    override def isEmpty: Boolean = underlying.isEmpty

    override def equals(that: Any) = that match {
      case that: AsCollection[T] => tArrayTag.eqv(underlying.elements, that.underlying.elements)
      case _ => false
    }

    override def toString = underlying.toString

    override def hashCode: Int = tArrayTag.hash(underlying.elements)

    override def apply(e: T): Boolean = underlying.apply(e)

    override def empty = new AsCollection(ArraySet.empty[T])
  }

  private object AsCollection {

    def wrap[U: OrderedArrayTag](underlying: ArraySet[U]) = new AsCollection[U](underlying)

    implicit def cbf[CC, @sp(Int, Long, Double) U: OrderedArrayTag]: CanBuildFrom[CC, U, ArraySet[U]] = new CanBuildFrom[CC, U, ArraySet[U]] {
      def apply(from: CC) = apply()

      def apply(): mutable.Builder[U, ArraySet[U]] = new ArraySetBuilder[U]
    }
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
      reducer.result().map(x ⇒ new ArraySet(x)).getOrElse(empty)
    }
  }

  implicit def eqv[T](implicit tArrayTag: OrderedArrayTag[T]): Eq[ArraySet[T]] =
    tArrayTag.on(_.elements)

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
