package com.rklaehn.abc

import algebra.ring.Semiring
import cats.Show
import cats.syntax.show._
import com.rklaehn.sonicreducer.Reducer

import language.implicitConversions
import scala.collection.{GenSet, SortedSetLike, mutable}
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.SortedSet
import scala.reflect.ClassTag
import scala.{ specialized => sp }
import algebra.{PartialOrder, Order, Eq}

final class ArraySet[@sp(Int, Long, Double) T] private[abc] (private[abc] val elements: Array[T]) extends NoEquals { self ⇒

  def size: Int = elements.length

  // $COVERAGE-OFF$
  def asCollection(implicit order: Order[T], classTag: ClassTag[T], hash: Hash[T]): ArraySet.AsCollection[T] = ArraySet.AsCollection.wrap(this)
  // $COVERAGE-ON$

  def contains(elem: T)(implicit order: Order[T]) = self.apply(elem)

  def +(elem: T)(implicit order: Order[T], classTag: ClassTag[T]) = self.union(ArraySet.singleton(elem))

  def -(elem: T)(implicit order: Order[T], classTag: ClassTag[T]) = self.diff(ArraySet.singleton(elem))

  def iterator = elements.iterator

  def asArraySeq: ArraySeq[T] =
    new ArraySeq[T](elements)

  def apply(e: T)(implicit order: Order[T]): Boolean =
    Searching.search(elements, 0, elements.length, e) >= 0

  def subsetOf(that: ArraySet[T])(implicit order: Order[T]): Boolean =
    SetUtils.subsetOf(this.elements, that.elements)

  def intersects(that: ArraySet[T])(implicit order: Order[T]): Boolean =
    SetUtils.intersects(this.elements, that.elements)

  def union(that: ArraySet[T])(implicit order: Order[T], classTag: ClassTag[T]): ArraySet[T] =
    new ArraySet[T](SetUtils.union(this.elements, that.elements))

  def intersect(that: ArraySet[T])(implicit order: Order[T], classTag: ClassTag[T]): ArraySet[T] =
    new ArraySet[T](SetUtils.intersection(this.elements, that.elements))

  def diff(that: ArraySet[T])(implicit order: Order[T], classTag: ClassTag[T]): ArraySet[T] =
    new ArraySet[T](SetUtils.diff(this.elements, that.elements))

  def filter(p: T => Boolean): ArraySet[T] =
    new ArraySet[T](this.elements.filter(p))

  def xor(that: ArraySet[T])(implicit order: Order[T], classTag: ClassTag[T]): ArraySet[T] =
    new ArraySet[T](SetUtils.xor(this.elements, that.elements))

  def isEmpty: Boolean = elements.isEmpty

  override def toString: String = elements.mkString("Set(", ",", ")")
}

private[abc] trait ArraySet0 {

  implicit def eqv[A: Eq]: Eq[ArraySet[A]] = Eq.by(_.elements)
}

private[abc] trait ArraySet1 extends ArraySet0 {
  
  implicit def partialOrder[A: Order]: PartialOrder[ArraySet[A]] = new PartialOrder[ArraySet[A]] {
    def partialCompare(x: ArraySet[A], y: ArraySet[A]) : Double =
      if (x.size < y.size) if (x.subsetOf(y)) -1.0 else Double.NaN
      else if (y.size < x.size) -partialCompare(y, x)
      else if (eqv(x, y)) 0.0
      else Double.NaN
    override def eqv(x: ArraySet[A], y: ArraySet[A]) = Eq.eqv(x.elements, y.elements)
  }
}

object ArraySet extends ArraySet1 {

  implicit def show[A: Show]: Show[ArraySet[A]] = Show.show(_.elements.map(_.show).mkString("ArraySet(", ",", ")"))

  implicit def hash[A: Hash]: Hash[ArraySet[A]] = Hash.by(_.elements)

  implicit def semiring[A: Order: ClassTag]: Semiring[ArraySet[A]] = new Semiring[ArraySet[A]] {
    def zero = ArraySet.empty[A]
    def times(x: ArraySet[A], y: ArraySet[A]) = x intersect y
    def plus(x: ArraySet[A], y: ArraySet[A]) = x union y
  }

  // $COVERAGE-OFF$
  final class AsCollection[T](val underlying: ArraySet[T])(implicit tOrder: Order[T], tClassTag: ClassTag[T], tHash: Hash[T]) extends SortedSet[T] with SortedSetLike[T, AsCollection[T]] {
    import AsCollection.wrap
    implicit def ordering = Order.ordering(tOrder)

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
      case that: AsCollection[T] => Eq.eqv(underlying.elements, that.underlying.elements)
      case _ => false
    }

    override def toString = underlying.toString

    override def hashCode: Int = Hash.hash(underlying.elements)

    override def apply(e: T): Boolean = underlying.apply(e)

    override def empty = new AsCollection(ArraySet.empty[T])
  }

  object AsCollection {

    private[abc] def wrap[U: Order: ClassTag: Hash](underlying: ArraySet[U]) = new AsCollection[U](underlying)

    implicit def cbf[CC, U: Order: ClassTag: Hash]: CanBuildFrom[CC, U, AsCollection[U]] = new CanBuildFrom[CC, U, AsCollection[U]] {
      def apply(from: CC) = apply()

      def apply(): mutable.Builder[U, AsCollection[U]] = new ArraySetBuilder[U].mapResult(x ⇒ wrap(x))
    }
  }

  private[this] class ArraySetBuilder[@sp(Int, Long, Double) T](implicit order: Order[T], classTag: ClassTag[T]) extends scala.collection.mutable.Builder[T, ArraySet[T]] {

    private[this] def union(a: Array[T], b: Array[T]) = {
      SetUtils.union(a, b)
    }

    private[this] var reducer = Reducer[Array[T]](union)

    def +=(elem: T) = {
      reducer.apply(Array.singleton(elem))
      this
    }

    def clear() = {
      reducer = Reducer[Array[T]](union)
    }

    def result() = {
      reducer.result().map(x ⇒ new ArraySet(x)).getOrElse(empty)
    }
  }
  // $COVERAGE-ON$

  def empty[@sp(Int, Long, Double) T: ClassTag]: ArraySet[T] =
    new ArraySet[T](Array.empty[T])

  def singleton[@sp(Int, Long, Double) T: ClassTag](e: T): ArraySet[T] =
    new ArraySet[T](Array.singleton(e))

  def apply[@sp(Int, Long, Double) T: Order : ClassTag](elements: T*): ArraySet[T] = {
    val t = new Array[T](elements.length)
    // we must not use toArray, because somebody might have passed an array, and toArray would return that array (*not* a copy!)
    elements.copyToArray(t)
    new ArraySet[T](t.sortAndRemoveDuplicatesInPlace())
  }
}
