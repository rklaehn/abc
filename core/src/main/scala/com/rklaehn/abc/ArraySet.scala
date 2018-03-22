package com.rklaehn.abc

import algebra.ring.Semiring
import algebra.{PartialOrder, Order, Eq}
import cats.{Eval, Foldable, Show}
import cats.syntax.show._

import cats.kernel.instances.MapInstances

final class ArraySet[@sp(ILD) T] private[abc] (private[abc] val elements: Array[T]) { self ⇒

  def asNegatable: NegatableArraySet[T] = new NegatableArraySet[T](elements, false)

  def size: Int = elements.length

  def contains(elem: T)(implicit T: Order[T]) = self.apply(elem)

  def +(elem: T)(implicit T: Order[T]) = self.union(ArraySet.singleton0(elem, elements))

  def -(elem: T)(implicit T: Order[T]) = self.diff(ArraySet.singleton0(elem, elements))

  def iterator = elements.iterator

  def asArraySeq: ArraySeq[T] =
    new ArraySeq[T](elements)

  def apply(e: T)(implicit T: Order[T]): Boolean =
    Searching.search(elements, 0, elements.length, e) >= 0

  def subsetOf(that: ArraySet[T])(implicit T: Order[T]): Boolean =
    SetUtils.subsetOf(this.elements, that.elements)

  def intersects(that: ArraySet[T])(implicit T: Order[T]): Boolean =
    SetUtils.intersects(this.elements, that.elements)

  def union(that: ArraySet[T])(implicit T: Order[T]): ArraySet[T] =
    new ArraySet[T](SetUtils.union(this.elements, that.elements))

  def intersect(that: ArraySet[T])(implicit T: Order[T]): ArraySet[T] =
    new ArraySet[T](SetUtils.intersection(this.elements, that.elements))

  def diff(that: ArraySet[T])(implicit T: Order[T]): ArraySet[T] =
    new ArraySet[T](SetUtils.diff(this.elements, that.elements))

  def xor(that: ArraySet[T])(implicit T: Order[T]): ArraySet[T] =
    new ArraySet[T](SetUtils.xor(this.elements, that.elements))

  def filter(p: T => Boolean): ArraySet[T] =
    new ArraySet[T](ArrayUtil.filter(elements, p))

  def isEmpty: Boolean = elements.isEmpty

  override def equals(that: Any): Boolean = that match {
    case that: ArraySet[T] => ArraySet.eqv(Universal[T]).eqv(this, that)
    case _ => false
  }

  override def hashCode(): Int = ArraySet.hash(Universal[T]).hash(this)

  override def toString: String = ArraySet.show(Universal[T]).show(this)
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

  private def singleton0[@sp(ILD) T](e: T, a: Array[T]): ArraySet[T] =
    new ArraySet[T](Array.singleton(e, a))

  implicit val foldable: Foldable[ArraySet] = new Foldable[ArraySet] {
    def foldLeft[A, B](fa: ArraySet[A], b: B)(f: (B, A) ⇒ B): B = fa.elements.foldLeft[B](b)(f)
    def foldRight[A, B](fa: ArraySet[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]) = Foldable.iterateRight(fa.elements, lb)(f)
  }

  implicit def show[A: Show]: Show[ArraySet[A]] = Show.show(_.elements.map(_.show).mkString("ArraySet(", ",", ")"))

  implicit def hash[A: Hash]: Hash[ArraySet[A]] = Hash.by(_.elements)

  implicit def semiring[A: Order: ClassTag]: Semiring[ArraySet[A]] = new Semiring[ArraySet[A]] {
    def zero = ArraySet.empty[A]
    def times(x: ArraySet[A], y: ArraySet[A]) = x intersect y
    def plus(x: ArraySet[A], y: ArraySet[A]) = x union y
  }

  def empty[@sp(ILD) T: ClassTag]: ArraySet[T] =
    new ArraySet[T](Array.empty[T])

  def singleton[@sp(ILD) T: ClassTag](e: T): ArraySet[T] =
    new ArraySet[T](Array.singleton(e))

  def apply[@sp(ILD) T: Order : ClassTag](elements: T*): ArraySet[T] = {
    val t = new Array[T](elements.length)
    // we must not use toArray, because somebody might have passed an array, and toArray would return that array (*not* a copy!)
    elements.copyToArray(t)
    new ArraySet[T](sortAndRemoveDuplicatesInPlace(t))
  }
}
