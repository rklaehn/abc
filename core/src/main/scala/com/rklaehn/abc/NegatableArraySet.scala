package com.rklaehn.abc

import com.rklaehn.sonicreducer.Reducer

import language.implicitConversions
import scala.reflect.ClassTag
import scala.{ specialized => sp }
import algebra.{Order, Eq}
import algebra.lattice.Bool

final class NegatableArraySet[@sp(Int, Long, Double) T] private[abc] (private[abc] val elements: Array[T], private[abc] val negated: Boolean) extends NoEquals {
  lhs ⇒
  import NegatableArraySet._

  def elementsAsArraySeq: ArraySeq[T] =
    new ArraySeq[T](elements)

  def isEmpty: Boolean = !negated && elements.isEmpty

  def negate: NegatableArraySet[T] = wrap(elements, !negated)

  def apply(elem: T)(implicit order: Order[T]) = negated ^ (Searching.search(elements, 0, elements.length, elem) >= 0)

  def +(elem: T)(implicit order: Order[T], classTag: ClassTag[T]) = lhs.union(singleton(elem))

  def -(elem: T)(implicit order: Order[T], classTag: ClassTag[T]) = lhs.diff(singleton(elem))

  def subsetOf(rhs: NegatableArraySet[T])(implicit order: Order[T], classTag: ClassTag[T]): Boolean =
    (lhs.negated, rhs.negated) match {
      case (false, false) ⇒ SetUtils.subsetOf(lhs.elements, rhs.elements)
      case (false, true)  ⇒ !SetUtils.intersects(lhs.elements, rhs.elements)
      case (true, false)  ⇒ false
      case (true, true)   ⇒ SetUtils.subsetOf(rhs.elements, lhs.elements)
    }

  def intersects(rhs: NegatableArraySet[T])(implicit order: Order[T], classTag: ClassTag[T]): Boolean = {
    (lhs.negated, rhs.negated) match {
      case (false, false) ⇒ SetUtils.intersects(lhs.elements, rhs.elements)
      case (false, true)  ⇒ !SetUtils.subsetOf(lhs.elements, rhs.elements)
      case (true, false)  ⇒ !SetUtils.subsetOf(rhs.elements, lhs.elements)
      case (true, true)   ⇒ true
    }
  }

  def xor(rhs: NegatableArraySet[T])(implicit order: Order[T], classTag: ClassTag[T]): NegatableArraySet[T] =
    wrap(SetUtils.xor(lhs.elements, rhs.elements), lhs.negated ^ rhs.negated)

  def union(rhs: NegatableArraySet[T])(implicit order: Order[T], classTag: ClassTag[T]): NegatableArraySet[T] = {
    (lhs.negated, rhs.negated) match {
      case (false, false) ⇒ wrap(SetUtils.union(lhs.elements, rhs.elements), false)
      case (false, true)  ⇒ wrap(SetUtils.diff(rhs.elements, lhs.elements), true)
      case (true, false)  ⇒ wrap(SetUtils.diff(lhs.elements, rhs.elements), true)
      case (true, true)   ⇒ wrap(SetUtils.intersection(lhs.elements, rhs.elements), true)
    }
  }

  def intersect(rhs: NegatableArraySet[T])(implicit order: Order[T], classTag: ClassTag[T]): NegatableArraySet[T] = {
    (lhs.negated, rhs.negated) match {
      case (false, false) ⇒ wrap(SetUtils.intersection(lhs.elements, rhs.elements), false)
      case (false, true)  ⇒ wrap(SetUtils.diff(lhs.elements, rhs.elements), false)
      case (true, false)  ⇒ wrap(SetUtils.diff(rhs.elements, lhs.elements), false)
      case (true, true)   ⇒ wrap(SetUtils.union(lhs.elements, rhs.elements), true)
    }
  }

  def diff(rhs: NegatableArraySet[T])(implicit order: Order[T], classTag: ClassTag[T]): NegatableArraySet[T] = {
    (lhs.negated, rhs.negated) match {
      case (false, false) ⇒ wrap(SetUtils.diff(lhs.elements, rhs.elements), false)
      case (false, true)  ⇒ wrap(SetUtils.intersection(lhs.elements, rhs.elements), false)
      case (true, false)  ⇒ wrap(SetUtils.union(lhs.elements, rhs.elements), true)
      case (true, true)   ⇒ wrap(SetUtils.diff(rhs.elements, lhs.elements), false)
    }
  }

  override def toString: String =
    if(negated) {
      if (elements.isEmpty) "All"
      else elements.mkString("Except(", ",", ")")
    }
    else {
      if(elements.isEmpty) "Empty"
      else elements.mkString("Set(", ",", ")")
    }
}

object NegatableArraySet {

  implicit def negatableArraySetEq[T: Eq]: Eq[NegatableArraySet[T]] = new Eq[NegatableArraySet[T]] {
    def eqv(x: NegatableArraySet[T], y: NegatableArraySet[T]) = x.negated == y.negated && Eq.eqv(x.elements, y.elements)
  }

  implicit def negatableArraySetBool[T : Order: ClassTag]: Bool[NegatableArraySet[T]] = new Bool[NegatableArraySet[T]] {

    def complement(a: NegatableArraySet[T]) = a.negate

    def or(a: NegatableArraySet[T], b: NegatableArraySet[T]) = a union b

    def and(a: NegatableArraySet[T], b: NegatableArraySet[T]) = a intersect b

    override def xor(a: NegatableArraySet[T], b: NegatableArraySet[T]) = a xor b

    def zero = NegatableArraySet.empty[T]

    def one = NegatableArraySet.all[T]
  }

  // $COVERAGE-OFF$
  private[this] class NegatableArraySetBuilder[@sp(Int, Long, Double) T](implicit order: Order[T], classTag: ClassTag[T]) extends scala.collection.mutable.Builder[T, NegatableArraySet[T]] {

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
      reducer.result().map(x ⇒ wrap(x, false)).getOrElse(empty)
    }
  }
  // $COVERAGE-ON$

  private[abc] def wrap[T](elements: Array[T], negated: Boolean) = new NegatableArraySet[T](elements, negated)

  def fromBoolean[@sp(Int, Long, Double) T: ClassTag](value: Boolean): NegatableArraySet[T] =
    if(value) all else empty

  def empty[@sp(Int, Long, Double) T: ClassTag]: NegatableArraySet[T] =
    wrap(Array.empty[T], false)

  def all[@sp(Int, Long, Double) T: ClassTag]: NegatableArraySet[T] =
    wrap(Array.empty[T], true)

  def singleton[@sp(Int, Long, Double) T: ClassTag](e: T): NegatableArraySet[T] =
    wrap(Array.singleton(e), false)

  def apply[@sp(Int, Long, Double) T: Order: ClassTag](elements: T*): NegatableArraySet[T] = {
    val b = new NegatableArraySetBuilder[T]
    b ++= elements
    b.result()
  }
}
