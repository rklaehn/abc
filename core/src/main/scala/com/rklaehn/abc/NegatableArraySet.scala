package com.rklaehn.abc

import cats.Show

import language.implicitConversions
import scala.reflect.ClassTag
import scala.{ specialized => sp }
import algebra.{PartialOrder, Order, Eq}
import algebra.lattice.Bool
import cats.implicits._

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
}

private[abc] trait NegatableArraySet0 {
  implicit def eqv[T: Eq]: Eq[NegatableArraySet[T]] = new Eq[NegatableArraySet[T]] {
    def eqv(x: NegatableArraySet[T], y: NegatableArraySet[T]) = x.negated == y.negated && Eq.eqv(x.elements, y.elements)
  }
}

object NegatableArraySet extends NegatableArraySet0 {

  implicit def show[T: Show]: Show[NegatableArraySet[T]] = Show.show { s ⇒
    if(s.negated) {
      if (s.elements.isEmpty) "All"
      else s.elements.map(_.show).mkString("Except(", ",", ")")
    }
    else {
      if(s.elements.isEmpty) "Empty"
      else s.elements.map(_.show).mkString("Set(", ",", ")")
    }
  }

  implicit def partialOrder[T: Order: ClassTag]: PartialOrder[NegatableArraySet[T]] = PartialOrder.from { (x, y) ⇒
    (x subsetOf y, y subsetOf x) match {
      case (true, true)   ⇒ 0.0
      case (true, false)  ⇒ -1.0
      case (false, true)  ⇒ +1.0
      case (false, false) ⇒ Double.NaN
    }
  }

  implicit def bool[T : Order: ClassTag]: Bool[NegatableArraySet[T]] = new Bool[NegatableArraySet[T]] {
    def complement(a: NegatableArraySet[T]) = a.negate
    def or(a: NegatableArraySet[T], b: NegatableArraySet[T]) = a union b
    def and(a: NegatableArraySet[T], b: NegatableArraySet[T]) = a intersect b
    override def xor(a: NegatableArraySet[T], b: NegatableArraySet[T]) = a xor b
    def zero = NegatableArraySet.empty[T]
    def one = NegatableArraySet.all[T]
  }

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
    val t = new Array[T](elements.length)
    // we must not use toArray, because somebody might have passed an array, and toArray would return that array (*not* a copy!)
    elements.copyToArray(t)
    new NegatableArraySet[T](t.sortAndRemoveDuplicatesInPlace(), false)
  }
}
