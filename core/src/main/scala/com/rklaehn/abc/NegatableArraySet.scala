package com.rklaehn.abc

import algebra.{PartialOrder, Order, Eq}
import algebra.lattice.Bool
import cats.Show
import cats.implicits._

final class NegatableArraySet[T] private[abc] (private[abc] val elements0: Array[T], private[abc] val negated: Boolean) {
  lhs ⇒
  import NegatableArraySet._

  def elements: ArraySet[T] = new ArraySet[T](elements0)

  def isEmpty: Boolean = !negated && elements0.isEmpty

  def negate: NegatableArraySet[T] = wrap(elements0, !negated)

  def apply(elem: T)(implicit order: Order[T]) = negated ^ (Searching.search(elements0, 0, elements0.length, elem) >= 0)

  def +(elem: T)(implicit order: Order[T]) = lhs.union(singleton(elem))

  def -(elem: T)(implicit order: Order[T]) = lhs.diff(singleton(elem))

  def subsetOf(rhs: NegatableArraySet[T])(implicit order: Order[T]): Boolean =
    (lhs.negated, rhs.negated) match {
      case (false, false) ⇒ SetUtils.subsetOf(lhs.elements0, rhs.elements0)
      case (false, true)  ⇒ !SetUtils.intersects(lhs.elements0, rhs.elements0)
      case (true, false)  ⇒ false
      case (true, true)   ⇒ SetUtils.subsetOf(rhs.elements0, lhs.elements0)
    }

  def intersects(rhs: NegatableArraySet[T])(implicit order: Order[T]): Boolean = {
    (lhs.negated, rhs.negated) match {
      case (false, false) ⇒ SetUtils.intersects(lhs.elements0, rhs.elements0)
      case (false, true)  ⇒ !SetUtils.subsetOf(lhs.elements0, rhs.elements0)
      case (true, false)  ⇒ !SetUtils.subsetOf(rhs.elements0, lhs.elements0)
      case (true, true)   ⇒ true
    }
  }

  def xor(rhs: NegatableArraySet[T])(implicit order: Order[T]): NegatableArraySet[T] =
    wrap(SetUtils.xor(lhs.elements0, rhs.elements0), lhs.negated ^ rhs.negated)

  def union(rhs: NegatableArraySet[T])(implicit order: Order[T]): NegatableArraySet[T] = {
    (lhs.negated, rhs.negated) match {
      case (false, false) ⇒ wrap(SetUtils.union(lhs.elements0, rhs.elements0), false)
      case (false, true)  ⇒ wrap(SetUtils.diff(rhs.elements0, lhs.elements0), true)
      case (true, false)  ⇒ wrap(SetUtils.diff(lhs.elements0, rhs.elements0), true)
      case (true, true)   ⇒ wrap(SetUtils.intersection(lhs.elements0, rhs.elements0), true)
    }
  }

  def intersect(rhs: NegatableArraySet[T])(implicit order: Order[T]): NegatableArraySet[T] = {
    (lhs.negated, rhs.negated) match {
      case (false, false) ⇒ wrap(SetUtils.intersection(lhs.elements0, rhs.elements0), false)
      case (false, true)  ⇒ wrap(SetUtils.diff(lhs.elements0, rhs.elements0), false)
      case (true, false)  ⇒ wrap(SetUtils.diff(rhs.elements0, lhs.elements0), false)
      case (true, true)   ⇒ wrap(SetUtils.union(lhs.elements0, rhs.elements0), true)
    }
  }

  def diff(rhs: NegatableArraySet[T])(implicit order: Order[T]): NegatableArraySet[T] = {
    (lhs.negated, rhs.negated) match {
      case (false, false) ⇒ wrap(SetUtils.diff(lhs.elements0, rhs.elements0), false)
      case (false, true)  ⇒ wrap(SetUtils.intersection(lhs.elements0, rhs.elements0), false)
      case (true, false)  ⇒ wrap(SetUtils.union(lhs.elements0, rhs.elements0), true)
      case (true, true)   ⇒ wrap(SetUtils.diff(rhs.elements0, lhs.elements0), false)
    }
  }

  override def equals(that: Any): Boolean = that match {
    case that: NegatableArraySet[T] => NegatableArraySet.eqv(Universal[T]).eqv(this, that)
    case _ => false
  }

  override def hashCode(): Int = NegatableArraySet.hash(Universal[T]).hash(this)

  override def toString: String = NegatableArraySet.show(Universal[T]).show(this)
}

private[abc] trait NegatableArraySet0 {

  implicit def eqv[T: Eq]: Eq[NegatableArraySet[T]] = new Eq[NegatableArraySet[T]] {
    def eqv(x: NegatableArraySet[T], y: NegatableArraySet[T]) = NegatableArraySet.eqv0(x, y)
  }
}

private[abc] trait NegatableArraySet1 extends NegatableArraySet0 {

  implicit def partialOrder[T: Order]: PartialOrder[NegatableArraySet[T]] = new PartialOrder[NegatableArraySet[T]] {
    override def eqv(x: NegatableArraySet[T], y: NegatableArraySet[T]) = NegatableArraySet.eqv0(x, y)
    def partialCompare(x: NegatableArraySet[T], y: NegatableArraySet[T]): Double =
      (x subsetOf y, y subsetOf x) match {
        case (true, true)   ⇒ 0.0
        case (true, false)  ⇒ -1.0
        case (false, true)  ⇒ +1.0
        case (false, false) ⇒ Double.NaN
      }
  }
}

object NegatableArraySet extends NegatableArraySet1 {

  private[abc] def eqv0[A: Eq](x: NegatableArraySet[A], y: NegatableArraySet[A]): Boolean =
    x.negated == y.negated && ArrayUtil.eqv(x.elements0, y.elements0)

  implicit def show[T: Show]: Show[NegatableArraySet[T]] = Show.show { s ⇒
    if(s.negated) {
      if (s.elements0.isEmpty) "All"
      else s.elements0.map(_.show).mkString("Except(", ",", ")")
    }
    else {
      if(s.elements0.isEmpty) "Empty"
      else s.elements0.map(_.show).mkString("Set(", ",", ")")
    }
  }

  implicit def hash[T: Hash]: Hash[NegatableArraySet[T]] = new Hash[NegatableArraySet[T]] {
    override def eqv(x: NegatableArraySet[T], y: NegatableArraySet[T]): Boolean = NegatableArraySet.eqv0(x, y)
    def hash(a: NegatableArraySet[T]): Int = (Hash.hash(a.negated), ArrayUtil.hash(a.elements0)).##
  }

  implicit def bool[T : Order]: Bool[NegatableArraySet[T]] = new Bool[NegatableArraySet[T]] {
    def complement(a: NegatableArraySet[T]) = a.negate
    def or(a: NegatableArraySet[T], b: NegatableArraySet[T]) = a union b
    def and(a: NegatableArraySet[T], b: NegatableArraySet[T]) = a intersect b
    override def xor(a: NegatableArraySet[T], b: NegatableArraySet[T]) = a xor b
    def zero = NegatableArraySet.empty[T]
    def one = NegatableArraySet.all[T]
  }

  private[abc] def wrap[T](elements: Array[T], negated: Boolean) = new NegatableArraySet[T](elements, negated)

  def fromBoolean[T](value: Boolean): NegatableArraySet[T] =
    if(value) all else empty

  def empty[T]: NegatableArraySet[T] =
    wrap(ArrayFactory.empty[T], false)

  def all[T]: NegatableArraySet[T] =
    wrap(ArrayFactory.empty[T], true)

  def singleton[T](e: T): NegatableArraySet[T] =
    wrap(ArrayFactory.singleton(e), false)

  def apply[T: Order](elements: T*): NegatableArraySet[T] = {
    val b = UnboxedArrayBuilder[T](elements.length)
    elements.foreach(b.add)
    new NegatableArraySet[T](sortAndRemoveDuplicatesInPlace(b.result), false)
  }
}
