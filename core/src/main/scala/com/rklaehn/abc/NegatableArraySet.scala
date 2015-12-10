package com.rklaehn.abc

import com.rklaehn.sonicreducer.Reducer

import language.implicitConversions
import scala.{ specialized => sp }
import algebra.Eq
import algebra.lattice.Bool

final class NegatableArraySet[@sp(Int, Long, Double) T] private[abc] (private[abc] val elements: Array[T], private[abc] val negated: Boolean) {
  lhs ⇒
  import NegatableArraySet._

  def elementsAsArraySeq: ArraySeq[T] =
    new ArraySeq[T](elements)

  def isEmpty: Boolean = !negated && elements.isEmpty

  def negate: NegatableArraySet[T] = wrap(elements, !negated)

  def apply(elem: T)(implicit tArrayTag: OrderedArrayTag[T]) = negated ^ (tArrayTag.binarySearch(elements, 0, elements.length, elem) >= 0)

  def +(elem: T)(implicit tArrayTag: OrderedArrayTag[T]) = lhs.union(singleton(elem))

  def -(elem: T)(implicit tArrayTag: OrderedArrayTag[T]) = lhs.diff(singleton(elem))

  def subsetOf(rhs: NegatableArraySet[T])(implicit tArrayTag: OrderedArrayTag[T]): Boolean =
    (lhs.negated, rhs.negated) match {
      case (false, false) ⇒ SetUtils.subsetOf(lhs.elements, rhs.elements)
      case (false, true)  ⇒ !SetUtils.intersects(lhs.elements, rhs.elements)
      case (true, false)  ⇒ false
      case (true, true)   ⇒ SetUtils.subsetOf(rhs.elements, lhs.elements)
    }

  def intersects(rhs: NegatableArraySet[T])(implicit tArrayTag: OrderedArrayTag[T]): Boolean = {
    (lhs.negated, rhs.negated) match {
      case (false, false) ⇒ SetUtils.intersects(lhs.elements, rhs.elements)
      case (false, true)  ⇒ !SetUtils.subsetOf(lhs.elements, rhs.elements)
      case (true, false)  ⇒ !SetUtils.subsetOf(rhs.elements, lhs.elements)
      case (true, true)   ⇒ true
    }
  }

  def xor(rhs: NegatableArraySet[T])(implicit tArrayTag: OrderedArrayTag[T]): NegatableArraySet[T] =
    wrap(SetUtils.xor(lhs.elements, rhs.elements), lhs.negated ^ rhs.negated)

  def union(rhs: NegatableArraySet[T])(implicit tArrayTag: OrderedArrayTag[T]): NegatableArraySet[T] = {
    (lhs.negated, rhs.negated) match {
      case (false, false) ⇒ wrap(SetUtils.union(lhs.elements, rhs.elements), false)
      case (false, true)  ⇒ wrap(SetUtils.diff(rhs.elements, lhs.elements), true)
      case (true, false)  ⇒ wrap(SetUtils.diff(lhs.elements, rhs.elements), true)
      case (true, true)   ⇒ wrap(SetUtils.intersection(lhs.elements, rhs.elements), true)
    }
  }

  def intersect(rhs: NegatableArraySet[T])(implicit tArrayTag: OrderedArrayTag[T]): NegatableArraySet[T] = {
    (lhs.negated, rhs.negated) match {
      case (false, false) ⇒ wrap(SetUtils.intersection(lhs.elements, rhs.elements), false)
      case (false, true)  ⇒ wrap(SetUtils.diff(lhs.elements, rhs.elements), false)
      case (true, false)  ⇒ wrap(SetUtils.diff(rhs.elements, lhs.elements), false)
      case (true, true)   ⇒ wrap(SetUtils.union(lhs.elements, rhs.elements), true)
    }
  }

  def diff(rhs: NegatableArraySet[T])(implicit tArrayTag: OrderedArrayTag[T]): NegatableArraySet[T] = {
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

  implicit def negatableArraySetEq[T](implicit tag: OrderedArrayTag[T]): Eq[NegatableArraySet[T]] = new Eq[NegatableArraySet[T]] {
    def eqv(x: NegatableArraySet[T], y: NegatableArraySet[T]) = x.negated == y.negated && tag.eqv(x.elements, y.elements)
  }

  implicit def negatableArraySetBool[T](implicit tag: OrderedArrayTag[T]): Bool[NegatableArraySet[T]] = new Bool[NegatableArraySet[T]] {

    def complement(a: NegatableArraySet[T]) = a.negate

    def or(a: NegatableArraySet[T], b: NegatableArraySet[T]) = a union b

    def and(a: NegatableArraySet[T], b: NegatableArraySet[T]) = a intersect b

    override def xor(a: NegatableArraySet[T], b: NegatableArraySet[T]) = a xor b

    def zero = NegatableArraySet.empty[T]

    def one = NegatableArraySet.all[T]
  }

  private[this] class NegatableArraySetBuilder[@sp(Int, Long, Double) T](implicit tag: OrderedArrayTag[T]) extends scala.collection.mutable.Builder[T, NegatableArraySet[T]] {

    private[this] def union(a: Array[T], b: Array[T]) = {
      SetUtils.union(a, b)
    }

    private[this] var reducer = Reducer[Array[T]](union)

    def +=(elem: T) = {
      reducer.apply(tag.singleton(elem))
      this
    }

    def clear() = {
      reducer = Reducer[Array[T]](union)
    }

    def result() = {
      reducer.result().map(x ⇒ wrap(x, false)).getOrElse(empty)
    }
  }

  private[abc] def wrap[T](elements: Array[T], negated: Boolean) = new NegatableArraySet[T](elements, negated)

  def fromBoolean[@sp(Int, Long, Double) T: OrderedArrayTag](value: Boolean): NegatableArraySet[T] =
    if(value) all else empty

  def empty[@sp(Int, Long, Double) T: OrderedArrayTag]: NegatableArraySet[T] =
    wrap(ArrayTag[T].empty, false)

  def all[@sp(Int, Long, Double) T: OrderedArrayTag]: NegatableArraySet[T] =
    wrap(ArrayTag[T].empty, true)

  def singleton[@sp(Int, Long, Double) T: OrderedArrayTag](e: T): NegatableArraySet[T] =
    wrap(ArrayTag[T].singleton(e), false)

  def apply[@sp(Int, Long, Double) T: OrderedArrayTag](elements: T*): NegatableArraySet[T] = {
    val b = new NegatableArraySetBuilder[T]
    b ++= elements
    b.result()
  }
}