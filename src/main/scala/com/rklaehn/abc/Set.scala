package com.rklaehn.abc

import language.implicitConversions
import scala.{specialized => sp}
import spire.algebra.Order
import spire.implicits._

import scala.reflect.ClassTag

final class Set[@sp T] private[abc] (private[abc] val elements: Array[T], private[abc] val negated: Boolean = false) {
  import Set._

  def apply(e: T)(implicit o:Order[T]): Boolean =
    SetOps.contains(elements, e) ^ negated

  def negate: Set[T] = new Set[T](elements, !negated)

  def subsetOf(that: Set[T])(implicit o:Order[T]): Boolean = {
    if(this.negated) {
      if(that.negated)
        SetOps.subsetOf(that.elements, this.elements)
      else
        SetOps.intersects(that.elements, this.elements)
    } else {
      if(that.negated)
        !SetOps.intersects(that.elements, this.elements)
      else
        SetOps.subsetOf(this.elements, that.elements)
    }
  }

  def intersects(that: Set[T])(implicit o:Order[T]): Boolean =
    SetOps.intersects(this.elements, that.elements)

  def union(that:Set[T])(implicit o:Order[T]): Set[T] = {    
    unsafe[T](SetOps.union(this.elements, that.elements))
  }

  def intersection(that:Set[T])(implicit o:Order[T]): Set[T] =
    unsafe[T](SetOps.intersection(this.elements, that.elements))

  def diff(that: Set[T])(implicit o:Order[T]): Set[T] =
    unsafe[T](SetOps.diff(this.elements, that.elements))

  def xor(that: Set[T])(implicit o:Order[T]): Set[T] =
    unsafe[T](SetOps.xor(this.elements, that.elements))

  override def toString: String = elements.mkString("Set(",",",")")
}

object Set {

  implicit def algebra[T](s: Set[T])(implicit o:Order[T]): Order[Set[T]] = new Order[Set[T]] {

    override def eqv(x: Set[T], y: Set[T]) =
      x.elements === y.elements

    def compare(x: Set[T], y: Set[T]) =
      x.elements compare y.elements
  }

  @inline private def unsafe[@sp T](a: Array[T], negated: Boolean = false) = new Set[T](a, negated)

  def empty[@sp T: ClassTag]: Set[T] = new Set[T](Array.empty[T])

  def apply[@sp T: Order: ClassTag](elements: T*): Set[T] = {
    val reducer = Reducer.create[Set[T]](_ union _)
    for(e <- elements)
      reducer(singleton(e))
    reducer.result().getOrElse(empty[T])
  }

  def singleton[@sp T: ClassTag](e: T): Set[T] = new Set[T](Array(e))
}
