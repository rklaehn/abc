package com.rklaehn.abc

import language.implicitConversions
import scala.util.hashing.Hashing
import scala.{specialized => sp}
import spire.algebra.Order
import spire.implicits._

import scala.reflect.ClassTag

final class ArraySet[@sp(Int, Long, Double) T] private[abc] (private[abc] val elements: Array[T])(implicit f: ArraySet.Family[T]) {
  import f._

  def toSeq: ArraySeq[T] =
    new ArraySeq[T](elements)(f.tSeqFamily)

  def apply(e: T): Boolean =
    SetOps.contains(elements, e)

  def subsetOf(that: ArraySet[T]): Boolean =
    SetOps.subsetOf(that.elements, this.elements)

  def intersects(that: ArraySet[T]): Boolean =
    SetOps.intersects(this.elements, that.elements)

  def union(that:ArraySet[T]): ArraySet[T] =
    new ArraySet[T](SetOps.union(this.elements, that.elements))

  def intersection(that:ArraySet[T]): ArraySet[T] =
    new ArraySet[T](SetOps.intersection(this.elements, that.elements))

  def diff(that: ArraySet[T]): ArraySet[T] =
    new ArraySet[T](SetOps.diff(this.elements, that.elements))

  def xor(that: ArraySet[T]): ArraySet[T] =
    new ArraySet[T](SetOps.xor(this.elements, that.elements))

  override def equals(that: Any) = that match {
    case that: ArraySet[T] => this.elements === that.elements
    case _ => false
  }

  override def hashCode:Int = ArrayHashing.arrayHashCode(elements)

  override def toString: String = elements.mkString("Set(",",",")")
}

object ArraySet {

  trait Family[@sp(Int, Long, Double) T] {

    def empty: ArraySet[T]

    implicit def tOrder: Order[T]

    implicit def tHashing: Hashing[T]

    def tSeqFamily: ArraySeq.Family[T]
  }

  implicit def genericFamily[@sp(Int, Long, Double) T: Order : ClassTag : Hashing] = new GenericFamily(Array.empty[T])

  private[abc] class GenericFamily[@sp(Int, Long, Double) T](ea:Array[T])(implicit val tOrder: Order[T], val tHashing: Hashing[T]) extends Family[T] {

    val empty = new ArraySet[T](ea)(this)

    val tSeqFamily = new ArraySeq.GenericFamily[T](empty.elements)
  }

  def empty[@sp(Int, Long, Double) T: Family]: ArraySet[T] = implicitly[Family[T]].empty

  def apply[@sp(Int, Long, Double) T](elements: T*)(implicit f: Family[T]): ArraySet[T] = {
    import f._
    val reducer = Reducer.create[ArraySet[T]](_ union _)
    for(e <- elements)
      reducer(singleton(e))
    reducer.result().getOrElse(f.empty)
  }

  def singleton[@sp(Int, Long, Double) T: Family](e: T): ArraySet[T] = new ArraySet[T](singletonArray(e))
}
