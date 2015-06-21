package com.rklaehn.abc

import spire.algebra.Eq

import scala.reflect.ClassTag
import scala.util.hashing.Hashing
import spire.implicits._
import scala.{ specialized => sp }

class ArraySeq[@sp(Int, Long, Double) T] private[abc] (
  private[abc] val elements: Array[T])(
    implicit f: ArraySeq.Family[T]) {
  import f._

  def asIndexedSeq: IndexedSeq[T] = new IndexedSeq[T] {

    def length = elements.length

    def apply(idx: Int) = elements(idx)
  }

  def isEmpty: Boolean = elements.isEmpty

  def concat(that: ArraySeq[T]): ArraySeq[T] =
    if (this.isEmpty) that
    else if (that.isEmpty) this
    else {
      val temp = elements.newArray(this.elements.length + that.elements.length)
      System.arraycopy(that.elements, 0, temp, 0, this.elements.length)
      System.arraycopy(that.elements, 0, temp, this.elements.length, that.elements.length)
      new ArraySeq[T](temp)
    }

  override def equals(that: Any) = that match {
    case that: ArraySeq[T] => this.elements === that.elements
    case _ => false
  }

  override def hashCode: Int = ArrayHashing.arrayHashCode(elements)
}

object ArraySeq {

  trait Family[@sp(Int, Long, Double) T] {

    implicit def tEq: Eq[T]

    implicit def tHashing: Hashing[T]

    def empty: ArraySeq[T]
  }

  implicit def genericFamily[@sp(Int, Long, Double) T: Eq: Hashing: ClassTag]: Family[T] =
    new GenericFamily(Array.empty[T])

  private[abc] final class GenericFamily[@sp(Int, Long, Double) T](
    val tEmptyArray: Array[T])(
      implicit val tEq: Eq[T], val tHashing: Hashing[T]) extends Family[T] {
    val empty = new ArraySeq[T](tEmptyArray)(this)
  }

  def empty[@sp(Int, Long, Double) T: Family]: ArraySeq[T] =
    implicitly[Family[T]].empty

  def singleton[@sp(Int, Long, Double) T: Family](e: T): ArraySeq[T] =
    new ArraySeq[T](singletonArray(e))

  def apply[@sp(Int, Long, Double) T: Family: ClassTag](elements: T*): ArraySeq[T] =
    new ArraySeq[T](elements.toArray)
}
