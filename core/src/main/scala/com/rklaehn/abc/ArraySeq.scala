package com.rklaehn.abc

import spire.algebra.Eq

import scala.reflect.ClassTag
import scala.util.hashing.Hashing
import scala.{specialized => sp}
import spire.implicits._

class ArraySeq[@sp(Int, Long, Double) T] private[abc] (
    private val elements: Array[T])(
    implicit f: ArraySeq.Family[T]) {
  import f._

  override def equals(that: Any) = that match {
    case that: ArraySeq[T] => this.elements === that.elements
    case _ => false
  }

  override def hashCode:Int = ArrayHashing.arrayHashCode(elements)
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

  def single[@sp(Int, Long, Double) T: Family](e: T): ArraySeq[T] =
    new ArraySeq[T](singletonArray(e))

  def apply[@sp(Int, Long, Double) T: Family: ClassTag](elements: T*): ArraySeq[T] =
    new ArraySeq[T](elements.toArray)
}
