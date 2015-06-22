package com.rklaehn.abc

import spire.algebra.Eq
import scala.reflect.ClassTag
import scala.util.hashing.Hashing
import scala.{specialized => sp}
import spire.implicits._

object ArrayTag {

  implicit val byteArrayTag: ArrayTag[Byte] = OrderedArrayTag.byteOrderedArrayTag
  implicit val shortArrayTag: ArrayTag[Short] = OrderedArrayTag.shortOrderedArrayTag
  implicit val intArrayTag: ArrayTag[Int] = OrderedArrayTag.intOrderedArrayTag
  implicit val longArrayTag: ArrayTag[Long] = OrderedArrayTag.longOrderedArrayTag
  implicit val floatArrayTag: ArrayTag[Float] = OrderedArrayTag.floatOrderedArrayTag
  implicit val doubleArrayTag: ArrayTag[Double] = OrderedArrayTag.doubleOrderedArrayTag
  implicit val charArrayTag: ArrayTag[Char] = OrderedArrayTag.charOrderedArrayTag

  implicit def generic[T](implicit o: Eq[T], c: ClassTag[T], h: Hashing[T]): ArrayTag[T] =
    new GenericArrayTag[T]()(o, c, h)

  private[abc] class GenericArrayTag[@sp T](implicit val tEq: Eq[T], val tClassTag: ClassTag[T], val tHashing: Hashing[T]) extends ArrayTag[T] {

    override def singleton(e: T): Array[T] = {
      val r = tClassTag.newArray(1)
      r(0) = e
      r
    }

    override def resize(a: Array[T], n: Int) = a.resizeInPlace(n)(tClassTag)

    override def newArray(n: Int): Array[T] = tClassTag.newArray(n)

    override def eqv(a: Array[T], b: Array[T]): Boolean = a === b

    override def hash(a: Array[T]): Int = ArrayHashing.arrayHashCode(a)

    val empty = Array.empty[T]
  }
}

trait ArrayTag[@sp T] {

  def empty: Array[T]

  def singleton(e: T): Array[T]

  def newArray(n: Int): Array[T]

  def resize(a: Array[T], n: Int): Array[T]

  def eqv(a: Array[T], b: Array[T]): Boolean

  def hash(a: Array[T]): Int

  implicit def tClassTag: ClassTag[T]
}
