package com.rklaehn.abc

import spire.algebra.Eq

import scala.reflect.ClassTag
import scala.util.hashing.Hashing
import scala.{specialized => sp}
import spire.implicits._

trait ArrayTag[@sp T] {

  def singleton(e: T): Array[T]

  def newArray(n: Int): Array[T]

  def eqv: Eq[Array[T]]

  def hashing: Hashing[Array[T]]
}

trait OrderedArrayTag[@sp T] extends ArrayTag[T] {

  def compare(a: Array[T], ai: Int, b: Array[T], bi: Int): Int
}

object OrderedArrayTag {

  implicit def default[@sp T](implicit o: spire.algebra.Order[T], c: ClassTag[T], h: Hashing[T]): OrderedArrayTag[T] =
    new DefaultOrderedArrayTag[T]()(o, c, h)

  private class DefaultOrderedArrayTag[@sp T](implicit o: spire.algebra.Order[T], c: ClassTag[T], h: Hashing[T]) extends OrderedArrayTag[T] {

    override def compare(a: Array[T], ai: Int, b: Array[T], bi: Int): Int = o.compare(a(ai), b(bi))

    override def singleton(e: T): Array[T] = {
      val r = c.newArray(1)
      r(0) = e
      r
    }

    override def newArray(n: Int): Array[T] = c.newArray(n)

    val eqv: Eq[Array[T]] = implicitly[Eq[Array[T]]]

    val hashing: Hashing[Array[T]] = new Hashing[Array[T]] {

      override def hash(x: Array[T]): Int = ArrayHashing.arrayHashCode(x)
    }
  }
}