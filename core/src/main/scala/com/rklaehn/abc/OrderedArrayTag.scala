package com.rklaehn.abc

import spire.algebra.{Order, Eq}

import scala.reflect.ClassTag
import scala.util.hashing.Hashing
import scala.{specialized => sp}
import spire.implicits._

trait ArrayTag[@sp T] {
  
  def empty: Array[T]

  def singleton(e: T): Array[T]

  def newArray(n: Int): Array[T]

  def eqv(a: Array[T], b: Array[T]): Boolean

  def hash(a: Array[T]): Int

  implicit def tClassTag: ClassTag[T]

  implicit def tEq: Eq[T]

  implicit def tHashing: Hashing[T]
}

trait OrderedArrayTag[@sp T] extends ArrayTag[T] {

  implicit def tClassTag: ClassTag[T]

  implicit def tHashing: Hashing[T]

  implicit def tOrder: Order[T]

  def compare(a: Array[T], ai: Int, b: Array[T], bi: Int): Int
}

object OrderedArrayTag {

  implicit def default[@sp T](implicit o: spire.algebra.Order[T], c: ClassTag[T], h: Hashing[T]): OrderedArrayTag[T] =
    new DefaultOrderedArrayTag[T]()(o, c, h)

  private class DefaultOrderedArrayTag[@sp T](implicit val tOrder: spire.algebra.Order[T], val tClassTag: ClassTag[T], val tHashing: Hashing[T]) extends OrderedArrayTag[T] {

    override def compare(a: Array[T], ai: Int, b: Array[T], bi: Int): Int = tOrder.compare(a(ai), b(bi))

    override def singleton(e: T): Array[T] = {
      val r = tClassTag.newArray(1)
      r(0) = e
      r
    }

    override def newArray(n: Int): Array[T] = tClassTag.newArray(n)

    override def eqv(a: Array[T], b: Array[T]): Boolean = {
      a === b
    }

    override def hash(a: Array[T]): Int = ArrayHashing.arrayHashCode(a)

    def tEq = tOrder

    val empty = Array.empty[T]
  }
}