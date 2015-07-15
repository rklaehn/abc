package com.rklaehn.abc

import spire.algebra.Order
import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.hashing.Hashing
import scala.{specialized => sp}
import spire.implicits._

trait OrderedArrayTag[@sp T] extends ArrayTag[T] {
  implicit def classTag: ClassTag[T]
  implicit def order: Order[T]
  def sort(as: Array[T]): Unit
  def binarySearch(as: Array[T], a0: Int, a1: Int, a: T): Int
  def compare(a: Array[T], ai: Int, b: Array[T], bi: Int): Int
}

object OrderedArrayTag {

  @inline final def apply[@sp T](implicit ev: OrderedArrayTag[T]): OrderedArrayTag[T] = ev

  implicit def generic[T](implicit o: Order[T], c: ClassTag[T], h: Hashing[T]): OrderedArrayTag[T] =
    new GenericOrderedArrayTag[T]()(o, c, h)

  private def binarySearch[@sp(Int, Long, Double) T](a: Array[T], e: T, from: Int, until: Int)(implicit o: Order[T]): Int = {

    @tailrec
    def binarySearch0(low: Int, high: Int): Int =
      if (low <= high) {
        val mid = (low + high) >>> 1
        val c = o.compare(e, a(mid))
        if (c > 0)
          binarySearch0(mid + 1, high)
        else if (c < 0)
          binarySearch0(low, mid - 1)
        else
          mid
      } else -(low + 1)
    binarySearch0(from, until - 1)
  }

  private[abc] class GenericOrderedArrayTag[@sp T](implicit val order: spire.algebra.Order[T], val classTag: ClassTag[T], val tHashing: Hashing[T]) extends OrderedArrayTag[T] {
    override def compare(a: Array[T], ai: Int, b: Array[T], bi: Int): Int = order.compare(a(ai), b(bi))
    override def singleton(e: T): Array[T] = {
      val r = classTag.newArray(1)
      r(0) = e
      r
    }
    override def binarySearch(as: Array[T], a0: Int, a1: Int, a: T) = OrderedArrayTag.binarySearch(as, a, a0, a1)
    override def sort(as: Array[T]): Unit = spire.math.Sorting.sort(as)
    override def copyOf(a: Array[T], n: Int) = {
      val t = newArray(n)
      System.arraycopy(a, 0, t, 0, n)
      t
    }
    override def newArray(n: Int): Array[T] = classTag.newArray(n)
    override def eqv(a: Array[T], b: Array[T]): Boolean = spire.std.array.ArrayEq(order).eqv(a, b)
    override def hash(a: Array[T]): Int = ArrayTag.hash(a)
    def tEq = order
    val empty = Array.empty[T]
  }

  implicit val byteOrderedArrayTag: OrderedArrayTag[Byte] = new OrderedArrayTag[Byte] {
    val classTag = implicitly[ClassTag[Byte]]
    val order = implicitly[Order[Byte]]
    val empty = Array.empty[Byte]
    def compare(a: Array[Byte], ai: Int, b: Array[Byte], bi: Int) = java.lang.Byte.compare(a(ai), b(bi))
    def binarySearch(as: Array[Byte], a0: Int, a1: Int, a: Byte) = java.util.Arrays.binarySearch(as, a0, a1, a)
    def copyOf(a: Array[Byte], n: Int) = java.util.Arrays.copyOf(a, n)
    def sort(a: Array[Byte]) = java.util.Arrays.sort(a)
    def singleton(e: Byte) = {
      val r = new Array[Byte](1)
      r(0) = e
      r
    }
    def newArray(n: Int) = new Array[Byte](n)
    def hash(a: Array[Byte]) = java.util.Arrays.hashCode(a)
    def eqv(a: Array[Byte], b: Array[Byte]) = java.util.Arrays.equals(a, b)
  }

  implicit val shortOrderedArrayTag: OrderedArrayTag[Short] = new OrderedArrayTag[Short] {
    val classTag = implicitly[ClassTag[Short]]
    val order = implicitly[Order[Short]]
    val empty = Array.empty[Short]
    def compare(a: Array[Short], ai: Int, b: Array[Short], bi: Int) = java.lang.Short.compare(a(ai), b(bi))
    def binarySearch(as: Array[Short], a0: Int, a1: Int, a: Short) = java.util.Arrays.binarySearch(as, a0, a1, a)
    def copyOf(a: Array[Short], n: Int) = java.util.Arrays.copyOf(a, n)
    def sort(a: Array[Short]) = java.util.Arrays.sort(a)
    def singleton(e: Short) = {
      val r = new Array[Short](1)
      r(0) = e
      r
    }
    def newArray(n: Int) = new Array[Short](n)
    def hash(a: Array[Short]) = java.util.Arrays.hashCode(a)
    def eqv(a: Array[Short], b: Array[Short]) = java.util.Arrays.equals(a, b)
  }

  implicit val intOrderedArrayTag: OrderedArrayTag[Int] = new OrderedArrayTag[Int] {
    val classTag = implicitly[ClassTag[Int]]
    val order = implicitly[Order[Int]]
    val empty = Array.empty[Int]
    def compare(a: Array[Int], ai: Int, b: Array[Int], bi: Int) = java.lang.Integer.compare(a(ai), b(bi))
    def binarySearch(as: Array[Int], a0: Int, a1: Int, a: Int) = java.util.Arrays.binarySearch(as, a0, a1, a)
    def copyOf(a: Array[Int], n: Int) = java.util.Arrays.copyOf(a, n)
    def sort(a: Array[Int]) = java.util.Arrays.sort(a)
    def singleton(e: Int) = {
      val r = new Array[Int](1)
      r(0) = e
      r
    }
    def newArray(n: Int) = new Array[Int](n)
    def hash(a: Array[Int]) = java.util.Arrays.hashCode(a)
    def eqv(a: Array[Int], b: Array[Int]) = java.util.Arrays.equals(a, b)
  }

  implicit val longOrderedArrayTag: OrderedArrayTag[Long] = new OrderedArrayTag[Long] {
    val classTag = implicitly[ClassTag[Long]]
    val order = implicitly[Order[Long]]
    val empty = Array.empty[Long]
    def compare(a: Array[Long], ai: Int, b: Array[Long], bi: Int) = java.lang.Long.compare(a(ai), b(bi))
    def binarySearch(as: Array[Long], a0: Int, a1: Int, a: Long) = java.util.Arrays.binarySearch(as, a0, a1, a)
    def copyOf(a: Array[Long], n: Int) = java.util.Arrays.copyOf(a, n)
    def sort(a: Array[Long]) = java.util.Arrays.sort(a)
    def singleton(e: Long) = {
      val r = new Array[Long](1)
      r(0) = e
      r
    }
    def newArray(n: Int) = new Array[Long](n)
    def hash(a: Array[Long]) = java.util.Arrays.hashCode(a)
    def eqv(a: Array[Long], b: Array[Long]) = java.util.Arrays.equals(a, b)
  }

  implicit val floatOrderedArrayTag: OrderedArrayTag[Float] = new OrderedArrayTag[Float] {
    val classTag = implicitly[ClassTag[Float]]
    val order = implicitly[Order[Float]]
    val empty = Array.empty[Float]
    def compare(a: Array[Float], ai: Int, b: Array[Float], bi: Int) = java.lang.Double.compare(a(ai), b(bi))
    def binarySearch(as: Array[Float], a0: Int, a1: Int, a: Float) = java.util.Arrays.binarySearch(as, a0, a1, a)
    def copyOf(a: Array[Float], n: Int) = java.util.Arrays.copyOf(a, n)
    def sort(a: Array[Float]) = java.util.Arrays.sort(a)
    def singleton(e: Float) = {
      val r = new Array[Float](1)
      r(0) = e
      r
    }
    def newArray(n: Int) = new Array[Float](n)
    def hash(a: Array[Float]) = java.util.Arrays.hashCode(a)
    def eqv(a: Array[Float], b: Array[Float]) = java.util.Arrays.equals(a, b)
  }

  implicit val doubleOrderedArrayTag: OrderedArrayTag[Double] = new OrderedArrayTag[Double] {
    val classTag = implicitly[ClassTag[Double]]
    val order = implicitly[Order[Double]]
    val empty = Array.empty[Double]
    def compare(a: Array[Double], ai: Int, b: Array[Double], bi: Int) = java.lang.Double.compare(a(ai), b(bi))
    def binarySearch(as: Array[Double], a0: Int, a1: Int, a: Double) = java.util.Arrays.binarySearch(as, a0, a1, a)
    def copyOf(a: Array[Double], n: Int) = java.util.Arrays.copyOf(a, n)
    def sort(a: Array[Double]) = java.util.Arrays.sort(a)
    def singleton(e: Double) = {
      val r = new Array[Double](1)
      r(0) = e
      r
    }
    def newArray(n: Int) = new Array[Double](n)
    def hash(a: Array[Double]) = java.util.Arrays.hashCode(a)
    def eqv(a: Array[Double], b: Array[Double]) = java.util.Arrays.equals(a, b)
  }

  implicit val booleanOrderedArrayTag: OrderedArrayTag[Boolean] = new OrderedArrayTag[Boolean] {
    val classTag = implicitly[ClassTag[Boolean]]
    val order = implicitly[Order[Boolean]]
    val empty = Array.empty[Boolean]
    def compare(a: Array[Boolean], ai: Int, b: Array[Boolean], bi: Int) = java.lang.Boolean.compare(a(ai), b(bi))
    def binarySearch(as: Array[Boolean], a0: Int, a1: Int, a: Boolean) = OrderedArrayTag.binarySearch(as, a, a0, a1)
    def copyOf(a: Array[Boolean], n: Int) = java.util.Arrays.copyOf(a, n)
    def sort(a: Array[Boolean]) = spire.math.Sorting.sort(a)
    def singleton(e: Boolean) = {
      val r = new Array[Boolean](1)
      r(0) = e
      r
    }
    def newArray(n: Int) = new Array[Boolean](n)
    def hash(a: Array[Boolean]) = java.util.Arrays.hashCode(a)
    def eqv(a: Array[Boolean], b: Array[Boolean]) = java.util.Arrays.equals(a, b)
  }

  implicit val charOrderedArrayTag: OrderedArrayTag[Char] = new OrderedArrayTag[Char] {
    val classTag = implicitly[ClassTag[Char]]
    val order = implicitly[Order[Char]]
    val empty = Array.empty[Char]
    def compare(a: Array[Char], ai: Int, b: Array[Char], bi: Int) = java.lang.Long.compare(a(ai), b(bi))
    def binarySearch(as: Array[Char], a0: Int, a1: Int, a: Char) = java.util.Arrays.binarySearch(as, a0, a1, a)
    def copyOf(a: Array[Char], n: Int) = java.util.Arrays.copyOf(a, n)
    def sort(a: Array[Char]) = java.util.Arrays.sort(a)
    def singleton(e: Char) = {
      val r = new Array[Char](1)
      r(0) = e
      r
    }
    def newArray(n: Int) = new Array[Char](n)
    def hash(a: Array[Char]) = java.util.Arrays.hashCode(a)
    def eqv(a: Array[Char], b: Array[Char]) = java.util.Arrays.equals(a, b)
  }
}
