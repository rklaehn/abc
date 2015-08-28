package com.rklaehn.abc

import spire.algebra.Eq
import scala.reflect.ClassTag
import scala.util.hashing.{MurmurHash3, Hashing}
import scala.{specialized => sp}
import spire.implicits._

trait ArrayTag[@sp T] extends Eq[Array[T]] with Hashing[Array[T]] {
  def empty: Array[T]
  def singleton(e: T): Array[T]
  def newArray(n: Int): Array[T]
  def copyOf(a: Array[T], n: Int): Array[T]
  implicit def classTag: ClassTag[T]
  final def resizeInPlace(a: Array[T], n: Int): Array[T] = if(n == a.length) a else copyOf(a, n)
}

object ArrayTag {

  @inline final def apply[@sp T](implicit ev: ArrayTag[T]): ArrayTag[T] = ev

  private[abc] def hash[T](a: Array[T])(implicit elementHashing: Hashing[T]): Int =  {
    var r = MurmurHash3.arraySeed
    var i = 0
    while(i < a.length) {
      r = MurmurHash3.mix(r, elementHashing.hash(a(i)))
      i += 1
    }
    r
  }

  implicit val anyRefArrayTag: ArrayTag[AnyRef] = new ArrayTag[AnyRef] {
    override def singleton(e: AnyRef): Array[AnyRef] = {
      val r = classTag.newArray(1)
      r(0) = e
      r
    }

    override def newArray(n: Int): Array[AnyRef] = classTag.newArray(n)

    override val empty: Array[AnyRef] = new Array[AnyRef](0)

    override def copyOf(a: Array[AnyRef], n: Int): Array[AnyRef] = java.util.Arrays.copyOf(a, n)

    override val classTag: ClassTag[AnyRef] = implicitly[ClassTag[AnyRef]]

    override def eqv(x: Array[AnyRef], y: Array[AnyRef]): Boolean = java.util.Arrays.equals(x, y)

    override def hash(x: Array[AnyRef]): Int = java.util.Arrays.hashCode(x)
  }
  implicit val byteArrayTag: ArrayTag[Byte] = OrderedArrayTag.byteOrderedArrayTag
  implicit val shortArrayTag: ArrayTag[Short] = OrderedArrayTag.shortOrderedArrayTag
  implicit val intArrayTag: ArrayTag[Int] = OrderedArrayTag.intOrderedArrayTag
  implicit val longArrayTag: ArrayTag[Long] = OrderedArrayTag.longOrderedArrayTag
  implicit val floatArrayTag: ArrayTag[Float] = OrderedArrayTag.floatOrderedArrayTag
  implicit val doubleArrayTag: ArrayTag[Double] = OrderedArrayTag.doubleOrderedArrayTag
  implicit val charArrayTag: ArrayTag[Char] = OrderedArrayTag.charOrderedArrayTag
  implicit val booleanArrayTag: ArrayTag[Boolean] = OrderedArrayTag.booleanOrderedArrayTag

  implicit def generic[T](implicit o: Eq[T], c: ClassTag[T], h: Hashing[T]): ArrayTag[T] =
    new GenericArrayTag[T]()(o, c, h)

  def reference[T <: AnyRef](implicit classTag: ClassTag[T]) = new ReferenceArrayTag[T](classTag)

  private[abc] class ReferenceArrayTag[T <: AnyRef](val classTag: ClassTag[T]) extends ArrayTag[T] {
    def empty = Array.empty[T](classTag)
    def singleton(e: T) = Array(e)(classTag)
    def newArray(n: Int) = classTag.newArray(n)
    def hash(x: Array[T]) = java.util.Arrays.hashCode(x.asInstanceOf[Array[AnyRef]])
    def copyOf(a: Array[T], n: Int) = java.util.Arrays.copyOf(a.asInstanceOf[Array[AnyRef]], n).asInstanceOf[Array[T]]
    def eqv(x: Array[T], y: Array[T]) = java.util.Arrays.equals(x.asInstanceOf[Array[AnyRef]], y.asInstanceOf[Array[AnyRef]])
  }

  private[abc] class GenericArrayTag[@sp T](implicit val tEq: Eq[T], val classTag: ClassTag[T], val tHashing: Hashing[T]) extends ArrayTag[T] {

    override def singleton(e: T): Array[T] = {
      val r = classTag.newArray(1)
      r(0) = e
      r
    }

    override def copyOf(a: Array[T], n: Int) = {
      val t = newArray(n)
      System.arraycopy(a, 0, t, 0, n)
      t
    }

    override def newArray(n: Int): Array[T] = classTag.newArray(n)

    override def eqv(a: Array[T], b: Array[T]): Boolean = spire.std.array.ArrayEq(tEq).eqv(a, b)

    override def hash(a: Array[T]): Int = ArrayTag.hash(a)

    val empty = Array.empty[T]
  }
}
