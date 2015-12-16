package com.rklaehn

import algebra.{Order, Eq}

package object abc extends abc.abc1 {

  private[abc] type sp = scala.specialized
  private[abc] type ClassTag[A] = scala.reflect.ClassTag[A]
  private[abc] type tailrec     = scala.annotation.tailrec
  private[abc] val ClassTag = scala.reflect.ClassTag
  private[abc] val ILD = new Specializable.Group(Long, Int, Double)

  implicit class ArrayCompanionOps(private val a: Array.type) extends AnyVal {
    def singleton[@sp(ILD) T](value: T)(implicit classTag: ClassTag[T]) = {
      val result = classTag.newArray(1)
      result(0) = value
      result
    }
  }

  implicit class ClassTagCompanionOps(private val c: ClassTag.type) extends AnyVal {

    def apply[T](implicit ev: ClassTag[T]): ClassTag[T] = ev
  }

  private[abc] def sortAndRemoveDuplicatesInPlace[@sp T: Order: ClassTag](a: Array[T]): Array[T] = {
    if(a.length <= 1)
      a
    else {
      // use mergeSort since it is better if the elements are pre-ordered, which should be common
      Sorting.sort(a)
      var j = 1
      var i = 1
      while (i < a.length) {
        if (Eq.neqv(a(j - 1), a(j))) {
          a(j) = a(i)
          j += 1
        }
        i += 1
      }
      a.resizeInPlace(j)
    }
  }

  implicit private[abc] class ArrayOps[T](private val underlying: Array[T]) extends AnyVal {

    def resizeInPlace(n: Int)(implicit c: ClassTag[T]): Array[T] =
      if (underlying.length == n)
        underlying
      else {
        val r = c.newArray(n)
        System.arraycopy(underlying, 0, r, 0, n min underlying.length)
        r
      }
  }

  implicit def arrayOrder[@sp A: Order]: Order[Array[A]] = new Order[Array[A]] {
    override def eqv(x: Array[A], y: Array[A]): Boolean = ArrayUtil.eqv(x, y)
    def compare(x: Array[A], y: Array[A]): Int = ArrayUtil.compare(x, y)
  }
}

package abc {

  private[abc] trait abc0 {

    // todo: remove once algebra has array instances (or use spire for instances once that moves to algebra)?
    implicit def arrayEq[@specialized A: Eq]: Eq[Array[A]] = new Eq[Array[A]] {
      override def eqv(x: Array[A], y: Array[A]): Boolean = ArrayUtil.eqv(x, y)
    }
  }

  private[abc] trait abc1 extends abc0 {

    implicit def arrayHash[@specialized A: Hash]: Hash[Array[A]] = new Hash[Array[A]] {
      def eqv(x: Array[A], y: Array[A]): Boolean = ArrayUtil.eqv(x, y)

      def hash(a: Array[A]): Int = ArrayUtil.hash(a)
    }
  }
}
