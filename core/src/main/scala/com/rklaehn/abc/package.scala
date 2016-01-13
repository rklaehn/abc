package com.rklaehn

import algebra.{Order, Eq}

package object abc extends abc.abc1 {

  private[abc] type sp = scala.specialized
  private[abc] type ClassTag[A] = scala.reflect.ClassTag[A]
  private[abc] type tailrec     = scala.annotation.tailrec
  private[abc] val ClassTag = scala.reflect.ClassTag
  private[abc] val ILD = new Specializable.Group(Long, Int, Double)

  private[abc] final class AbortControl extends scala.util.control.ControlThrowable

  private[abc] val abort = new AbortControl

  private[abc] def newArray[T](n: Int, prototype: Array[T]) : Array[T] =
    java.lang.reflect.Array.newInstance(prototype.getClass.getComponentType, n).asInstanceOf[Array[T]]

  private[abc] implicit class ArrayCompanionOps(private val a: Array.type) extends AnyVal {
    def singleton[@sp(ILD) T: ClassTag](value: T) = {
      val result = new Array[T](1)
      result(0) = value
      result
    }

    def singleton[@sp(ILD) T](value: T, prototype: Array[T]) = {
      val result = newArray(1, prototype)
      result(0) = value
      result
    }
  }

  private[abc] implicit class ClassTagCompanionOps(private val c: ClassTag.type) extends AnyVal {

    def apply[T](implicit ev: ClassTag[T]): ClassTag[T] = ev
  }

  private[abc] def sortAndRemoveDuplicatesInPlace[@sp T: Order](a: Array[T]): Array[T] = {
    if(a.length <= 1)
      a
    else {
      // use mergeSort since it is better if the elements are pre-ordered, which should be common
      Sorting.sort(a)
      var j = 1
      var i = 1
      while (i < a.length) {
        if (Eq.neqv(a(j - 1), a(i))) {
          a(j) = a(i)
          j += 1
        }
        i += 1
      }
      a.resizeInPlace(j)
    }
  }

  implicit private[abc] class ArrayOps[T](private val underlying: Array[T]) extends AnyVal {

    def resizeInPlace(n: Int): Array[T] =
      if (underlying.length == n)
        underlying
      else {
        val r = newArray(n, underlying)
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
