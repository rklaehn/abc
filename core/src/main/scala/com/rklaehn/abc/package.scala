package com.rklaehn

import algebra.{Order, Eq}

import scala.reflect.ClassTag
import scala.util.hashing.MurmurHash3

package object abc {

  implicit class ArrayCompanionOps(private val a: Array.type) extends AnyVal {
    def singleton[@specialized T](value: T)(implicit classTag: ClassTag[T]) = {
      val result = classTag.newArray(1)
      result(0) = value
      result
    }
  }

  implicit class ClassTagCompanionOps(private val c: ClassTag.type) extends AnyVal {

    def apply[T](implicit ev: ClassTag[T]): ClassTag[T] = ev
  }

  implicit private[abc] class ArrayOps[T](private val underlying: Array[T]) extends AnyVal {

    def sortAndRemoveDuplicatesInPlace()(implicit order: Order[T], classTag: ClassTag[T]): Array[T] = {
      if(underlying.length <= 1)
        underlying
      else {
        val a = underlying
        // use mergeSort since it is better if the elements are pre-ordered, which should be common
        Sorting.sort(a)
        var j = 1
        var i = 1
        while (i < underlying.length) {
          if (Eq.neqv(a(j - 1), a(j))) {
            a(j) = a(i)
            j += 1
          }
          i += 1
        }
        resizeInPlace(j)
      }
    }

    def resizeInPlace(n: Int)(implicit c: ClassTag[T]): Array[T] =
      if (underlying.length == n)
        underlying
      else {
        val r = c.newArray(n)
        System.arraycopy(underlying, 0, r, 0, n min underlying.length)
        r
      }
  }

  // $COVERAGE-OFF$
  // scalastyle:off return
  // todo: remove once algebra has array instances (or use spire for instances once that moves to algebra)?
  implicit def arrayEq[@specialized A](implicit aEq: Eq[A]): Eq[Array[A]] = new Eq[Array[A]] {
    def eqv(x: Array[A], y: Array[A]): Boolean = {
      x.length == y.length && {
        var i = 0
        while(i < x.length) {
          if(!aEq.eqv(x(i), y(i)))
            return false
          i += 1
        }
        true
      }
    }
  }

  implicit def arrayHash[@specialized A](implicit aHash: Hash[A]): Hash[Array[A]] = new Hash[Array[A]] {
    def eqv(x: Array[A], y: Array[A]): Boolean = {
      x.length == y.length && {
        var i = 0
        while(i < x.length) {
          if(!aHash.eqv(x(i), y(i)))
            return false
          i += 1
        }
        true
      }
    }

    override def hash(a: Array[A]): Int = {
      var result = MurmurHash3.arraySeed
      var i = 0
      while(i < a.length) {
        result = MurmurHash3.mix(result, aHash.hash(a(i)))
        i += 1
      }
      result
    }
  }
  // $COVERAGE-ON$
  // scalastyle:on return
}
