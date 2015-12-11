package com.rklaehn

import algebra.Eq

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

  // $COVERAGE-OFF$
  implicit class ArrayOps[T](private val underlying: Array[T]) extends AnyVal {

    def updated(index: Int, value: T): Array[T] = {
      val result = underlying.clone
      result(index) = value
      result
    }

    def patched(index: Int, value: T)(implicit c: ClassTag[T]): Array[T] = {
      val result = new Array[T](underlying.length + 1)
      System.arraycopy(underlying, 0, result, 0, index)
      result(index) = value
      if (index < underlying.length)
        System.arraycopy(underlying, index, result, index + 1, underlying.length - index)
      result
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
}
