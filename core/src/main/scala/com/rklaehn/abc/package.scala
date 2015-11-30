package com.rklaehn

import scala.reflect.ClassTag
import scala.util.hashing.Hashing

package object abc {

  implicit class ClassTagCompanionOps(private val c: ClassTag.type) extends AnyVal {

    def apply[T](implicit ev: ClassTag[T]): ClassTag[T] = ev
  }

  implicit class ClassTagOps[T](private val underlying: ClassTag[T]) extends AnyVal {

    def singletonArray(value: T): Array[T] = {
      val a = underlying.newArray(1)
      a(0) = value
      a
    }

    def emptyArray: Array[T] = underlying.newArray(0)
  }

  implicit class HashingOps(private val underlying: Hashing.type) extends AnyVal {
    def apply[T](implicit ev: Hashing[T]): Hashing[T] = ev
  }

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
}
