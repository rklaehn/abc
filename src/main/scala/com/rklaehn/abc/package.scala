package com.rklaehn

import scala.reflect.ClassTag

package object abc {

  def singletonArray[@specialized T](value: T): Array[T] = {
    val r = java.lang.reflect.Array.newInstance(value.getClass, 1).asInstanceOf[Array[T]]
    r(0) = value
    r
  }

  implicit class ArrayOps[T](private val underlying: Array[T]) extends AnyVal {

    def newArray(n: Int): Array[T] =
      java.lang.reflect.Array.newInstance(underlying.getClass.getComponentType, n).asInstanceOf[Array[T]]

    def resizeInPlace(n: Int): Array[T] =
      if(underlying.length == n)
        underlying
      else {
        val r = java.lang.reflect.Array.newInstance(underlying.getClass.getComponentType, n).asInstanceOf[Array[T]]
        // val r = ClassTag.apply[T](underlying.getClass.getComponentType).newArray(n)
        // val r = c.newArray(n)
        System.arraycopy(underlying, 0, r, 0, n min underlying.length)
        r
      }
  }
}
