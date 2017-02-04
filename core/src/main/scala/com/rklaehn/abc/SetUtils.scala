package com.rklaehn.abc

import algebra.Order

/**
 * Set operations. Some of them can be used for both sets and ordered sequences, as well as from inside the radix tree
 */
private object SetUtils {

  def union[T: Order](a: Array[T], b: Array[T]): Array[T] =
    new UnionMerge[T](a, b).result

  def intersection[T: Order](a: Array[T], b: Array[T]): Array[T] =
    new IntersectionMerge[T](a, b).result

  def diff[T: Order](a: Array[T], b: Array[T]): Array[T] =
    new DiffMerge[T](a, b).result

  def xor[T: Order](a: Array[T], b: Array[T]): Array[T] =
    new XorMerge[T](a, b).result

  def subsetOf[T: Order](a: Array[T], b: Array[T]): Boolean = {
    try {
      new SubsetOf[T](a, b)
      true
    } catch {
      case x: AbortControl => false
    }
  }

  def intersects[T: Order](a: Array[T], b: Array[T]): Boolean = {
    try {
      new NoIntersect[T](a, b)
      false
    } catch {
      case x: AbortControl => true
    }
  }

  final class SubsetOf[T: Order](val a: Array[T], val b: Array[T]) extends BinaryMerge {
    def compare(ai: Int, bi: Int) = Order.compare(a(ai), b(bi))
    def collision(ai: Int, bi: Int): Unit = {}
    def fromA(a0: Int, a1: Int, bi: Int): Unit = {
      throw abort
    }
    def fromB(ai: Int, b0: Int, b1: Int): Unit = {}
    merge0(0, a.length, 0, b.length)
  }

  final class NoIntersect[T: Order](val a: Array[T], val b: Array[T]) extends BinaryMerge {
    def compare(ai: Int, bi: Int) = Order.compare(a(ai), b(bi))
    def collision(ai: Int, bi: Int): Unit = { throw abort }
    def fromA(a0: Int, a1: Int, bi: Int): Unit = {}
    def fromB(ai: Int, b0: Int, b1: Int): Unit = {}
    merge0(0, a.length, 0, b.length)
  }

  final class UnionMerge[T: Order](val a: Array[T], val b: Array[T]) extends BinaryMerge with UnboxedArrayBuilder[T] {
    def size = a.length + b.length
    def compare(ai: Int, bi: Int) = Order.compare(a(ai), b(bi))
    def collision(ai: Int, bi: Int): Unit = copyFrom(a, ai, 1)
    def fromA(a0: Int, a1: Int, bi: Int): Unit = copyFrom(a, a0, a1 - a0)
    def fromB(ai: Int, b0: Int, b1: Int): Unit = copyFrom(b, b0, b1 - b0)
    merge0(0, a.length, 0, b.length)
  }

  final class IntersectionMerge[T: Order](val a: Array[T], val b: Array[T]) extends BinaryMerge with UnboxedArrayBuilder[T] {
    def size = a.length min b.length
    def compare(ai: Int, bi: Int) = Order.compare(a(ai), b(bi))
    def collision(ai: Int, bi: Int): Unit = copyFrom(a, ai, 1)
    def fromA(a0: Int, a1: Int, bi: Int): Unit = {}
    def fromB(ai: Int, b0: Int, b1: Int): Unit = {}
    merge0(0, a.length, 0, b.length)
  }

  final class DiffMerge[T: Order](val a: Array[T], val b: Array[T]) extends BinaryMerge with UnboxedArrayBuilder[T] {
    def size = a.length
    def compare(ai: Int, bi: Int) = Order.compare(a(ai), b(bi))
    def collision(ai: Int, bi: Int): Unit = {}
    def fromA(a0: Int, a1: Int, bi: Int): Unit = copyFrom(a, a0, a1 - a0)
    def fromB(ai: Int, b0: Int, b1: Int): Unit = {}
    merge0(0, a.length, 0, b.length)
  }

  final class XorMerge[T: Order](val a: Array[T], val b: Array[T]) extends BinaryMerge with UnboxedArrayBuilder[T] {
    def size = a.length + b.length
    def compare(ai: Int, bi: Int) = Order.compare(a(ai), b(bi))
    def collision(ai: Int, bi: Int): Unit = {}
    def fromA(a0: Int, a1: Int, bi: Int): Unit = copyFrom(a, a0, a1 - a0)
    def fromB(ai: Int, b0: Int, b1: Int): Unit = copyFrom(b, b0, b1 - b0)
    merge0(0, a.length, 0, b.length)
  }
}

