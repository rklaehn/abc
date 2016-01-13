package com.rklaehn.abc

import algebra.Order

/**
 * Set operations. Some of them can be used for both sets and ordered sequences, as well as from inside the radix tree
 */
private object SetUtils {

  def filter[@sp(ILD) T](a: Array[T], f: T => Boolean): Array[T] = {
    val r = newArray(a.length, a)
    var ri = 0
    var i = 0
    while (i < a.length) {
      if (f(a(i))) {
        r(ri) = a(i)
        ri += 1
      }
      i += 1
    }
    if (ri == r.length) a
    else r.resizeInPlace(ri)
  }

  def union[@sp(ILD) T: Order](a: Array[T], b: Array[T]): Array[T] =
    new UnionMerge[T](a, b).result

  def intersection[@sp(ILD) T: Order](a: Array[T], b: Array[T]): Array[T] =
    new IntersectionMerge[T](a, b).result

  def diff[@sp(ILD) T: Order](a: Array[T], b: Array[T]): Array[T] =
    new DiffMerge[T](a, b).result

  def xor[@sp(ILD) T: Order](a: Array[T], b: Array[T]): Array[T] =
    new XorMerge[T](a, b).result

  def subsetOf[@sp(ILD) T: Order](a: Array[T], b: Array[T]): Boolean = {
    try {
      new SubsetOf[T](a, b)
      true
    } catch {
      case x: AbortControl => false
    }
  }

  def intersects[@sp(ILD) T: Order](a: Array[T], b: Array[T]): Boolean = {
    try {
      new NoIntersect[T](a, b)
      false
    } catch {
      case x: AbortControl => true
    }
  }

  final class SubsetOf[@sp(ILD) T: Order](val a: Array[T], val b: Array[T]) extends BinaryMerge {
    def compare(ai: Int, bi: Int) = Order.compare(a(ai), b(bi))
    def collision(ai: Int, bi: Int): Unit = {}
    def fromA(a0: Int, a1: Int, bi: Int): Unit = {
      throw abort
    }
    def fromB(ai: Int, b0: Int, b1: Int): Unit = {}
    merge0(0, a.length, 0, b.length)
  }

  final class NoIntersect[@sp(ILD) T: Order](val a: Array[T], val b: Array[T]) extends BinaryMerge {
    def compare(ai: Int, bi: Int) = Order.compare(a(ai), b(bi))
    def collision(ai: Int, bi: Int): Unit = { throw abort }
    def fromA(a0: Int, a1: Int, bi: Int): Unit = {}
    def fromB(ai: Int, b0: Int, b1: Int): Unit = {}
    merge0(0, a.length, 0, b.length)
  }

  final class UnionMerge[@sp(ILD) T: Order](val a: Array[T], val b: Array[T]) extends BinaryMerge {
    val r = newArray(a.length + b.length, a)
    var ri: Int = 0
    def compare(ai: Int, bi: Int) = Order.compare(a(ai), b(bi))
    def collision(ai: Int, bi: Int): Unit = {
      System.arraycopy(a,ai,r,ri,1)
      // r(ri) = a(ai)
      ri += 1
    }
    def fromA(a0: Int, a1: Int, bi: Int): Unit = {
      System.arraycopy(a, a0, r, ri, a1 - a0)
      ri += a1 - a0
    }
    def fromB(ai: Int, b0: Int, b1: Int): Unit = {
      System.arraycopy(b, b0, r, ri, b1 - b0)
      ri += b1 - b0
    }
    def result: Array[T] = r.resizeInPlace(ri)
    merge0(0, a.length, 0, b.length)
  }

  final class IntersectionMerge[@sp(ILD) T: Order](val a: Array[T], val b: Array[T]) extends BinaryMerge {
    val r = newArray(a.length min b.length, a)
    var ri: Int = 0
    def compare(ai: Int, bi: Int) = Order.compare(a(ai), b(bi))
    def collision(ai: Int, bi: Int): Unit = {
      System.arraycopy(a,ai,r,ri,1)
      // r(ri) = a(ai)
      ri += 1
    }
    def fromA(a0: Int, a1: Int, bi: Int): Unit = {}
    def fromB(ai: Int, b0: Int, b1: Int): Unit = {}
    def result: Array[T] = r.resizeInPlace(ri)
    merge0(0, a.length, 0, b.length)
  }

  final class DiffMerge[@sp(ILD) T: Order](val a: Array[T], val b: Array[T]) extends BinaryMerge {
    val r = newArray(a.length, a)
    var ri: Int = 0
    def compare(ai: Int, bi: Int) = Order.compare(a(ai), b(bi))
    def collision(ai: Int, bi: Int): Unit = {}
    def fromA(a0: Int, a1: Int, bi: Int): Unit = {
      System.arraycopy(a, a0, r, ri, a1 - a0)
      ri += a1 - a0
    }
    def fromB(ai: Int, b0: Int, b1: Int): Unit = {}
    def result: Array[T] = r.resizeInPlace(ri)
    merge0(0, a.length, 0, b.length)
  }

  final class XorMerge[@sp(ILD) T: Order](val a: Array[T], val b: Array[T]) extends BinaryMerge {
    var ri: Int = 0
    val r = newArray(a.length + b.length, a)
    def compare(ai: Int, bi: Int) = Order.compare(a(ai), b(bi))
    def collision(ai: Int, bi: Int): Unit = {}
    def fromA(a0: Int, a1: Int, bi: Int): Unit = {
      System.arraycopy(a, a0, r, ri, a1 - a0)
      ri += a1 - a0
    }
    def fromB(ai: Int, b0: Int, b1: Int): Unit = {
      System.arraycopy(b, b0, r, ri, b1 - b0)
      ri += b1 - b0
    }
    def result: Array[T] = r.resizeInPlace(ri)
    merge0(0, a.length, 0, b.length)
  }
}
