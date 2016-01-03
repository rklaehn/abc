package com.rklaehn.abc

import algebra.Order

/**
 * Set operations. Some of them can be used for both sets and ordered sequences, as well as from inside the radix tree
 */
private object SetUtils {

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

//  object UnionMerge2 {
//    final val FromA = 0
//    final val FromB = 1
//  }
//
//  final class UnionMerge2[@sp(ILD) T](val a: Array[T], val b: Array[T])(implicit t:OrderedArrayTag[T]) extends BinaryMerge {
//    import UnionMerge2._
//    val r = t.newArray(a.length + b.length)
//    var ri: Int = 0
//    var mode: Int = -1
//    var start: Int = 0
//    var end: Int = 0
//
//    def exec(): Unit = {
//      mode match {
//        case FromA ⇒
//          val size = end - start
//          System.arraycopy(a, start, r, ri, size)
//          ri += size
//        case FromB ⇒
//          val size = end - start
//          System.arraycopy(b, start, r, ri, size)
//          ri += size
//        case _ ⇒
//      }
//    }
//
//    @inline
//    private[this] final def op(mode: Int, start:Int, end: Int): Unit = {
//      if(this.mode == mode)
//        this.end = end
//      else {
//        exec()
//        this.mode = mode
//        this.start = start
//        this.end = end
//      }
//    }
//
//    def binarySearchB(ai: Int, b0: Int, b1: Int) = t.binarySearch(b, b0, b1, a(ai))
//    def compare(ai: Int, bi: Int): Int = t.order.compare(a(ai), b(bi))
//    def collision(ai: Int, bi: Int): Unit = op(FromA, ai, ai + 1)
//    def fromA(a0: Int, a1: Int, bi: Int): Unit = op(FromA, a0, a1)
//    def fromB(ai: Int, b0: Int, b1: Int): Unit = op(FromB, b0, b1)
//    def result: Array[T] = t.resizeInPlace(r, ri)
//    merge0(0, a.length, 0, b.length)
//    exec()
//  }

  final class UnionMerge[@sp(ILD) T: Order](val a: Array[T], val b: Array[T]) extends BinaryMerge {
    val r = newArray(a.length + b.length, a, b)
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
    val r = newArray(a.length + b.length, a, b)
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
