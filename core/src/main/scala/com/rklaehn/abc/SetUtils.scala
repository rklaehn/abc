package com.rklaehn.abc

import scala.annotation.tailrec
import scala.util.control.ControlThrowable
import scala.{specialized => sp}
import spire.algebra.Order

/**
 * Set operations. Some of them can be used for both sets and ordered sequences, as well as from inside the radix tree
 */
private[abc] object SetUtils {

  private class AbortControl extends ControlThrowable

  private[this] val abort = new AbortControl

  def binarySearch[@sp(Int, Long, Double) T](a: Array[T], e: T, from: Int, until: Int)(implicit o:Order[T]): Int = {

    @tailrec
    def binarySearch0(low: Int, high: Int): Int =
      if (low <= high) {
        val mid = (low + high) >>> 1
        val c = o.compare(e, a(mid))
        if (c > 0)
          binarySearch0(mid + 1, high)
        else if (c < 0)
          binarySearch0(low, mid - 1)
        else
          mid
      } else -(low + 1)
    binarySearch0(from, until - 1)
  }

  def contains[@sp(Int, Long, Double) T: Order](a: Array[T], e: T): Boolean =
    binarySearch(a, e, 0, a.length) >= 0

  def indexOf[@sp(Int, Long, Double) T: Order](a: Array[T], e: T): Int =
    binarySearch(a, e, 0, a.length)

  def union[@sp(Int, Long, Double) T: Order](a: Array[T], b: Array[T]): Array[T] =
    new UnionMerge[T](a,b,a.newArray(a.length + b.length), implicitly[Order[T]]).result

  def intersection[@sp(Int, Long, Double) T: Order](a: Array[T], b: Array[T]): Array[T] =
    new IntersectionMerge[T](a,b,a.newArray(a.length min b.length), implicitly[Order[T]]).result

  def diff[@sp(Int, Long, Double) T: Order](a: Array[T], b: Array[T]): Array[T] =
    new DiffMerge[T](a,b,a.newArray(a.length), implicitly[Order[T]]).result

  def xor[@sp(Int, Long, Double) T: Order](a: Array[T], b: Array[T]): Array[T] =
    new XorMerge[T](a,b,a.newArray(a.length + b.length), implicitly[Order[T]]).result

  def subsetOf[@sp(Int, Long, Double) T: Order](a: Array[T], b: Array[T]): Boolean = {
    try {
      new SubsetOf[T](a,b,implicitly[Order[T]])
      true
    } catch {
      case x:AbortControl => false
    }
  }

  def intersects[@sp(Int, Long, Double) T: Order](a: Array[T], b: Array[T]): Boolean = {
    try {
      new NoIntersect[T](a,b,implicitly[Order[T]])
      false
    } catch {
      case x:AbortControl => true
    }
  }

  sealed trait BooleanOperation[@sp(Int, Long, Double) T] extends spire.math.BinaryMerge {

    def a: Array[T]

    def b: Array[T]

    def order: Order[T]

    def compare(ai: Int, bi: Int) = order.compare(a(ai), b(bi))

    merge0(0, a.length, 0, b.length)
  }

  final class SubsetOf[@sp(Int, Long, Double) T](val a: Array[T], val b: Array[T], val order: Order[T]) extends BooleanOperation[T] {

    def collision(ai: Int, bi: Int): Unit = {}

    def fromA(a0: Int, a1: Int, bi: Int): Unit = { throw abort }

    def fromB(ai: Int, b0: Int, b1: Int): Unit = {}
  }

  final class NoIntersect[@sp(Int, Long, Double) T](val a: Array[T], val b: Array[T], val order: Order[T]) extends BooleanOperation[T] {

    def collision(ai: Int, bi: Int): Unit = { throw abort }

    def fromA(a0: Int, a1: Int, bi: Int): Unit = {}

    def fromB(ai: Int, b0: Int, b1: Int): Unit = {}
  }

  sealed trait AbstractMerge[@sp(Int, Long, Double) T] extends spire.math.BinaryMerge {

    def a: Array[T]

    def b: Array[T]

    def r: Array[T]

    def order: Order[T]

    def compare(ai: Int, bi: Int): Int = order.compare(a(ai), b(bi))

    def copyFromA(a0: Int, a1: Int): Unit = {
      System.arraycopy(a, a0, r, ri, a1 - a0)
      ri += a1 - a0
    }

    def copyFromB(b0: Int, b1: Int): Unit = {
      System.arraycopy(b, b0, r, ri, b1 - b0)
      ri += b1 - b0
    }

    var ri: Int = 0

    merge0(0, a.length, 0, b.length)

    def result: Array[T] = r.resizeInPlace(ri)
  }

  final class UnionMerge[@sp(Int, Long, Double) T](val a: Array[T], val b: Array[T], val r: Array[T], val order: Order[T]) extends AbstractMerge[T] {

    def collision(ai: Int, bi: Int): Unit = {
      r(ri) = a(ai)
      ri += 1
    }

    def fromA(a0: Int, a1: Int, bi: Int): Unit = copyFromA(a0, a1)

    def fromB(ai: Int, b0: Int, b1: Int): Unit = copyFromB(b0, b1)
  }

  final class IntersectionMerge[@sp(Int, Long, Double) T](val a: Array[T], val b: Array[T], val r: Array[T], val order: Order[T]) extends AbstractMerge[T] {

    def collision(ai: Int, bi: Int): Unit = {
      r(ri) = a(ai)
      ri += 1
    }

    def fromA(a0: Int, a1: Int, bi: Int): Unit = {}

    def fromB(ai: Int, b0: Int, b1: Int): Unit = {}
  }

  final class DiffMerge[@sp(Int, Long, Double) T](val a: Array[T], val b: Array[T], val r: Array[T], val order: Order[T]) extends AbstractMerge[T] {

    def collision(ai: Int, bi: Int): Unit = {}

    def fromA(a0: Int, a1: Int, bi: Int): Unit = copyFromA(a0, a1)

    def fromB(ai: Int, b0: Int, b1: Int): Unit = {}
  }

  final class XorMerge[@sp(Int, Long, Double) T](val a: Array[T], val b: Array[T], val r: Array[T], val order: Order[T]) extends AbstractMerge[T] {

    def collision(ai: Int, bi: Int): Unit = {}

    def fromA(a0: Int, a1: Int, bi: Int): Unit = copyFromA(a0, a1)

    def fromB(ai: Int, b0: Int, b1: Int): Unit = copyFromB(b0, b1)
  }
}
