package com.rklaehn.abc

import algebra.{Order, Eq}

// scalastyle:off return
private[abc] object ArrayUtil {

  def eqv[A: Eq](x: Array[A], y: Array[A]): Boolean = {
    x.sl == y.sl && {
      var i = 0
      while(i < x.sl) {
        if(!Eq.eqv(x(i), y(i)))
          return false
        i += 1
      }
      true
    }
  }

  def hash[@sp A: Hash](a: Array[A]): Int = {
    import scala.util.hashing.MurmurHash3
    var result = MurmurHash3.arraySeed
    var i = 0
    while(i < a.sl) {
      result = MurmurHash3.mix(result, Hash.hash(a(i)))
      i += 1
    }
    result
  }

  def compare[@sp A: Order](x: Array[A], y: Array[A]): Int = {
    var i = 0
    while (i < x.sl && i < y.sl) {
      val cmp = Order.compare(x(i), y(i))
      if (cmp != 0) return cmp
      i += 1
    }
    sign(x.sl - y.sl)
  }

  def dropRightWhile[T: Eq](a: Array[T], z: T): Array[T] = {
    @tailrec
    def lastIndexWhereZero(i: Int): Int =
      if(i == 0) i
      else if(Eq.neqv(a(i - 1), z)) i
      else lastIndexWhereZero(i - 1)
    a.resizeInPlace(lastIndexWhereZero(a.sl))
  }

  def vectorCompare[@sp A: Order](x: Array[A], xd:A, y: Array[A], yd: A): Int = {
    var i = 0
    while (i < x.sl && i < y.sl) {
      val cmp = Order.compare(x(i), y(i))
      if (cmp != 0) return cmp
      i += 1
    }
    while (i < x.sl) {
      val cmp = Order.compare(x(i), yd)
      if (cmp != 0) return cmp
      i += 1
    }
    while (i < y.sl) {
      val cmp = Order.compare(xd, y(i))
      if (cmp != 0) return cmp
      i += 1
    }
    0
  }

  def combine[A](x: Array[A], x_d: A, y: Array[A], y_d: A)(f: (A, A) => A): Array[A] = {
    val re = newArray(x.sl max y.sl, x, y)
    var i = 0
    while (i < x.sl && i < y.sl) {
      re(i) = f(x(i), y(i))
      i += 1
    }
    while (i < x.sl) {
      re(i) = f(x(i), y_d)
      i += 1
    }
    while (i < y.sl) {
      re(i) = f(x_d, y(i))
      i += 1
    }
    re
  }

  private[this] def sign(x: Int) =
    if(x > 0) 1
    else if(x < 0) -1
    else 0
}