package com.rklaehn.abc

import algebra.{Order, Eq}

// scalastyle:off return
private[abc] object ArrayUtil {

  def eqv[A: Eq](x: Array[A], y: Array[A]): Boolean = {
    x.length == y.length && {
      var i = 0
      while(i < x.length) {
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
    while(i < a.length) {
      result = MurmurHash3.mix(result, Hash.hash(a(i)))
      i += 1
    }
    result
  }

  def compare[@sp A: Order](x: Array[A], y: Array[A]): Int = {
    var i = 0
    while (i < x.length && i < y.length) {
      val cmp = Order.compare(x(i), y(i))
      if (cmp != 0) return cmp
      i += 1
    }
    sign(x.length - y.length)
  }

  def dropRightWhile[T: Eq](a: Array[T], z: T): Array[T] = {
    @tailrec
    def lastIndexWhereZero(i: Int): Int =
      if(i == 0) i
      else if(Eq.neqv(a(i - 1), z)) i
      else lastIndexWhereZero(i - 1)
    a.resizeInPlace(lastIndexWhereZero(a.length))
  }

  def vectorCompare[@sp A: Order](x: Array[A], xd:A, y: Array[A], yd: A): Int = {
    var i = 0
    while (i < x.length && i < y.length) {
      val cmp = Order.compare(x(i), y(i))
      if (cmp != 0) return cmp
      i += 1
    }
    while (i < x.length) {
      val cmp = Order.compare(x(i), yd)
      if (cmp != 0) return cmp
      i += 1
    }
    while (i < y.length) {
      val cmp = Order.compare(xd, y(i))
      if (cmp != 0) return cmp
      i += 1
    }
    0
  }

  def combine[A](x: Array[A], x_d: A, y: Array[A], y_d: A)(f: (A, A) => A): Array[A] = {
    val re = newArray(x.length max y.length, x)
    var i = 0
    while (i < x.length && i < y.length) {
      re(i) = f(x(i), y(i))
      i += 1
    }
    while (i < x.length) {
      re(i) = f(x(i), y_d)
      i += 1
    }
    while (i < y.length) {
      re(i) = f(x_d, y(i))
      i += 1
    }
    re
  }

  def exists[@sp T](a: Array[T], p: T ⇒ Boolean): Boolean = {
    var i = 0
    while(i < a.length) {
      if(p(a(i)))
        return true
      i += 1
    }
    false
  }

  def forall[@sp T](a: Array[T], p: T ⇒ Boolean): Boolean = {
    var i = 0
    while(i < a.length) {
      if(!p(a(i)))
        return false
      i += 1
    }
    true
  }

  def foreach[@sp T](a: Array[T], f: T ⇒ Unit): Unit = {
    var i = 0
    while(i < a.length) {
      f(a(i))
      i += 1
    }
  }

  def filter[@sp T](a: Array[T], f: T => Boolean): Array[T] = {

    @tailrec
    def filter1(r: Array[T], ri: Int, i: Int): Array[T] = {
      if(i == a.length) r.resizeInPlace(ri)
      else if(!f(a(i))) filter1(r, ri, i + 1)
      else {
        r(ri) = a(i)
        filter1(r, ri + 1, i + 1)
      }
    }

    @tailrec
    def filter0(i: Int): Array[T] = {
      if(i == a.length) a
      else if(f(a(i))) filter0(i + 1)
      else {
        val r = newArray(a.length - 1, a)
        val ri = i
        System.arraycopy(a, 0, r, 0, ri)
        filter1(r, ri, i + 1)
      }
    }
    filter0(0)
  }

  private[this] def sign(x: Int) =
    if(x > 0) 1
    else if(x < 0) -1
    else 0
}