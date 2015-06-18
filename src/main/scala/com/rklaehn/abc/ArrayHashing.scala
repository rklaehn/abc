package com.rklaehn.abc

import scala.util.hashing.Hashing
import scala.util.hashing.MurmurHash3._

object ArrayHashing {

  implicit def arrayHashing[T: Hashing] = new Hashing[Array[T]] {

    def hash(xs: Array[T]): Int = arrayHashCode(xs)
  }

  def arrayHashCode[T](xs: Array[T])(implicit h: Hashing[T]): Int = {
    var i = 0
    var r = arraySeed
    while (i < xs.length) {
      r = mix(r, h.hash(xs(i)))
      i += 1
    }
    r
  }
}
