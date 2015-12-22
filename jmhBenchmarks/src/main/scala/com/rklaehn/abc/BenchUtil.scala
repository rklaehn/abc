package com.rklaehn.abc

import scala.util.hashing.MurmurHash3

trait BenchUtil {

  def mix(x: Int): Int = MurmurHash3.mix(0, x)
}
