package com.rklaehn.abc

object RadixTreeTest extends App {

  val t = RadixTree("" -> 1, "a" -> 2, "c" -> 3)

  require(t.count == 3)

  val u = t.filterPrefix("a")

  require(u.count == 1)
}
