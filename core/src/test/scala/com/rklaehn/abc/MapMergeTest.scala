package com.rklaehn.abc
import spire.implicits._

object MapMergeTest extends App {
  val r = ArrayMap(1 -> 2) merge ArrayMap(2 -> 3)
  println(r)
}
