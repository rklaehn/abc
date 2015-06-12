package com.rklaehn.abc
import spire.implicits._

object MapMergeTest extends App {
  val r = Map(1 -> 2) merge Map(2 -> 3)
  println(r)
}
