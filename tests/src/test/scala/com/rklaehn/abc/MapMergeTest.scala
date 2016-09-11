package com.rklaehn.abc

import algebra.Order

 import cats.kernel.instances.all._
import Instances._

object MapMergeTest extends App {

  val r = ArrayMap(1 -> 2) merge ArrayMap(2 -> 3)
  println(r)

  val r2 = ArrayMap(1L -> "2") merge ArrayMap(2L -> "3")
//  val r3 = r2.updated(1L, "x")
  println(r2)
}
