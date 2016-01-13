package com.rklaehn.abc

import ichi.bench.Thyme
import ichi.bench.Thyme.HowWarm

/**
 * The purpose of this benchmark is to make sure that allocating using j.l.r.Array.newInstance is fast enough
 */
object ArrayCreateBench extends App {

  val th = Thyme.warmed(verbose = println, warmth = HowWarm.BenchOff)
  val t = Array.empty[Int]

  def alloc1: Array[Int] =
    new Array[Int](1)

  def alloc2: Array[Int] =
    java.lang.reflect.Array.newInstance(t.getClass.getComponentType, 1).asInstanceOf[Array[Int]]

  th.pbenchOffWarm("ClassTag.newArray vs java.lang.reflect.Array.newInstance")(th.Warm(alloc1))(th.Warm(alloc2))
}
