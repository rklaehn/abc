package com.rklaehn.abc

import ichi.bench.Thyme
import ichi.bench.Thyme.HowWarm

import scala.reflect.ClassTag

/**
 * The purpose of this benchmark is to make sure that allocating using j.l.r.Array.newInstance is fast enough
 */
object ArrayResizeBench extends App {

  val th = Thyme.warmed(verbose = println, warmth = HowWarm.BenchOff)
  val t = Array(1)

  def alloc1: Array[Int] =
    t.resizeInPlace(10)

  def alloc2: Array[Int] =
    java.lang.reflect.Array.newInstance(t.getClass.getComponentType, 10).asInstanceOf[Array[Int]]

  th.pbenchOffWarm("ClassTag.newArray vs java.lang.reflect.Array.newInstance")(th.Warm(alloc1))(th.Warm(alloc2))
}
