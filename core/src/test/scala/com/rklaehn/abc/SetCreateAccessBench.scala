package com.rklaehn.abc

import ichi.bench.Thyme
import ichi.bench.Thyme.HowWarm

import scala.collection.immutable.HashSet

object SetCreateAccessBench extends App {

  val th = Thyme.warmed(verbose = println, warmth = HowWarm.BenchOff)

  def createInt(): Unit = {
    for (n ← Array(1, 10, 100, 1000, 10000, 100000, 1000000)) {
      val elements = (0 until n).toArray
      def s0 = HashSet(elements:_*)
      def s1 = ArraySet(elements:_*)
      th.pbenchOffWarm(s"Create HashSet[Int] vs ArraySet[Int] $n")(
        th.Warm(s0.asInstanceOf[AnyRef]))(
        th.Warm(s1.asInstanceOf[AnyRef]))
    }
  }

  def accessInt(): Unit = {
    for (n ← Array(1, 10, 100, 1000, 10000, 100000, 1000000)) {
      val elements = (0 until n).toArray
      val s0 = HashSet(elements:_*)
      val s1 = ArraySet(elements:_*)
      val x = n/2
      val r = th.pbenchOffWarm(s"Access HashSet[Int] vs ArraySet[Int] $n")(
        th.Warm(s0(x)))(
          th.Warm(s1(x)))
    }
  }

  createInt()
  accessInt()
}
