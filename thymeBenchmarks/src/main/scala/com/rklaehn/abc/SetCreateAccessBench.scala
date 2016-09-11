package com.rklaehn.abc

import cats.kernel.instances.all._
import ichi.bench.Thyme
import ichi.bench.Thyme.HowWarm

import scala.collection.immutable.{HashSet, SortedSet}

object SetCreateAccessBench extends App {

  val th = Thyme.warmed(verbose = println, warmth = HowWarm.BenchOff)

  val ns = Array(1, 10, 100, 1000, 10000, 100000)

  def createInt(): Unit = {
    for (n ← ns) {
      val elements = (0 until n).toArray
      def s0 = HashSet(elements:_*)
      def s1 = SortedSet(elements:_*)
      def s2 = ArraySet(elements:_*)
      th.pbenchOffWarm(s"Create HashSet[Int] vs ArraySet[Int] $n")(
        th.Warm(s0.asInstanceOf[AnyRef]))(
        th.Warm(s2.asInstanceOf[AnyRef]))
      th.pbenchOffWarm(s"Create SortedSet[Int] vs ArraySet[Int] $n")(
        th.Warm(s1.asInstanceOf[AnyRef]))(
        th.Warm(s2.asInstanceOf[AnyRef]))
    }
  }

  def accessInt(): Unit = {
    for (n ← ns) {
      val elements = (0 until n).toArray
      val s0 = HashSet(elements:_*)
      val s1 = SortedSet(elements:_*)
      val s2 = ArraySet(elements:_*)
      val x = 0
      th.pbenchOffWarm(s"Access HashSet[Int] vs ArraySet[Int] $n")(
        th.Warm(s0(x)))(
        th.Warm(s2(x)))
      th.pbenchOffWarm(s"Access SortedSet[Int] vs ArraySet[Int] $n")(
        th.Warm(s1(x)))(
        th.Warm(s2(x)))
    }
  }

  createInt()
  accessInt()
}
