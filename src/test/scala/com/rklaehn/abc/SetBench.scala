package com.rklaehn.abc

import ichi.bench.Thyme
import ichi.bench.Thyme.HowWarm
import spire.implicits._

import scala.collection.immutable.HashSet

object SetBench extends App {

  val th = new Thyme()
  // val th = Thyme.warmed(verbose = println, warmth = HowWarm.BenchOff)

  def create(): Unit = {
    for (n <- Array(1, 10, 100, 1000, 10000, 100000)) {
      val elements = (0 until n).toArray
      val r = th.pbenchOffWarm(s"$n")(
        th.Warm(Set(elements: _*).asInstanceOf[AnyRef]))(
          th.Warm(HashSet(elements: _*).asInstanceOf[AnyRef]))
    }
  }

  def access(): Unit = {
    for (n <- Array(1, 10, 100, 1000, 10000, 100000)) {
      val elements = (0 until n).toArray
      val s0 = HashSet(elements:_*)
      val s1 = Set(elements:_*)
      val x = n/2
      val r = th.pbenchOffWarm(s"$n")(
        th.Warm(s0(x)))(
          th.Warm(s1(x)))
    }
  }

//  create()
  access()
}
