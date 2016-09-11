package com.rklaehn.abc

 import cats.kernel.instances.all._
import ichi.bench.Thyme
import ichi.bench.Thyme.HowWarm

import scala.collection.immutable.HashMap

object MapCreateAccessBench extends App {

  val th = Thyme.warmed(verbose = println, warmth = HowWarm.BenchOff)

  def create(): Unit = {
    for (n ← Array(1, 10, 100, 1000, 10000, 100000, 1000000)) {
      val elements = (0 until n).toArray.map(x ⇒ x → x)
      def s0 = HashMap(elements:_*)
      def s1 = ArrayMap(elements:_*)
      th.pbenchOffWarm(s"Create HashMap[Int, Int] vs ArrayMap[Int, Int] $n")(
        th.Warm(s0.asInstanceOf[AnyRef]))(
          th.Warm(s1.asInstanceOf[AnyRef]))
    }
  }

  def access(): Unit = {
    for (n ← Array(1, 10, 100, 1000, 10000, 100000, 1000000)) {
      val elements = (0 until n).toArray.map(x ⇒ x → x)
      val s0 = HashMap(elements:_*)
      val s1 = ArrayMap(elements:_*)
      val x = n/2
      val r = th.pbenchOffWarm(s"Access HashMap[Int, Int] vs ArrayMap[Int, Int] $n")(
        th.Warm(s0(x)))(
          th.Warm(s1.apply0(x)))
    }
  }

  def createIntString(): Unit = {
    for (n ← Array(1, 10, 100, 1000, 10000, 100000)) {
      val elements = (0 until n).toArray.map(x ⇒ x → x.toString)
      def s0 = HashMap(elements:_*)
      def s1 = ArrayMap(elements:_*)
      th.pbenchOffWarm(s"Create HashMap[Int, String] vs ArrayMap[Int, String] $n")(
        th.Warm(s0.asInstanceOf[AnyRef]))(
          th.Warm(s1.asInstanceOf[AnyRef]))
    }
  }

  def accessIntString(): Unit = {
    for (n ← Array(1, 10, 100, 1000, 10000, 100000)) {
      val elements = (0 until n).toArray.map(x ⇒ x → x.toString)
      val s0 = HashMap(elements:_*)
      val s1 = ArrayMap(elements:_*)
      val x = n/2
      val r = th.pbenchOffWarm(s"Access HashMap[Int, String] vs ArrayMap[Int, String] $n")(
        th.Warm(s0(x)))(
          th.Warm(s1.apply0(x)))
    }
  }

  create()
  access()
  createIntString()
  accessIntString()
}