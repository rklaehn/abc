package com.rklaehn.abc

import ichi.bench.Thyme
import ichi.bench.Thyme.HowWarm
import spire.implicits._
import spire.math.Rational

object SpecializationBench extends App {

  val th = Thyme.warmed(verbose = println, warmth = HowWarm.BenchOff)

  /*
  val aa0 = (0 until 1000000).map(Rational.apply).toArray
  val ae = Rational(123456)
  val aa = aa0.asInstanceOf[Array[AnyRef]]
  val as = Set(aa0: _*)
  */

  val aa0 = (0 until 1000000).toArray
  val ae = 123456
  val aa = aa0
  val as = ArraySet(aa0: _*)

  def search1: Int =
    java.util.Arrays.binarySearch(aa, 0, aa.length, ae)

  def search2: Int =
    SetOps.binarySearch(aa, ae, 0, aa.length)

  def search3: Int =
    if(as(ae)) 1 else 0

  th.pbenchOffWarm("java.util.binarySearch vs Order[T]-based binarySearch")(th.Warm(search1))(th.Warm(search2))

  th.pbenchOffWarm("java.util.binarySearch vs Set[T].indexOf")(th.Warm(search1))(th.Warm(search3))
}
