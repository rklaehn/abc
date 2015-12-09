package com.rklaehn.abc

import ichi.bench.Thyme
import ichi.bench.Thyme.HowWarm

import scala.collection.immutable.HashMap

object SubsetOfBench extends App {
  val th = Thyme.warmed(verbose = println, warmth = HowWarm.BenchOff)
  val a = ArraySet(0 until 4 by 2:_*)
  val b = ArraySet(0 until 5:_*)
//
//  th.pbenchOffWarm("subsetOf")(
//    th.Warm(a subsetOf b))(
//    th.Warm(a subsetOf2 b))
//
//  th.pbenchOffWarm("union")(
//    th.Warm(a union b))(
//    th.Warm(a union2 b))
}
