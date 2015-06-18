package com.rklaehn.abc

import ichi.bench.Thyme
import ichi.bench.Thyme.HowWarm

import scala.collection.Traversable._

object FastAbstractTraversableBench extends App {

  val th = Thyme.warmed(verbose = println, warmth = HowWarm.BenchOff)

  val es = (0 until 1000000).map(_.toString).toArray

  val t0 = FastTraversable.fromForeach0(es.foreach)

  val t1 = FastTraversable.fromForeach(es.foreach)

  val id: String => String = x => x

  val dup: String => Seq[String] = x => Array(x, x)

  println(t0.map(id).getClass)

  println(t0.flatMap(dup).getClass)

  println(t1.map(id).getClass)

  println(t1.flatMap(dup).getClass)

  th.pbenchOffWarm("map FastTraversable vs Traversable")(th.Warm(t1.map(id)))(th.Warm(t0.map(id)))

  th.pbenchOffWarm("flatMap FastTraversable vs Traversable")(th.Warm(t1.flatMap(dup)))(th.Warm(t0.flatMap(dup)))
}