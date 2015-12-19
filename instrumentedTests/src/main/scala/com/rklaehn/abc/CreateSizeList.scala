package com.rklaehn.abc

import org.github.jamm.MemoryMeter
import algebra.std.all._
import scala.collection.immutable.{SortedSet, HashSet}

object CreateSizeList extends App {

  lazy val mm = new MemoryMeter()
  lazy val overhead = mm.measure(new java.lang.Object)
  lazy val pointerSize = (mm.measure(new Array[java.lang.Object](256)) - mm.measure(new Array[java.lang.Object](128))) / 128

  println("|n|ArraySet|HashSet|SortedSet|")
  println("|--:|--:|--:|--:|")
  for(n <- Seq(1, 10, 100, 1000, 10000, 100000)) {
    val xs = Array(1 to n: _*)
    val as = ArraySet[Int](xs: _*)
    val hs = HashSet[Int](xs: _*)
    val ss = SortedSet[Int](xs: _*)
    val ass = mm.measureDeep(as)
    val hss = mm.measureDeep(hs)
    val sss = mm.measureDeep(ss)
    println(s"| $n| $ass| $hss| $sss|")
  }

  println()
  println("|n|ArrayMap|HashMap|SortedMap|")
  println("|--:|--:|--:|--:|")
  for(n <- Seq(1, 10, 100, 1000, 10000, 100000)) {
    val xs = Array(1 to n: _*)
    val entries = xs.map(x => x -> x)
    val as = ArrayMap(entries: _*)
    val hs = HashSet(entries: _*)
    val ss = SortedSet(entries: _*)
    val ass = mm.measureDeep(as)
    val hss = mm.measureDeep(hs)
    val sss = mm.measureDeep(ss)
    println(s"| $n| $ass| $hss| $sss|")
  }

  println()
  println("|n|ArraySeq|Vector|List|")
  println("|--:|--:|--:|--:|")
  for(n <- Seq(1, 10, 100, 1000, 10000, 100000)) {
    val xs = Array(1 to n: _*)
    val as = ArraySeq(xs: _*)
    val hs = Vector(xs: _*)
    val ss = List(xs: _*)
    val ass = mm.measureDeep(as)
    val hss = mm.measureDeep(hs)
    val sss = mm.measureDeep(ss)
    println(s"| $n| $ass| $hss| $sss|")
  }
}
