package com.rklaehn.abc

import ichi.bench.Thyme
import ichi.bench.Thyme.HowWarm

import scala.collection.immutable.{SortedSet, HashSet}

object SetCreateAccessBench extends App {

  val th = Thyme.warmed(verbose = println, warmth = HowWarm.BenchOff)

  def createVaryMerge(): Unit = {
    for (n ← Array(1, 10, 100, 1000, 10000, 100000)) {
      val elements = (0 until n).toArray
      def s0 = ArraySet.apply(elements:_*)
      def s1 = ArraySet.apply2(elements:_*)
      //      def s3 = ArraySet2(elements:_*)
      th.pbenchOffWarm(s"Create ArraySet[Int].apply vs ArraySet[Int].apply2 $n")(
        th.Warm(s0.asInstanceOf[AnyRef]))(
          th.Warm(s1.asInstanceOf[AnyRef]))
    }
  }

  def unionVaryMerge(): Unit = {
    for (n ← Array(1, 10, 100, 1000, 10000, 100000)) {
      val elements = (0 until n).toArray
      val a = ArraySet.apply(elements:_*)
      val b = ArraySet(elements.drop(n / 2).take(1):_*)
      //      def s3 = ArraySet2(elements:_*)
      th.pbenchOffWarm(s"Union [1,2,3,4] and [2,4] $n union1 union2")(
        th.Warm(a union b))(
          th.Warm(a union2 b))
      th.pbenchOffWarm(s"Union [2,4] and [1,2,3,4] $n union1 union2")(
        th.Warm(b union a))(
          th.Warm(b union2 a))
    }
  }

  def createInt(): Unit = {
    for (n ← Array(1, 10, 100, 1000, 10000, 100000)) {
      val elements = (0 until n).toArray
      def s0 = HashSet(elements:_*)
      def s1 = SortedSet(elements:_*)
      def s2 = ArraySet(elements:_*)
//      def s3 = ArraySet2(elements:_*)
      th.pbenchOffWarm(s"Create HashSet[Int] vs ArraySet[Int] $n")(
        th.Warm(s0.asInstanceOf[AnyRef]))(
        th.Warm(s2.asInstanceOf[AnyRef]))
      th.pbenchOffWarm(s"Create SortedSet[Int] vs ArraySet[Int] $n")(
        th.Warm(s1.asInstanceOf[AnyRef]))(
        th.Warm(s2.asInstanceOf[AnyRef]))
    }
  }

  def accessInt(): Unit = {
    for (n ← Array(1, 10, 100, 1000, 10000, 100000)) {
      val elements = (0 until n).toArray
      val s0 = HashSet(elements:_*)
      val s1 = SortedSet(elements:_*)
      val s2 = ArraySet(elements:_*)
//      val s3 = ArraySet2(elements:_*)
      val x = 0
      th.pbenchOffWarm(s"Access HashSet[Int] vs ArraySet[Int] $n")(
        th.Warm(s0(x)))(
        th.Warm(s2(x)))
      th.pbenchOffWarm(s"Access SortedSet[Int] vs ArraySet[Int] $n")(
        th.Warm(s1(x)))(
        th.Warm(s2(x)))
/*      th.pbenchOffWarm(s"Access ArraySet[Int] vs ArraySet2[Int] $n")(
        th.Warm(s2(x)))(
        th.Warm(s3(x)))*/
    }
  }

//  createInt()
//  accessInt()
//  createVaryMerge()
  unionVaryMerge()
}
