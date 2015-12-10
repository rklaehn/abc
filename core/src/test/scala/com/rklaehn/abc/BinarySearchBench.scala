package com.rklaehn.abc

import algebra.{Order, Eq}
import ichi.bench.Thyme
import ichi.bench.Thyme.HowWarm
import algebra.std.all._

object BinarySearchBench extends App {

  val th = Thyme.warmed(verbose = println, warmth = HowWarm.BenchOff)

  def block(f: ⇒ Unit) = f
  block {
    val as = (0 until 1000).map(_.toByte).toArray
    val bs = (0 until 1000).map(_.toByte).toArray
    def eq = Eq[Array[Byte]]
    def hash = Hash[Array[Byte]]
    th.pbenchOffWarm(
      "java.util.Arrays.binarySearch vs. Order[Byte]"
    )(
        th.Warm(java.util.Arrays.binarySearch(as, 0, as.length, as(17)))
      )(
        th.Warm(Searching.search(as, 0, as.length, as(17)))
      )

    th.pbenchOffWarm(
      "java.util.Arrays.equals vs. Eq[Array[Byte]]"
    )(
        th.Warm(java.util.Arrays.equals(as, bs))
      )(
        th.Warm(eq.eqv(as, bs))
      )

    th.pbenchOffWarm(
      "java.util.Arrays.hashCode vs. Hash[Array[Byte]]"
    )(
        th.Warm(java.util.Arrays.hashCode(as))
      )(
        th.Warm(hash.hash(as))
      )
  }
  block {
    val as = (0 until 1000).map(_.toShort).toArray
    val bs = (0 until 1000).map(_.toShort).toArray
    def eq = Eq[Array[Short]]
    def hash = Hash[Array[Short]]
    th.pbenchOffWarm(
      "java.util.Arrays.binarySearch vs. Order[Short]"
    )(
        th.Warm(java.util.Arrays.binarySearch(as, 0, as.length, as(17)))
      )(
        th.Warm(Searching.search(as, 0, as.length, as(17)))
      )

    th.pbenchOffWarm(
      "java.util.Arrays.equals vs. Eq[Array[Short]]"
    )(
        th.Warm(java.util.Arrays.equals(as, bs))
      )(
        th.Warm(eq.eqv(as, bs))
      )

    th.pbenchOffWarm(
      "java.util.Arrays.hashCode vs. Hash[Array[Short]]"
    )(
        th.Warm(java.util.Arrays.hashCode(as))
      )(
        th.Warm(hash.hash(as))
      )
  }
  block {
    val as = (0 until 1000).map(_.toInt).toArray
    val bs = (0 until 1000).map(_.toInt).toArray
    def eq = Eq[Array[Int]]
    def hash = Hash[Array[Int]]
    th.pbenchOffWarm(
      "java.util.Arrays.binarySearch vs. Order[Int]"
    )(
        th.Warm(java.util.Arrays.binarySearch(as, 0, as.length, as(17)))
      )(
        th.Warm(Searching.search(as, 0, as.length, as(17)))
      )

    th.pbenchOffWarm(
      "java.util.Arrays.equals vs. Eq[Array[Int]]"
    )(
        th.Warm(java.util.Arrays.equals(as, bs))
      )(
        th.Warm(eq.eqv(as, bs))
      )

    th.pbenchOffWarm(
      "java.util.Arrays.hashCode vs. Hash[Array[Int]]"
    )(
        th.Warm(java.util.Arrays.hashCode(as))
      )(
        th.Warm(hash.hash(as))
      )
  }
  block {
    val as = (0 until 1000).map(_.toLong).toArray
    val bs = (0 until 1000).map(_.toLong).toArray
    def eq = Eq[Array[Long]]
    def hash = Hash[Array[Long]]
    th.pbenchOffWarm(
      "java.util.Arrays.binarySearch vs. Order[Long]"
    )(
        th.Warm(java.util.Arrays.binarySearch(as, 0, as.length, as(17)))
      )(
        th.Warm(Searching.search(as, 0, as.length, as(17)))
      )

    th.pbenchOffWarm(
      "java.util.Arrays.equals vs. Eq[Array[Long]]"
    )(
        th.Warm(java.util.Arrays.equals(as, bs))
      )(
        th.Warm(eq.eqv(as, bs))
      )

    th.pbenchOffWarm(
      "java.util.Arrays.hashCode vs. Hash[Array[Long]]"
    )(
        th.Warm(java.util.Arrays.hashCode(as))
      )(
        th.Warm(hash.hash(as))
      )
  }
}
