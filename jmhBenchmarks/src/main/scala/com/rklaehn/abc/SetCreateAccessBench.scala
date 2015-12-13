package com.rklaehn.abc

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import algebra.std.all._
import ScalaCollectionConverters._

import scala.collection.immutable.{HashSet, SortedSet}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class SetCreateAccessBench {

  @Param(Array("1", "10", "100", "1000", "10000", "100000"))
  var size = 0

  @Param(Array("0.0", "0.5", "1.0"))
  var offset = 0.0

  @Param(Array("arrayset", "hashset", "sortedset"))
  var kind = ""

  var k: Int = 0
  var a: Set[Int] = _
  var b: Set[Int] = _

  @Setup
  def setup(): Unit = {
    def make(elements: Seq[Int]): Set[Int] = kind match {
      case "arrayset" ⇒ ArraySet(elements: _*).asCollection
      case "hashset" ⇒ HashSet(elements: _*)
      case "sortedset" ⇒ SortedSet(elements: _*)
    }
    k = (offset * size).toInt
    a = make(0 until size)
    b = make(k until (k + size))
  }


  @Benchmark
  def union(x: Blackhole): Unit = {
    x.consume(a union b)
  }

  @Benchmark
  def intersect(x: Blackhole): Unit = {
    x.consume(a intersect b)
  }

  @Benchmark
  def diff(x: Blackhole): Unit = {
    x.consume(a diff b)
  }

  @Benchmark
  def subsetOf(x: Blackhole): Unit = {
    x.consume(a subsetOf b)
  }

  @Benchmark
  def filter(x: Blackhole): Unit = {
    x.consume(a filter(_ < k))
  }
}
