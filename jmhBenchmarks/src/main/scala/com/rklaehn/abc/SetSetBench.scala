package com.rklaehn.abc

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import algebra.std.all._
import ScalaCollectionConverters._

import scala.collection.immutable.{HashSet, SortedSet}
import scala.util.hashing.MurmurHash3

sealed trait SetSetBenchOps {
  def union: Any
  def intersect: Any
  def diff: Any
  def subsetOf: Boolean
  def filter(f: Int => Boolean): Any
}

object SetSetBenchOps {

  def mix(x: Int): Int = MurmurHash3.mix(0, x)

  def apply(a: Seq[Int], b: Seq[Int], kind: String) = {
    val a1 = a.map(mix)
    val b1 = b.map(mix)
    kind match {
      case "hashset" => ScalaCollectionBench(HashSet(a1: _*), HashSet(b1: _*))
      case "sortedset" => ScalaCollectionBench(SortedSet(a1: _*), SortedSet(b1: _*))
      case "arrayset" => TypeClassBench(ArraySet(a1: _*), ArraySet(b1: _*))
      case "arrayset2" => ScalaCollectionBench(ArraySet(a1: _*).asCollection, ArraySet(b1: _*).asCollection)
    }
  }

  private final case class ScalaCollectionBench(a: Set[Int], b: Set[Int]) extends SetSetBenchOps {
    override def union: Any = a union b
    override def diff: Any = a diff b
    override def subsetOf: Boolean = a subsetOf b
    override def intersect: Any = a intersect b
    override def filter(f: (Int) => Boolean): Any = a filter f
  }

  private final case class TypeClassBench(a: ArraySet[Int], b: ArraySet[Int]) extends SetSetBenchOps {
    override def union: Any = a union b
    override def diff: Any = a diff b
    override def subsetOf: Boolean = a subsetOf b
    override def intersect: Any = a intersect b
    override def filter(f: (Int) => Boolean): Any = a filter f
  }
}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class SetSetBench {

  @Param(Array("1", "10", "100", "1000", "10000", "100000"))
  var size = 0

  @Param(Array("0.0", "0.5", "1.0"))
  var offset = 0.0

  @Param(Array("arrayset", "hashset", "sortedset")) //, "arrayset2"))
  var kind = ""

  var k: Int = 0
  var bench: SetSetBenchOps = _

  val shift = 1000000 // so we don't get the cached java.lang.Integer instances

  @Setup
  def setup(): Unit = {
    k = (offset * size).toInt
    bench = SetSetBenchOps(shift until (shift + size), (shift + k) until (shift + k + size), kind)
  }


  @Benchmark
  def union(x: Blackhole): Unit = {
    x.consume(bench.union)
  }

  @Benchmark
  def intersect(x: Blackhole): Unit = {
    x.consume(bench.intersect)
  }

  @Benchmark
  def diff(x: Blackhole): Unit = {
    x.consume(bench.diff)
  }

  @Benchmark
  def subsetOf(x: Blackhole): Unit = {
    x.consume(bench.subsetOf)
  }

  @Benchmark
  def filter(x: Blackhole): Unit = {
    x.consume(bench.filter(_ < k + shift))
  }
}
