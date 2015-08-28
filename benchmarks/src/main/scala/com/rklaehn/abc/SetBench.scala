package com.rklaehn.abc

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.collection.immutable.{HashSet, SortedSet}

sealed trait SetBenchOps {
  def union: Any
  def intersect: Any
  def diff: Any
  def subsetOf: Boolean
  def contains: Any
  def filter(f: Int => Boolean): Any
}

object SetBenchOps {

  def apply(a: Seq[Int], b: Seq[Int], kind: String) = {
    kind match {
      case "arrayset" => ScalaCollectionBench(ArraySet(a: _*).asCollection, ArraySet(b: _*).asCollection)
      case "hashset" => ScalaCollectionBench(HashSet(a: _*), HashSet(b: _*))
      case "sortedset" => ScalaCollectionBench(SortedSet(a: _*), SortedSet(b: _*))
      case "arrayset2" => TypeClassBench(ArraySet(a: _*), ArraySet(b: _*))
    }
  }

  private final case class ScalaCollectionBench(a: Set[Int], b: Set[Int]) extends SetBenchOps {
    override def union: Any = a union b
    override def diff: Any = a diff b
    override def subsetOf: Boolean = a subsetOf b
    override def intersect: Any = a intersect b
    override def contains = for(be <- b.iterator) a.contains(be)
    override def filter(f: (Int) => Boolean): Any = a filter f
  }

  private final case class TypeClassBench(a: ArraySet[Int], b: ArraySet[Int]) extends SetBenchOps {
    override def union: Any = a union b
    override def diff: Any = a diff b
    override def subsetOf: Boolean = a subsetOf b
    override def intersect: Any = a intersect b
    override def contains = for(be <- b.iterator) a.contains(be)
    override def filter(f: (Int) => Boolean): Any = a filter f
  }
}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class SetBench {

  @Param(Array("1", "10", "100", "1000", "10000", "100000"))
  var size = 0

  @Param(Array("0.0", "0.5", "1.0"))
  var offset = 0.0

  @Param(Array("arrayset", "arrayset2", "hashset", "sortedset"))
  var kind = ""

  var k: Int = 0
  var bench: SetBenchOps = _

  @Setup
  def setup(): Unit = {
    k = (offset * size).toInt
    bench = SetBenchOps(0 until size, k until (k + size), kind)
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
    x.consume(bench.filter(_ < k))
  }
}
