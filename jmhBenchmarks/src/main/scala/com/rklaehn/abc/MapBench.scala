//package com.rklaehn.abc
//
//import java.util.concurrent.TimeUnit
//
//import org.openjdk.jmh.annotations._
//import org.openjdk.jmh.infra.Blackhole
//
//import scala.collection.immutable.{SortedMap, HashMap, HashSet, SortedSet}
//
//object MapBench {
//
//  sealed trait Bench {
//    def union: Any
//    def intersect: Any
//    def diff: Any
//    def subsetOf: Boolean
//    def filter(f: Int => Boolean): Any
//  }
//
//  object Bench {
//    def apply(a: Seq[(Int, String)], b: Seq[(Int, String)], kind: String) = {
//      kind match {
//        case "hashmap" => ScalaCollectionBench(ArrayMap(a: _*), ArrayMap(b: _*))
//        case "sortedmap" => ScalaCollectionBench(HashMap(a: _*), HashMap(b: _*))
//        case "arraymap" => ScalaCollectionBench(SortedMap(a: _*), SortedMap(b: _*))
//      }
//    }
//  }
//
//  private final case class ScalaCollectionBench(a: Map[Int, String], b: Map[Int, String]) extends Bench {
//    override def union: Any = a union b
//    override def diff: Any = a diff b
//    override def subsetOf: Boolean = a subsetOf b
//    override def intersect: Any = a intersect b
//    override def filter(f: (Int) => Boolean): Any = a filter f
//  }
//
//  private final case class TypeClassBench(a: ArraySet2[Int], b: ArraySet2[Int]) extends Bench {
//    override def union: Any = a union b
//    override def diff: Any = a diff b
//    override def subsetOf: Boolean = a subsetOf b
//    override def intersect: Any = a intersect b
//    override def filter(f: (Int) => Boolean): Any = a filter f
//  }
//}
//
//@BenchmarkMode(Array(Mode.AverageTime))
//@OutputTimeUnit(TimeUnit.NANOSECONDS)
//@State(Scope.Thread)
//class MapBench {
//  import MapBench._
//
//  @Param(Array("1", "10", "100", "1000", "10000", "100000"))
//  var size = 0
//
//  @Param(Array("0.0", "0.5", "1.0"))
//  var offset = 0.0
//
//  @Param(Array("arraymap", "hashmap", "sortedmap"))
//  var kind = ""
//
//  var k: Int = 0
//  var bench: Bench = _
//
//  @Setup
//  def setup(): Unit = {
//    k = (offset * size).toInt
//    bench = Bench(0 until size, k until (k + size), kind)
//  }
//
//
//  @Benchmark
//  def union(x: Blackhole): Unit = {
//    x.consume(bench.union)
//  }
//
//  @Benchmark
//  def intersect(x: Blackhole): Unit = {
//    x.consume(bench.intersect)
//  }
//
//  @Benchmark
//  def diff(x: Blackhole): Unit = {
//    x.consume(bench.diff)
//  }
//
//  @Benchmark
//  def subsetOf(x: Blackhole): Unit = {
//    x.consume(bench.subsetOf)
//  }
//
//  @Benchmark
//  def filter(x: Blackhole): Unit = {
//    x.consume(bench.filter(_ < k))
//  }
//}
