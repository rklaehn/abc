package com.rklaehn.abc

import java.util.concurrent.TimeUnit

import algebra.Order
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import algebra.std.all._
import ScalaCollectionConverters._

import scala.collection.immutable.{HashSet, SortedSet}

sealed trait SetElementBenchOps {
  def containsTrue: Any
  def containsFalse: Any
}

//object IntOrder extends Order[Int] {
//  override def compare(x: Int, y: Int): Int = {
//    println("foo")
//    Integer.compare(x, y)
//  }
//}

object SetElementBenchOps {

  def apply(a: Seq[Int], c: Int, n: Int, kind: String) = {
    kind match {
      case "hashset" => ScalaCollectionBench(HashSet(a: _*), c, n)
      case "sortedset" => ScalaCollectionBench(SortedSet(a: _*), c, n)
      case "arrayset" => TypeClassBench(ArraySet(a: _*), c, n)
      case "arrayset2" => ScalaCollectionBench(ArraySet(a: _*).asCollection, c, n)
    }
  }

  private final case class ScalaCollectionBench(a: Set[Int], c: Int, n: Int) extends SetElementBenchOps {
    override def containsTrue: Boolean = a.contains(c)
    override def containsFalse: Boolean = a.contains(n)
  }

  private final case class TypeClassBench(a: ArraySet[Int], c: Int, n: Int) extends SetElementBenchOps {
    override def containsTrue: Boolean = a.contains(c)
    override def containsFalse: Boolean = a.contains(n)
  }
}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class SetElementBench {

  @Param(Array("1", "10", "100", "1000", "10000"))
  var size = 0

  @Param(Array("arrayset", "arrayset2", "hashset", "sortedset"))
  var kind = ""

  var k: Int = 0
  var bench: SetElementBenchOps = _

  @Setup
  def setup(): Unit = {
    val shift = 1000000 // so we don't get the cached java.lang.Integer instances
    val c = (0.3 * size).toInt // a value that is contained in the set
    val n = (1.3 * size).toInt // a value that is not contained in the set
    bench = SetElementBenchOps(shift until (shift + size), c + shift, n + shift, kind)
  }

  @Benchmark
  def containsFalse(x: Blackhole): Unit = x.consume(bench.containsFalse)

  @Benchmark
  def containsTrue(x: Blackhole): Unit = x.consume(bench.containsTrue)
}
