package com.rklaehn.abc

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
 import cats.kernel.instances.all._
import ScalaCollectionConverters._

import scala.collection.immutable.{HashSet, SortedSet}

sealed trait SetElementBenchOps {
  def containsTrue: Any
  def containsFalse: Any
}

object SetElementBenchOps extends BenchUtil {

  def apply(a: Seq[Int], c: Int, n: Int, kind: String) = {
    val a1 = a.map(mix)
    val c1 = mix(c)
    val n1 = mix(n)
    require(a1.length == a.length)
    kind match {
      case "hashset" => ScalaCollectionBench(HashSet(a1: _*), c1, n1)
      case "sortedset" => ScalaCollectionBench(SortedSet(a1: _*), c1, n1)
      case "arrayset" => TypeClassBench(ArraySet(a1: _*), c1, n1)
      case "arrayset2" => ScalaCollectionBench(ArraySet(a1: _*).asCollection, c1, n1)
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

  @Param(Array("1", "10", "100", "1000", "10000", "100000"))
  var size = 0

  @Param(Array("arrayset", "hashset", "sortedset")) //, "arrayset2"))
  var kind = ""

  var k: Int = 0
  var bench: SetElementBenchOps = _

  @Setup
  def setup(): Unit = {
    val c = (0.3 * size).toInt // a value that is contained in the set
    val n = (1.3 * size).toInt // a value that is not contained in the set
    bench = SetElementBenchOps(0 until size, c, n, kind)
  }

  @Benchmark
  def containsFalse(x: Blackhole): Unit = x.consume(bench.containsFalse)

  @Benchmark
  def containsTrue(x: Blackhole): Unit = x.consume(bench.containsTrue)
}
