package com.rklaehn.abc

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
 import cats.kernel.instances.all._

import scala.collection.immutable.{HashSet, SortedSet}

sealed trait SetCreateBenchOps {
  def createBulk: Any
  def createElements: Any
}

object SetCreateBenchOps extends BenchUtil {

  def apply(a: Seq[Int], kind: String) = {
    val a1 = a.map(mix).toArray
    require(a1.length == a.length)
    kind match {
      case "hashset" => ScalaCollectionBench(a1, x => HashSet.apply(x: _*))
      case "sortedset" => ScalaCollectionBench(a1, x => SortedSet.apply(x: _*))
      case "arrayset" => TypeClassBench(a1)
      case "arrayset2" => ???
    }
  }

  private final case class ScalaCollectionBench(a: Array[Int], f: Array[Int] => Any) extends SetCreateBenchOps {
    override def createBulk: Any = f(a)
    override def createElements: Any = f(a)
  }

  private final case class TypeClassBench(a: Array[Int]) extends SetCreateBenchOps {
    override def createBulk: Any = {
      ArraySet(a: _*)
    }
    override def createElements: Any = {
      a.foldLeft(ArraySet.empty[Int])(_ + _)
    }
  }
}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class SetCreateBench {

  @Param(Array("1", "10", "100", "1000", "10000", "100000"))
  var size = 0

  @Param(Array("arrayset", "hashset", "sortedset")) //, "arrayset2"))
  var kind = ""

  var bench: SetCreateBenchOps = _

  @Setup
  def setup(): Unit = {
    bench = SetCreateBenchOps(0 until size, kind)
  }

  @Benchmark
  def createBulk(x: Blackhole): Unit = x.consume(bench.createBulk)

  @Benchmark
  def createElements(x: Blackhole): Unit = x.consume(bench.createElements)
}
