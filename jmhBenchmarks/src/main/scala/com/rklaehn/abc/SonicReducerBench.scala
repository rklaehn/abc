package com.rklaehn.abc

import java.util.concurrent.TimeUnit

import com.rklaehn.sonicreducer.Reducer
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class SonicReducerBench {

  @Param(Array("1", "10", "100", "1000", "10000"))
  var size = 0

  var data: Array[String] = null

  @Setup
  def setup(): Unit = {
    data = (0 until size).map(_.toString).toArray
  }

  def foldLeft(): String = {
    data.foldLeft("")(_ + _)
  }

  def reduceIterable(): String = {
    Reducer.reduce(data)(_ + _).get
  }

  def reduceArray(): String = {
    Reducer.reduceArray(data)(_ + _).get
  }

  def stringBuilder(): String = {
    val result = new StringBuilder
    for(x <- data)
      result append x
    result.toString()
  }

  @Benchmark
  def foldLeft(x: Blackhole): Unit = x.consume(foldLeft)

  @Benchmark
  def reduceIterable(x: Blackhole): Unit = x.consume(reduceIterable())

  @Benchmark
  def reduceArray(x: Blackhole): Unit = x.consume(reduceArray())

  @Benchmark
  def stringBuilder(x: Blackhole): Unit = x.consume(stringBuilder())
}