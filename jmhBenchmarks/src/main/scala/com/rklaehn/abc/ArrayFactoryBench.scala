package com.rklaehn.abc

import java.util.concurrent.TimeUnit

import com.rklaehn.sonicreducer.Reducer
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class ArrayFactoryBench {
  import ArrayFactory._

  @Benchmark
  def booleanDirect(x: Blackhole): Unit = x.consume(singleton(true))

  @Benchmark
  def booleanIndirect(x: Blackhole): Unit = x.consume(singleton(java.lang.Boolean.TRUE))

  @Benchmark
  def byteDirect(x: Blackhole): Unit = x.consume(singleton(1.toByte))

  @Benchmark
  def byteIndirect(x: Blackhole): Unit = x.consume(singleton(java.lang.Byte.valueOf(1.toByte)))
}