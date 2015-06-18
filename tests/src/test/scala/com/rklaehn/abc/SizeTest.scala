package com.rklaehn.abc

import org.github.jamm.MemoryMeter
import org.junit.Assert._
import org.junit.Test
import spire.algebra.Eq
import spire.implicits._

import scala.util.hashing.Hashing

class SizeTest {

  /*

    val mm = new MemoryMeter()

    val overhead = mm.measure(new java.lang.Object)
    val pointerSize = (mm.measure(new Array[java.lang.Object](256)) - mm.measure(new Array[java.lang.Object](128))) / 128
    val segment = new Segment(0L,0L,0L)
    val segmentSize = mm.measure(new Segment(0L,0L,0L))
    val branchSize = mm.measure(new Branch(0L, 0, segment, segment))
    println("Testing constant profile object sizes")
    println(s"Pointer size is $pointerSize")
    println(s"Object overhead is $overhead")
    println(s"Segment size is $segmentSize")
    println(s"Branch size is $branchSize")
    assertEquals(24, segmentSize - overhead) // prefix, at and above = 3 * 8 bytes
    assertEquals(24, branchSize - overhead) // prefix, level, hash, left, right = 8 + 1 + 4 + 4 + 4 = 24 bytes
   */

  val mm = new MemoryMeter()
  val overhead = mm.measure(new java.lang.Object)
  val pointerSize = (mm.measure(new Array[java.lang.Object](256)) - mm.measure(new Array[java.lang.Object](128))) / 128

  @Test
  def testIntArraySet(): Unit = {
    val es = (0 until 100).toArray
    val a = ArraySet(es: _*)
    val b = Set(es: _*)
    println("Elements: " + mm.measureDeep(es))
    println("ArraySet: " + mm.measureDeep(a))
    println("Set:      " + mm.measureDeep(b))
  }

  @Test
  def testStringArraySet(): Unit = {
    val es = (0 until 100).map(_.toString).toArray
    val a = ArraySet(es: _*)
    val b = Set(es: _*)

    println("Elements: " + mm.measureDeep(es))
    println("ArraySet: " + mm.measureDeep(a))
    println("Set:      " + mm.measureDeep(b))
  }

  @Test
  def testIntArrayMap(): Unit = {
    val es = (0 until 100).map(i => i -> i).toArray
    val a = ArrayMap(es: _*)
    val b = Map(es: _*)
    println("ArrayMap: " + mm.measureDeep(a))
    println("Map:      " + mm.measureDeep(b))
  }

  @Test
  def testRadixTreeUnit(): Unit = {

    implicit object UnitEq extends Eq[Unit] {

      override def eqv(x: Unit, y: Unit): Boolean = true
    }

    implicit object EqHashing extends Hashing[Unit] {
      override def hash(x: Unit): Int = 0
    }

    val names = (0 until 10000).map(x => x.toString -> (())).toArray
    val tree1 = RadixTree(names: _*)
    val tree2 = tree1.packed
    println("Elements:           " + mm.measureDeep(names))
    println("RadixTree:          " + mm.measureDeep(tree1))
    println("RadixTree (packed): " + mm.measureDeep(tree2))
  }
}
