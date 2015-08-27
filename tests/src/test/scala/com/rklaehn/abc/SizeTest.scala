package com.rklaehn.abc

import org.github.jamm.MemoryMeter
import org.junit.Assert._
import org.junit.Test
import spire.algebra.Eq
import spire.implicits._

import scala.io.Source
import scala.util.hashing.Hashing
import spire.optional.genericEq._

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

  lazy val mm = new MemoryMeter()
  lazy val overhead = mm.measure(new java.lang.Object)
  lazy val pointerSize = (mm.measure(new Array[java.lang.Object](256)) - mm.measure(new Array[java.lang.Object](128))) / 128

  def measureElements[K](xs: Seq[K]): Long =
    xs.foldLeft(0L) { case (sum, x) ⇒ sum + mm.measureDeep(x) }

  @Test
  def testIntArraySet(): Unit = {
    val es = (0 until 100).toArray
    val a = ArraySet(es: _*)
    val b = Set(es: _*)
    val payload = mm.measureDeep(es)
    println("Set[Int] 100")
    println("Elements: " + payload)
    println("ArraySet: " + (mm.measureDeep(a) - payload))
    println("Set:      " + (mm.measureDeep(b) - payload))
  }

  @Test
  def testStringArraySet(): Unit = {
    val es = (0 until 100).map(_.toString).toArray
    val a = ArraySet(es: _*)
    val b = Set(es: _*)
    val payload = mm.measureDeep(es)
    println("Set[String] 100")
    println("Elements: " + payload)
    println("ArraySet: " + (mm.measureDeep(a) - payload))
    println("Set:      " + (mm.measureDeep(b) - payload))
  }

  @Test
  def testIntArrayMap(): Unit = {
    val ks = (0 until 100).toArray
    val vs = ks
    val a = ArrayMap(ks zip vs: _*)
    val b = Map(ks zip vs: _*)

    val payload = mm.measureDeep(ks) + mm.measureDeep(vs)
    println("Map[Int, Int] 100")
    println("Elements: " + payload)
    println("ArraySet: " + (mm.measureDeep(a) - payload))
    println("Set:      " + (mm.measureDeep(b) - payload))
  }

  @Test
  def testIntArrayMapLarge(): Unit = {
    val ks = (0 until 100000).toArray
    val vs = ks
    val a = ArrayMap(ks zip vs: _*)
    val b = Map(ks zip vs: _*)

    val payload = mm.measureDeep(ks) + mm.measureDeep(vs)
    println("Map[Int, Int] 100")
    println("Elements: " + payload)
    println("ArraySet: " + (mm.measureDeep(a) - payload))
    println("Set:      " + (mm.measureDeep(b) - payload))
  }

  @Test
  def testIntStringArrayMap(): Unit = {
    val ks = (0 until 100).toArray
    val vs = ks.map(_.toString)
    val a = ArrayMap(ks zip vs: _*)
    val b = Map(ks zip vs: _*)
    val payload = mm.measureDeep(ks) + measureElements(vs)
    println("Map[Int, String] 100")
    println("Elements: " + payload)
    println("ArrayMap: " + (mm.measureDeep(a) - payload))
    println("Map:      " + (mm.measureDeep(b) - payload))
  }

  @Test
  def testStringArrayMap(): Unit = {
    val ks = (0 until 100).map(_.toString).toArray
    val vs = (0 until 100).map(_.toString).toArray
    val a = ArrayMap(ks zip vs: _*)
    val b = Map(ks zip vs: _*)
    println(b.size)

    val payload = mm.measureDeep(ks) + mm.measureDeep(vs)
    println("Map[String, String] 100")
    println("Elements: " + payload)
    println("ArrayMap: " + (mm.measureDeep(a) - payload))
    println("Map:      " + (mm.measureDeep(b) - payload))
  }

  @Test
  def testIntIntMultiMap(): Unit = {
    val ks = (100000 until 101000).toArray
    val vs = (100000 until 101000).toArray
    val a = ArrayMultiMap[Int, Int](ks zip vs.map(x => ArraySet(0 until 100: _*)): _*)
    val b = Map[Int, Set[Int]](ks zip vs.map(x => Set(0 until 100: _*)): _*)

    val payload = mm.measureDeep(ks) + mm.measureDeep(vs) * 100
    println("MultiMap[Int, Int] 100*100")
    println("Elements: " + payload)
    println("ArraySet: " + (mm.measureDeep(a) - payload))
    println("Set:      " + (mm.measureDeep(b) - payload))
  }

  @Test
  def testRadixTreeUnit(): Unit = {

    val names = (0 until 10000).map(x => x.toString -> (())).toArray
    val tree1 = RadixTree(names: _*)
    val tree2 = tree1.packed
    println("Elements:           " + mm.measureDeep(names))
    println("RadixTree:          " + mm.measureDeep(tree1))
    println("RadixTree (packed): " + mm.measureDeep(tree2))
  }
//
//  @Test
//  def testRadixTreeUnit2(): Unit = {
//    val names = Source.fromURL("http://www-01.sil.org/linguistics/wordlists/english/wordlist/wordsEn.txt").getLines.toArray
//    val set = Set(names: _*)
//
//    val tree1 = RadixTree(names.map(s => s -> (())): _*)
//    val tree2 = tree1.packed
//
//    val tree3 = RadixTree(names.map(s => s.toCharArray -> (())): _*)
//    val tree4 = tree3.packed
//
//    val tree5 = RadixTree(names.map(s => s.getBytes("UTF-8") -> (())): _*)
//    val tree6 = tree5.packed
//
//    println(s"List of ${names.length} english words")
//    println("Elements:           " + mm.measureDeep(names))
//
//    println("RadixTree:          " + mm.measureDeep(tree1))
//    println("RadixTree (packed): " + mm.measureDeep(tree2))
//
//    println("RadixTree (Chars):  " + mm.measureDeep(tree3))
//    println("RadixTree (packed): " + mm.measureDeep(tree4))
//
//    println("RadixTree (Bytes):  " + mm.measureDeep(tree5))
//    println("RadixTree (packed): " + mm.measureDeep(tree6))
//
//    println("Set:                " + mm.measureDeep(set))
//  }
}
