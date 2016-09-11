package com.rklaehn.abc

import org.github.jamm.MemoryMeter
 import cats.kernel.instances.all._
import org.scalatest.FunSuite

class SizeTest extends FunSuite {

  lazy val mm = new MemoryMeter()
  lazy val overhead = mm.measure(new java.lang.Object)
  lazy val pointerSize = (mm.measure(new Array[java.lang.Object](256)) - mm.measure(new Array[java.lang.Object](128))) / 128

  def measureElements[K](xs: Seq[K]): Long =
    xs.foldLeft(0L) { case (sum, x) ⇒ sum + mm.measureDeep(x) }

  test("intArraySet") {
    val es = (0 until 100).toArray
    val a = ArraySet(es: _*)
    val b = Set(es: _*)
    val payload = mm.measureDeep(es)
    println("Set[Int] 100")
    println("Elements: " + payload)
    println("ArraySet: " + (mm.measureDeep(a) - payload))
    println("Set:      " + (mm.measureDeep(b) - payload))
  }

  test("stringArraySet") {
    val es = (0 until 100).map(_.toString).toArray
    val a = ArraySet(es: _*)
    val b = Set(es: _*)
    val payload = mm.measureDeep(es)
    println("Set[String] 100")
    println("Elements: " + payload)
    println("ArraySet: " + (mm.measureDeep(a) - payload))
    println("Set:      " + (mm.measureDeep(b) - payload))
  }

  test("intArrayMap") {
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

  test("intArrayMapLarge") {
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

  test("intStringArrayMap") {
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

  test("stringArrayMap") {
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

  test("intIntMultiMap") {
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
}
