package com.rklaehn.abc

import org.junit.Assert._
import org.junit.Test
import DebugUtil._
import algebra.std.all._
import Instances._

class SpecializeTest {

  import scala.util.hashing.Hashing._
  import OrderedArrayTag.generic

  @Test
  def testSeqSpecialization(): Unit = {
    assertTrue(ArraySeq.empty[Int].isSpecialized)
    assertTrue(ArraySeq.singleton(1).isSpecialized)
    assertTrue(ArraySeq(1,2,3).isSpecialized)
    assertTrue(ArraySeq(1,2,3).concat(ArraySeq(1,2,3)).isSpecialized)
  }

  @Test
  def testSetSpecialization(): Unit = {
    assertTrue(ArraySet.empty[Int].isSpecialized)
    assertTrue(ArraySet.singleton(1).isSpecialized)
    assertTrue(ArraySet(1,2,3).isSpecialized)
    assertTrue(ArraySet(1,2,3).union(ArraySet(3,4,5)).isSpecialized)
    assertTrue(ArraySet(1,2,3).intersect(ArraySet(3,4,5)).isSpecialized)
    assertTrue(ArraySet(1,2,3).diff(ArraySet(3,4,5)).isSpecialized)
    assertTrue(ArraySet(1,2,3).xor(ArraySet(3,4,5)).isSpecialized)
    assertTrue(ArraySet(1,2,3).asArraySeq.isSpecialized)
  }

  @Test
  def testMapSpecialization(): Unit = {
    // for an int/int map we expect full specialization
    assertTrue(ArrayMap.empty[Int, Int].isSpecialized)
    assertTrue(ArrayMap.singleton(1, 1).isSpecialized)
    assertTrue(ArrayMap(1 → 1, 2 → 2).isSpecialized)
    assertTrue(ArrayMap(1 → 1, 2 → 2).keys.isSpecialized)
    assertTrue(ArrayMap(1 → 1, 2 → 2).values.isSpecialized)
    assertTrue(ArrayMap(1 → 1, 2 → 2).justKeys(ArraySet(1)).isSpecialized)
    assertTrue(ArrayMap(1 → 1, 2 → 2).exceptKeys(ArraySet(1)).isSpecialized)

    // for a mixed primitive/anyref map there is no specialization (partial specialization does not work).
    // but we can still use a primitive array for the key array because we have the class tag
    assertTrue(ArrayMap.empty[Int, String].keys0.isIntArray)
    assertTrue(ArrayMap.singleton(1, "1").keys0.isIntArray)
    assertTrue(ArrayMap(1 -> "1", 2 -> "2").keys0.isIntArray)
    assertTrue(ArrayMap(1 -> "1", 2 -> "2").justKeys(ArraySet(1)).keys0.isIntArray)
    assertTrue(ArrayMap(1 -> "1", 2 -> "2").exceptKeys(ArraySet(1)).keys0.isIntArray)
  }

  @Test
  def testMultiMapSpecialization(): Unit = {
    assertTrue(ArrayMultiMap.empty[Int, Int].isSpecialized)
    assertTrue(ArrayMultiMap.singleton(1, ArraySet(1, 2)).isSpecialized)
    assertTrue(ArrayMultiMap.fromKVs(1 → 1, 1 → 2).isSpecialized)
    assertTrue(ArrayMultiMap.fromKVs(1 → 1, 1 → 2).apply(1).isSpecialized)
    assertTrue(ArrayMultiMap.fromKVs(1 → 1, 1 → 2).apply(1).elements.isIntArray)
    assertTrue(ArrayMultiMap.fromKVs(1 → 1, 1 → 2).map.keys0.isIntArray)
  }

  @Test
  def testBiMapSpecialization(): Unit = {
    assertTrue(ArrayBiMap.empty[Int, Int].isSpecialized)
    assertTrue(ArrayBiMap.singleton(0, 0).isSpecialized)
    assertTrue(ArrayBiMap(1 → 1, 1 → 2).isSpecialized)
    assertTrue(ArrayBiMap(1 → 1, 1 → 2).kv.isSpecialized)
    assertTrue(ArrayBiMap(1 → 1, 1 → 2).vk.isSpecialized)

    // for a mixed primitive/anyref map there is no specialization (partial specialization does not work).
    // but we can still use a primitive array for the key array because we have the class tag
    assertTrue(ArrayBiMap.empty[Int, String].kv.keys0.isIntArray)
    assertTrue(ArrayBiMap.singleton(0, "0").kv.keys0.isIntArray)
    assertTrue(ArrayBiMap(1 → "1", 1 → "2").kv.keys0.isIntArray)
    assertTrue(ArrayBiMap(1 → "1", 1 → "2").vk.values0.isIntArray)
  }
}
