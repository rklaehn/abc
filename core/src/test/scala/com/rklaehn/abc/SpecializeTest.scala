package com.rklaehn.abc

import spire.implicits._
import org.junit.Assert._
import org.junit.Test
import DebugUtil._
import spire.optional.genericEq._

class SpecializeTest {

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
    assertTrue(ArraySet(1,2,3).intersection(ArraySet(3,4,5)).isSpecialized)
    assertTrue(ArraySet(1,2,3).diff(ArraySet(3,4,5)).isSpecialized)
    assertTrue(ArraySet(1,2,3).xor(ArraySet(3,4,5)).isSpecialized)
    assertTrue(ArraySet(1,2,3).asArraySeq.isSpecialized)
  }

  @Test
  def testMapSpecialization(): Unit = {
    assertTrue(ArrayMap.empty[Int, Int].isSpecialized)
    assertTrue(ArrayMap.singleton(1, 1).isSpecialized)
    assertTrue(ArrayMap(1 → 1, 2 → 2).isSpecialized)
    assertTrue(ArrayMap(1 → 1, 2 → 2).keys.isSpecialized)
    assertTrue(ArrayMap(1 → 1, 2 → 2).values.isSpecialized)
    assertTrue(ArrayMap(1 → 1, 2 → 2).merge(ArrayMap(3 → 3, 4 → 4)).isSpecialized)
    assertTrue(ArrayMap(1 → 1, 2 → 2).filterKeys(ArraySet(1)).isSpecialized)
    assertTrue(ArrayMap(1 → 1, 2 → 2).filterNotKeys(ArraySet(1)).isSpecialized)
  }

  @Test
  def testMultiMapSpecialization(): Unit = {
    assertTrue(ArrayMultiMap.empty[Int, Int].isSpecialized)
    assertTrue(ArrayMultiMap.singleton(1, ArraySet(1, 2)).isSpecialized)
    assertTrue(ArrayMultiMap.fromKVs(1 → 1, 1 → 2).isSpecialized)
    assertTrue(ArrayMultiMap.fromKVs(1 → 1, 1 → 2).apply(1).isSpecialized)
  }
}
