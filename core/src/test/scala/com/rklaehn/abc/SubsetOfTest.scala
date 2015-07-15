package com.rklaehn.abc

import org.junit.Assert._
import org.junit.Test

class SubsetOfTest {

  @Test
  def testSubsetOf(): Unit = {
    val a = ArraySet(0 until 1000 :_*)
    val b = ArraySet(500 until 1500 :_*)
    val r = a.subsetOf(b)
    assertFalse(r)
  }
}
