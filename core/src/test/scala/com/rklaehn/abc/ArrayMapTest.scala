package com.rklaehn.abc

import algebra.ring.AdditiveGroup
import cats.Show
import cats.implicits._
import algebra.Eq
import org.scalatest.FunSuite

class ArrayMapTest extends FunSuite {

  test("instances") {
    implicitly[Eq[ArrayMap[Int, Int]]]
    implicitly[Show[ArrayMap[Int, Int]]]
    implicitly[Hash[ArrayMap[Int, Int]]]
  }

  test("apply") {
    intercept[NoSuchElementException] {
      ArrayMap(1 → 1).apply(2)
    }
  }

  test("toString") {
    assert(!ArrayMap.empty[Int, Int].toString.isEmpty)
  }

  test("show") {
    assert(!ArrayMap.empty[Int, Int].show.isEmpty)
  }
//
//  test("withDefaultValue") {
//    import algebra.std.all._
//    assert(ArrayMap(1 → 1).withDefaultValue(2).apply(-1) == 2)
//
////     val x = ArrayMap(-2147483648->0,-1363047361->124517748,-1338566520 -> -917420365,-1101480300-> -2147483648,-1-> -2147483648,0-> -604511429,495446364-> -300782046,646665741-> -1362120462,781958031->1776044249,1179334478-> -2147483648,1258794322->2147483647,1811071856->1365599478,1960593243-> -1,1978923550-> -649448160,2147483647->2038441776).withDefaultValue(1)
////    val x = ArrayMap(-2147483648->0,-1363047361->124517748,-1338566520 -> -917420365,-1101480300-> -2147483648,-1-> -2147483648,0-> -604511429,495446364-> -300782046,646665741-> -1362120462,781958031->1776044249,1179334478-> -2147483648,1258794322->2147483647,1811071856->1365599478,1960593243-> -1,1978923550-> -649448160).withDefaultValue(1)
//    val x = ArrayMap(1960593243-> -1).withDefaultValue(1)
//    val neg = AdditiveGroup.negate(x)
//    println(x)
//    println(neg)
//    for(k ← x.keys.iterator)
//      if(x(k) != -neg(k)) {
//        println(k)
//        assert(x(k) == -neg(k))
//      }
//
//    assert(AdditiveGroup.isZero(AdditiveGroup.plus(AdditiveGroup.negate(x), x)))
//  }
}
