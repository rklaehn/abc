package com.rklaehn.abc

import org.scalatest.FunSuite

class ArrayFactoryTest extends FunSuite {
  import ArrayFactory._

  test("singleton direct") {
    assert(singleton(1.toByte).getClass == classOf[Array[Byte]])
    assert(singleton(1.toShort).getClass == classOf[Array[Short]])
    assert(singleton(1.toInt).getClass == classOf[Array[Int]])
    assert(singleton(1.toLong).getClass == classOf[Array[Long]])
    assert(singleton(1.toFloat).getClass == classOf[Array[Float]])
    assert(singleton(1.toDouble).getClass == classOf[Array[Double]])
    assert(singleton('x').getClass == classOf[Array[Char]])
    assert(singleton(true).getClass == classOf[Array[Boolean]])
    assert(singleton("x").getClass == classOf[Array[String]])
  }

  test("singleton indirect") {
    assert(singletonIndirect(1.toByte).getClass == classOf[Array[Byte]])
    assert(singletonIndirect(1.toShort).getClass == classOf[Array[Short]])
    assert(singletonIndirect(1.toInt).getClass == classOf[Array[Int]])
    assert(singletonIndirect(1.toLong).getClass == classOf[Array[Long]])
    assert(singletonIndirect(1.toFloat).getClass == classOf[Array[Float]])
    assert(singletonIndirect(1.toDouble).getClass == classOf[Array[Double]])
    assert(singletonIndirect('x').getClass == classOf[Array[Char]])
    assert(singletonIndirect(true).getClass == classOf[Array[Boolean]])
    assert(singletonIndirect("x").getClass == classOf[Array[String]])
  }

  private def singletonIndirect[T](x: T): Array[T] = singleton(x)

}
