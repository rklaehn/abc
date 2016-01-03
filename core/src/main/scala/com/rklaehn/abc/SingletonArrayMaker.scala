package com.rklaehn.abc

private[abc] sealed abstract class Maker0 {
  def apply[@specialized T](value: T): Array[T] = {
    val r = value.asInstanceOf[Any] match {
      case x: Int => Array(x).asInstanceOf[Array[T]]
      case x: Long => Array(x).asInstanceOf[Array[T]]
      case x: Double => Array(x).asInstanceOf[Array[T]]
      case x =>
        val t = java.lang.reflect.Array.newInstance(x.getClass, 1).asInstanceOf[Array[T]]
        t(0) = value
        t.asInstanceOf[Array[T]]
    }
    r
  }
}

private[abc] sealed trait MakerS {
  def apply$mcZ$sp(x: Boolean) = Array(x)
  def apply$mcB$sp(x: Byte) = Array(x)
  def apply$mcS$sp(x: Short) = Array(x)
  def apply$mcI$sp(x: Int) = Array(x)
  def apply$mcJ$sp(x: Long) = Array(x)
  def apply$mcF$sp(x: Float) = Array(x)
  def apply$mcX$sp(x: Double) = Array(x)
  def apply$mcC$sp(x: Byte) = Array(x)
}

private[abc] object primitiveArray extends Maker0 with MakerS
