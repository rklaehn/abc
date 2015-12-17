package com.rklaehn.abc

object ArrayMaker {

  private trait CTS {
    def ct$mcZ$sp(x: Boolean) = ClassTag.Boolean
    def ct$mcB$sp(x: Byte) = ClassTag.Byte
    def ct$mcS$sp(x: Short) = ClassTag.Short
    def ct$mcI$sp(x: Int) = ClassTag.Int
    def ct$mcJ$sp(x: Long) = ClassTag.Long
    def ct$mcF$sp(x: Float) = ClassTag.Float
    def ct$mcX$sp(x: Double) = ClassTag.Double
    def ct$mcC$sp(x: Byte) = ClassTag.Char
  }

  private class CT[@specialized A] {
    def ct(a: A): ClassTag[A] = throw new UnsupportedOperationException("Can't find a ClassTag")
    def mkArray(size: Int): Array[A] = ct(null.asInstanceOf[A]).newArray(size)
  }

  def apply[@specialized T](size: Int, value: T = null.asInstanceOf[T]) = (new CT[T] with CTS).mkArray(size)
}
