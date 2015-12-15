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

  private class CT[@sp A] {
    def ct(a: A): ClassTag[A] = ClassTag.AnyRef.asInstanceOf[ClassTag[A]]
    def mkArray(size: Int): Array[A] = ct(null.asInstanceOf[A]).newArray(size)
  }

  def apply[@sp T](size: Int, value: T = null.asInstanceOf[T]) = (new CT[T] with CTS).mkArray(size)
}

object Main extends App {
  println(ArrayMaker[Byte](10))
  println(ArrayMaker[Short](10))
  println(ArrayMaker[String](10).toIndexedSeq)
//  println(mk[Short])
//  println(mk[Int])
//  println(mk[Long])
//  println(mk[Float])
//  println(mk[Double])
//  println(mk[Unit])
//  println(mk[String])
}