package com.rklaehn.abc

private[abc] trait ArrayFactory {

  final def singleton[@specialized A](a: A): Array[A] = s(a)

  protected def s[@specialized A](a: A): Array[A] = a.asInstanceOf[AnyRef] match {
    case x: java.lang.Byte =>
      Array(x.byteValue()).asInstanceOf[Array[A]]
    case x: java.lang.Short =>
      Array(x.shortValue()).asInstanceOf[Array[A]]
    case x: java.lang.Integer =>
      Array(x.intValue()).asInstanceOf[Array[A]]
    case x: java.lang.Long =>
      Array(x.longValue()).asInstanceOf[Array[A]]
    case x: java.lang.Float =>
      Array(x.floatValue()).asInstanceOf[Array[A]]
    case x: java.lang.Double =>
      Array(x.doubleValue()).asInstanceOf[Array[A]]
    case x: java.lang.Character =>
      Array(x.charValue()).asInstanceOf[Array[A]]
    case x: java.lang.Boolean =>
      Array(x.booleanValue()).asInstanceOf[Array[A]]
    case x =>
      val t = scala.reflect.ClassTag(x.getClass).newArray(1).asInstanceOf[Array[AnyRef]]
      t(0) = x
      t.asInstanceOf[Array[A]]
  }
}

private[abc] trait ArrayFactorySP {
  protected def s$mZc$sp(x: Boolean) = Array(x)
  protected def s$mBc$sp(x: Byte) =    Array(x)
  protected def s$mSc$sp(x: Short) =   Array(x)
  protected def s$mIc$sp(x: Int) =     Array(x)
  protected def s$mJc$sp(x: Long) =    Array(x)
  protected def s$mFc$sp(x: Float) =   Array(x)
  protected def s$mDc$sp(x: Double) =  Array(x)
  protected def s$mCc$sp(x: Char) =    Array(x)
}

object ArrayFactory extends ArrayFactory with ArrayFactorySP {

  private val empty0 = Array.empty[AnyRef]

  def empty[T] = empty0.asInstanceOf[Array[T]]

  def createFromElement[T](size: Int, element: T): Array[T] = {
    val el = element.asInstanceOf[AnyRef]
    if(el ne null)
      java.lang.reflect.Array.newInstance(el.getClass, size).asInstanceOf[Array[T]]
    else
      new Array[Object](size).asInstanceOf[Array[T]]
  }

  def create[T](size: Int, prototype: Array[T]): Array[T] =
    create0[T](size, prototype.getClass)

  private def create0[T](size: Int, cls: Class[_ <: Array[T]]): Array[T] = {
    if(cls eq classOf[Array[Object]])
      new Array[AnyRef](size).asInstanceOf[Array[T]]
    else
      java.lang.reflect.Array.newInstance(cls.getComponentType, size).asInstanceOf[Array[T]]
    /*
    T[] copy = ((Object)newType == (Object)Object[].class)
    ? (T[]) new Object[newLength]
      : (T[]) Array.newInstance(newType.getComponentType(), newLength);
    System.arraycopy(original, 0, copy, 0,
      Math.min(original.length, newLength));
    return copy;
    */
  }
}