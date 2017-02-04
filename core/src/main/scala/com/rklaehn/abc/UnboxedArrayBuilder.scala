package com.rklaehn.abc

private[abc] trait UnboxedArrayBuilder[T] {

  def size: Int

  private var ri: Int = 0

  private var r: Array[T] = null

  def result: Array[T] = {
    if(r eq null)
      ArrayFactory.empty[T]
    else
      r.resizeInPlace(ri)
  }

  def add(value: T): Unit = {
    copyFrom(ArrayFactory.singleton(value), 0, 1)
  }

  def copyFrom(s: Array[T], si: Int, n: Int): Unit = if(n != 0) {
    if(r eq null)
      r = ArrayFactory.create(size, s)
    try {
      System.arraycopy(s, si, r, ri, n)
    } catch {
      case _: ArrayStoreException =>
        val r0 = r
        r = new Array[AnyRef](size).asInstanceOf[Array[T]]
        System.arraycopy(r0, 0, r, 0, ri)
        System.arraycopy(s, si, r, ri, n)
    }
    ri += n
  }
}

private[abc] object UnboxedArrayBuilder {

  def apply[@sp T](s: Int) = new UnboxedArrayBuilder[T] {
    override def size: Int = s
  }
}