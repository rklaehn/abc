package com.rklaehn.abc

import scala.{ specialized => sp }

class ArraySeq[@sp(Int, Long, Double) T] private[abc] (
  private[abc] val elements: Array[T])(
    implicit tArrayTag: ArrayTag[T]) {

  def asIndexedSeq: IndexedSeq[T] = new IndexedSeq[T] {

    def length = elements.length

    def apply(idx: Int) = elements(idx)
  }

  def isEmpty: Boolean = elements.isEmpty

  def concat(that: ArraySeq[T]): ArraySeq[T] =
    if (this.isEmpty) that
    else if (that.isEmpty) this
    else {
      val temp = elements.newArray(this.elements.length + that.elements.length)
      System.arraycopy(that.elements, 0, temp, 0, this.elements.length)
      System.arraycopy(that.elements, 0, temp, this.elements.length, that.elements.length)
      new ArraySeq[T](temp)
    }

  override def equals(that: Any) = that match {
    case that: ArraySeq[T] => tArrayTag.eqv(this.elements, that.elements)
    case _ => false
  }

  override def hashCode: Int = tArrayTag.hash(elements)
}

object ArraySeq {

  def empty[@sp(Int, Long, Double) T: ArrayTag]: ArraySeq[T] =
    new ArraySeq(implicitly[ArrayTag[T]].empty)

  def singleton[@sp(Int, Long, Double) T: ArrayTag](e: T): ArraySeq[T] =
    new ArraySeq[T](implicitly[ArrayTag[T]].singleton(e))

  def apply[@sp(Int, Long, Double) T: ArrayTag](elements: T*): ArraySeq[T] =
    new ArraySeq[T](elements.toArray(implicitly[ArrayTag[T]].tClassTag))
}
