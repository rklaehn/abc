package com.rklaehn.abc

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.collection.{mutable, IndexedSeqOptimized}
import scala.{ specialized => sp }

class ArraySeq[@sp(Int, Long, Double) T] private[abc] (
                                                        private[abc] val elements: Array[T]) (
                                                        implicit tArrayTag: ArrayTag[T]) extends IndexedSeq[T] with IndexedSeqOptimized[T, ArraySeq[T]] {

  override protected[this] def newBuilder: mutable.Builder[T, ArraySeq[T]] =
    new ArrayBuffer[T].mapResult(x ⇒ new ArraySeq[T](x.toArray(tArrayTag.classTag)))

  def length = elements.length

  def apply(idx: Int): T = elements(idx)

  override def isEmpty: Boolean = elements.isEmpty

  def concat(that: ArraySeq[T]): ArraySeq[T] =
    if (this.isEmpty) that
    else if (that.isEmpty) this
    else {
      val temp = tArrayTag.newArray(this.elements.length + that.elements.length)
      System.arraycopy(that.elements, 0, temp, 0, this.elements.length)
      System.arraycopy(that.elements, 0, temp, this.elements.length, that.elements.length)
      new ArraySeq[T](temp)
    }

  def mapDirect[U : ArrayTag](f: T => U): ArraySeq[U] =
    new ArraySeq[U](this.elements.map(f).toArray(ArrayTag[U].classTag))

  override def filter(p: T => Boolean): ArraySeq[T] =
    new ArraySeq[T](this.elements.filter(p))

  override def equals(that: Any) = that match {
    case that: ArraySeq[T] => tArrayTag.eqv(this.elements, that.elements)
    case _ => false
  }

  override def hashCode: Int = tArrayTag.hash(elements)
}

object ArraySeq {

  implicit def cbf[T, U: ArrayTag]: CanBuildFrom[ArraySeq[T], U, ArraySeq[U]] = new CanBuildFrom[ArraySeq[T], U, ArraySeq[U]] {
    def apply(from: ArraySeq[T]) = apply()

    def apply() = new ArrayBuffer[U].mapResult(x ⇒ new ArraySeq[U](x.toArray(ArrayTag[U].classTag)))
  }

  def empty[@sp(Int, Long, Double) T: ArrayTag]: ArraySeq[T] =
    new ArraySeq(ArrayTag[T].empty)

  def singleton[@sp(Int, Long, Double) T: ArrayTag](e: T): ArraySeq[T] =
    new ArraySeq[T](ArrayTag[T].singleton(e))

  def apply[@sp(Int, Long, Double) T: ArrayTag](elements: T*): ArraySeq[T] =
    new ArraySeq[T](elements.toArray(ArrayTag[T].classTag))
}
