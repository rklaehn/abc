package com.rklaehn.abc

import com.rklaehn.abc.ArraySeq.AsCollection

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.collection.{mutable, IndexedSeqOptimized}
import scala.{ specialized => sp }

class ArraySeq[@sp(Int, Long, Double) T] private[abc] (private[abc] val elements: Array[T]) {

  def asCollection(implicit tArrayTag: ArrayTag[T]): IndexedSeq[T] =
    new AsCollection[T](this)

  def length = elements.length

  def apply(idx: Int): T = elements(idx)

  def isEmpty: Boolean = elements.isEmpty

  def concat(that: ArraySeq[T])(implicit tArrayTag: ArrayTag[T]): ArraySeq[T] =
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

  def filter(p: T => Boolean): ArraySeq[T] =
    new ArraySeq[T](this.elements.filter(p))
}

object ArraySeq {

  final class AsCollection[T](val underlying: ArraySeq[T])(implicit tArrayTag: ArrayTag[T]) extends IndexedSeq[T] with IndexedSeqOptimized[T, AsCollection[T]] {

    override protected[this] def newBuilder: mutable.Builder[T, AsCollection[T]] =
      new ArrayBuffer[T].mapResult(x ⇒ new AsCollection(new ArraySeq(x.toArray(tArrayTag.classTag))))

    def apply(idx: Int) = underlying.apply(idx)

    def length = underlying.length

    override def equals(that: Any) = that match {
      case that: AsCollection[T] => tArrayTag.eqv(this.underlying.elements, that.underlying.elements)
      case _ => false
    }

    override def hashCode: Int = tArrayTag.hash(underlying.elements)
  }

  object AsCollection {

    implicit def cbf[T, U: ArrayTag]: CanBuildFrom[AsCollection[T], U, AsCollection[U]] = new CanBuildFrom[AsCollection[T], U, AsCollection[U]] {
      def apply(from: AsCollection[T]) = apply()

      def apply() = new ArrayBuffer[U].mapResult(x ⇒ new AsCollection(new ArraySeq[U](x.toArray(ArrayTag[U].classTag))))
    }
  }

  def empty[@sp(Int, Long, Double) T: ArrayTag]: ArraySeq[T] =
    new ArraySeq(ArrayTag[T].empty)

  def singleton[@sp(Int, Long, Double) T: ArrayTag](e: T): ArraySeq[T] =
    new ArraySeq[T](ArrayTag[T].singleton(e))

  def apply[@sp(Int, Long, Double) T: ArrayTag](elements: T*): ArraySeq[T] =
    new ArraySeq[T](elements.toArray(ArrayTag[T].classTag))
}
