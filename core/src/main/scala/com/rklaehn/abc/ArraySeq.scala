package com.rklaehn.abc

import algebra.Eq
import com.rklaehn.abc.ArraySeq.AsCollection

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.collection.{mutable, IndexedSeqOptimized}
import scala.reflect.ClassTag
import scala.{ specialized => sp }

final class ArraySeq[@sp(Int, Long, Double) T] private[abc] (private[abc] val elements: Array[T]) extends NoEquals {

  // $COVERAGE-OFF$
  def asCollection(implicit eq:Eq[T], hash: Hash[T], classTag: ClassTag[T]): AsCollection[T] =
    new AsCollection[T](this)
  // $COVERAGE-ON$

  def length = elements.length

  def apply(idx: Int): T = elements(idx)

  def isEmpty: Boolean = elements.isEmpty

  def concat(that: ArraySeq[T])(implicit ev: ClassTag[T]): ArraySeq[T] =
    if (this.isEmpty) that
    else if (that.isEmpty) this
    else {
      val temp = ev.newArray(this.elements.length + that.elements.length)
      System.arraycopy(that.elements, 0, temp, 0, this.elements.length)
      System.arraycopy(that.elements, 0, temp, this.elements.length, that.elements.length)
      new ArraySeq[T](temp)
    }

  def map[@sp(Int, Long, Double) U : ClassTag](f: T => U): ArraySeq[U] =
    new ArraySeq[U](this.elements.map(f))

  def filter(p: T => Boolean): ArraySeq[T] =
    new ArraySeq[T](this.elements.filter(p))
}

object ArraySeq {

  implicit def hash[A: Hash]: Hash[ArraySeq[A]] = Hash.by(_.elements)

  // $COVERAGE-OFF$
  final class AsCollection[T : Eq: Hash: ClassTag](val underlying: ArraySeq[T]) extends IndexedSeq[T] with IndexedSeqOptimized[T, AsCollection[T]] {

    override protected[this] def newBuilder: mutable.Builder[T, AsCollection[T]] =
      new ArrayBuffer[T].mapResult(x ⇒ new AsCollection(new ArraySeq(x.toArray)))

    def apply(idx: Int) = underlying.apply(idx)

    def length = underlying.length

    override def equals(that: Any) = that match {
      case that: AsCollection[T] => Eq.eqv(this.underlying.elements, that.underlying.elements)
      case _ => false
    }

    override def hashCode: Int = Hash.hash(underlying.elements)
  }

  object AsCollection {

    implicit def cbf[T, U: Eq:Hash:ClassTag]: CanBuildFrom[AsCollection[T], U, AsCollection[U]] = new CanBuildFrom[AsCollection[T], U, AsCollection[U]] {
      def apply(from: AsCollection[T]) = apply()

      def apply() = new ArrayBuffer[U].mapResult(x ⇒ new AsCollection(new ArraySeq[U](x.toArray)))
    }
  }
  // $COVERAGE-ON$

  def empty[@sp(Int, Long, Double) T: ClassTag]: ArraySeq[T] =
    new ArraySeq(Array.empty[T])

  def singleton[@sp(Int, Long, Double) T: ClassTag](e: T): ArraySeq[T] =
    new ArraySeq[T](Array.singleton(e))

  def apply[@sp(Int, Long, Double) T: ClassTag](elements: T*): ArraySeq[T] =
    new ArraySeq[T](elements.toArray)
}
