//package com.rklaehn.abc
//
//import scala.collection.generic.CanBuildFrom
//import scala.collection.mutable.ArrayBuffer
//import scala.collection.{mutable, IndexedSeqOptimized}
//import scala.{ specialized => sp }
//
//class ArraySeq2[T] private[abc] (private[abc] val elements: Array[T]) extends AnyVal {
//
//  def length = elements.length
//
//  def apply(idx: Int): T = elements(idx)
//
//  def isEmpty: Boolean = elements.isEmpty
//
//  def concat(that: ArraySeq2[T])(implicit t: ArrayTag[T]): ArraySeq2[T] =
//    if (this.isEmpty) that
//    else if (that.isEmpty) this
//    else {
//      val temp = t.newArray(this.elements.length + that.elements.length)
//      System.arraycopy(that.elements, 0, temp, 0, this.elements.length)
//      System.arraycopy(that.elements, 0, temp, this.elements.length, that.elements.length)
//      new ArraySeq2[T](temp)
//    }
//
//  def map[U : ArrayTag](f: T => U): ArraySeq2[U] =
//    new ArraySeq2[U](this.elements.map(f).toArray(ArrayTag[U].classTag))
//
//  def filter(p: T => Boolean)(implicit t: ArrayTag[T]): ArraySeq2[T] =
//    new ArraySeq2[T](this.elements.filter(p))
//}
//
//object ArraySeq2 {
//
//  implicit def cbf[T, U: ArrayTag]: CanBuildFrom[ArraySeq2[T], U, ArraySeq2[U]] = new CanBuildFrom[ArraySeq2[T], U, ArraySeq2[U]] {
//    def apply(from: ArraySeq2[T]) = apply()
//
//    def apply() = new ArrayBuffer[U].mapResult(x ⇒ new ArraySeq2[U](x.toArray(ArrayTag[U].classTag)))
//  }
//
//  def empty[T: ArrayTag]: ArraySeq2[T] =
//    new ArraySeq2(ArrayTag[T].empty)
//
//  def singleton[T: ArrayTag](e: T): ArraySeq2[T] =
//    new ArraySeq2[T](ArrayTag[T].singleton(e))
//
//  def apply[T: ArrayTag](elements: T*): ArraySeq2[T] =
//    new ArraySeq2[T](elements.toArray(ArrayTag[T].classTag))
//}
