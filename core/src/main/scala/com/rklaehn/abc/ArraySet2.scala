//package com.rklaehn.abc
//
//object ArraySet2 {
//
//  private def apply[T](elements: Array[T]) = new ArraySet2[T](elements)
//
//  def empty[T: ArrayTag]: ArraySet2[T] =
//    ArraySet2[T](ArrayTag[T].empty)
//
//  def singleton[T: ArrayTag](e: T): ArraySet2[T] =
//    ArraySet2[T](ArrayTag[T].singleton(e))
//
//  def apply[T: OrderedArrayTag](elements: T*): ArraySet2[T] = {
//    val t = OrderedArrayTag[T]
//    def union(a: Array[T], b: Array[T]) =
//      SetUtils.union(a, b)
//    val reducer = Reducer.create[Array[T]](union)
//    elements.foreach(e => reducer(t.singleton(e)))
//    new ArraySet2[T](reducer.result().getOrElseFast(t.empty))
//  }
//}
//
//final class ArraySet2[T] private (private[abc] val elements0: Array[T]) extends AnyVal { self =>
//
//  def elements = new ArraySeq2[T](elements0)
//
//  def contains(elem: T)(implicit t: OrderedArrayTag[T]) =
//    self.apply(elem)
//
//  def apply(e: T)(implicit t: OrderedArrayTag[T]): Boolean =
//    t.binarySearch(elements0, 0, elements0.length, e) >= 0
//
//  def +(elem: T)(implicit t: OrderedArrayTag[T]) =
//    self.union(ArraySet2.singleton(elem))
//
//  def -(elem: T)(implicit t: OrderedArrayTag[T]) =
//    self.diff(ArraySet2.singleton(elem))
//
//  def iterator =
//    elements0.iterator
//
//  def intersects(that: ArraySet2[T])(implicit t: OrderedArrayTag[T]): Boolean =
//    SetUtils.intersects(this.elements0, that.elements0)
//
//  def subsetOf(that: ArraySet2[T])(implicit t: OrderedArrayTag[T]): Boolean =
//    SetUtils.subsetOf(this.elements0, that.elements0)
//
//  def union(that: ArraySet2[T])(implicit t: OrderedArrayTag[T]): ArraySet2[T] =
//    ArraySet2[T](SetUtils.union(this.elements0, that.elements0))
//
//  def intersect(that: ArraySet2[T])(implicit t: OrderedArrayTag[T]): ArraySet2[T] =
//    ArraySet2[T](SetUtils.intersection(this.elements0, that.elements0))
//
//  def diff(that: ArraySet2[T])(implicit t: OrderedArrayTag[T]): ArraySet2[T] =
//    ArraySet2[T](SetUtils.diff(this.elements0, that.elements0))
//
//  def xor(that: ArraySet2[T])(implicit t: OrderedArrayTag[T]): ArraySet2[T] =
//    ArraySet2[T](SetUtils.xor(this.elements0, that.elements0))
//
//  def filter(p: T => Boolean): ArraySet2[T] =
//    ArraySet2[T](this.elements0.filter(p))
//
//  def isEmpty: Boolean =
//    elements0.isEmpty
//
//  def size =
//    elements0.length
//}