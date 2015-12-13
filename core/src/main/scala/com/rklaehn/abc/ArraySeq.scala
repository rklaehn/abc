package com.rklaehn.abc

import algebra.{Order, Monoid, Eq}
import cats.Show
import cats.syntax.show._

import scala.reflect.ClassTag
import scala.{ specialized => sp }

final class ArraySeq[@sp(Int, Long, Double) T] private[abc] (private[abc] val elements: Array[T]) extends NoEquals {

  def length = elements.length

  def apply(idx: Int): T = elements(idx)

  def isEmpty: Boolean = elements.isEmpty

  def concat(that: ArraySeq[T])(implicit classTag: ClassTag[T]): ArraySeq[T] =
    if (this.isEmpty) that
    else if (that.isEmpty) this
    else {
      val temp = new Array[T](this.elements.length + that.elements.length)
      System.arraycopy(this.elements, 0, temp, 0, this.elements.length)
      System.arraycopy(that.elements, 0, temp, this.elements.length, that.elements.length)
      new ArraySeq[T](temp)
    }

  def map[@sp(Int, Long, Double) U : ClassTag](f: T => U): ArraySeq[U] =
    new ArraySeq[U](this.elements.map(f))

  def flatMap[@sp(Int, Long, Double) U : ClassTag](f: T => ArraySeq[U]): ArraySeq[U] =
    new ArraySeq[U](this.elements.flatMap(x ⇒ f(x).elements))

  def filter(p: T => Boolean): ArraySeq[T] =
    new ArraySeq[T](this.elements.filter(p))
}

private[abc] trait ArraySeq0 {
  implicit def eq[A: Eq]: Eq[ArraySeq[A]] = Eq.by(_.elements)
}

private[abc] trait ArraySeq1 extends ArraySeq0 {
  implicit def order[A: Order]: Order[ArraySeq[A]] = Order.by(_.elements)
}

object ArraySeq extends ArraySeq1 {

  implicit def show[A: Show]: Show[ArraySeq[A]] = Show.show(_.elements.map(_.show).mkString("ArraySeq(", ",", ")"))

  implicit def hash[A: Hash]: Hash[ArraySeq[A]] = Hash.by(_.elements)

  implicit def monoid[A: ClassTag]: Monoid[ArraySeq[A]] = new Monoid[ArraySeq[A]] {
    override def empty: ArraySeq[A] = ArraySeq.empty[A]
    override def combine(x: ArraySeq[A], y: ArraySeq[A]): ArraySeq[A] = x concat y
  }

  def empty[@sp(Int, Long, Double) T: ClassTag]: ArraySeq[T] =
    new ArraySeq(Array.empty[T])

  def singleton[@sp(Int, Long, Double) T: ClassTag](e: T): ArraySeq[T] =
    new ArraySeq[T](Array.singleton(e))

  def apply[@sp(Int, Long, Double) T: ClassTag](elements: T*): ArraySeq[T] =
    new ArraySeq[T](elements.toArray)
}
