package com.rklaehn.abc

import algebra.{Eq, Monoid, Order}
import cats._
import cats.syntax.show._
import com.rklaehn.sonicreducer.Reducer

final class ArraySeq[@sp T] private[abc] (private[abc] val elements: Array[T]) {

  def length = elements.length

  def get(index: Int): Option[T] =
    if(index < 0 || index >= elements.length) None
    else Some(elements(index))

  def getOrElse(index: Int, default: T): T =
    if(index < 0 || index >= elements.length) default
    else elements(index)

  def withDefault(value: T)(implicit ev:Eq[T]): TotalArraySeq[T] =
    new TotalArraySeq[T](ArrayUtil.dropRightWhile(elements, value), value)

  def isEmpty: Boolean = elements.isEmpty

  def concat(that: ArraySeq[T]): ArraySeq[T] =
    if (this.isEmpty) that
    else if (that.isEmpty) this
    else {
      val temp = newArray(this.elements.length + that.elements.length, this.elements)
      System.arraycopy(this.elements, 0, temp, 0, this.elements.length)
      System.arraycopy(that.elements, 0, temp, this.elements.length, that.elements.length)
      new ArraySeq[T](temp)
    }

  def map[@sp U: ClassTag](f: T => U): ArraySeq[U] =
    new ArraySeq[U](this.elements.map(f))

  def flatMap[@sp U: ClassTag](f: T => ArraySeq[U]): ArraySeq[U] =
    new ArraySeq[U](this.elements.flatMap(x ⇒ f(x).elements))

  def filter(p: T => Boolean): ArraySeq[T] =
    new ArraySeq[T](this.elements.filter(p))

  override def equals(that: Any): Boolean = that match {
    case that: ArraySeq[T] => ArraySeq.eqv(Universal[T]).eqv(this, that)
    case _ => false
  }

  override def hashCode(): Int = ArraySeq.hash(Universal[T]).hash(this)

  override def toString: String = ArraySeq.show(Universal[T]).show(this)
}

private[abc] trait ArraySeq0 {
  implicit def eqv[A: Eq]: Eq[ArraySeq[A]] = Eq.by(_.elements)
}

private[abc] trait ArraySeq1 extends ArraySeq0 {
  implicit def order[A: Order]: Order[ArraySeq[A]] = Order.by(_.elements)
}

object ArraySeq extends ArraySeq1 {

  implicit val foldable: Foldable[ArraySeq] = new Foldable[ArraySeq] {
    def foldLeft[A, B](fa: ArraySeq[A], b: B)(f: (B, A) ⇒ B): B = fa.elements.foldLeft[B](b)(f)
    def foldRight[A, B](fa: ArraySeq[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]) = {
      def loop(i: Int): Eval[B] =
        if (i < fa.elements.length) f(fa.elements(i), Eval.defer(loop(i + 1))) else lb
      Eval.defer(loop(0))
    }
  }

  implicit def show[A: Show]: Show[ArraySeq[A]] = Show.show(_.elements.map(_.show).mkString("ArraySeq(", ",", ")"))

  implicit def hash[A: Hash]: Hash[ArraySeq[A]] = Hash.by(_.elements)

  implicit def monoid[A: ClassTag]: Monoid[ArraySeq[A]] = new Monoid[ArraySeq[A]] {
    override def empty: ArraySeq[A] = ArraySeq.empty[A]
    override def combine(x: ArraySeq[A], y: ArraySeq[A]): ArraySeq[A] = x concat y
    override def combineAll(as: TraversableOnce[ArraySeq[A]]) = Reducer.reduce(as)(combine).getOrElse(empty)
  }

  def empty[@sp T: ClassTag]: ArraySeq[T] =
    new ArraySeq(Array.empty[T])

  def singleton[@sp T: ClassTag](e: T): ArraySeq[T] =
    new ArraySeq[T](Array.singleton(e))

  def apply[@sp T: ClassTag](elements: T*): ArraySeq[T] = {
    val t = new Array[T](elements.length)
    // we must not use toArray, because somebody might have passed an array, and toArray would return that array (*not* a copy!)
    elements.copyToArray(t)
    new ArraySeq[T](t)
  }
}
