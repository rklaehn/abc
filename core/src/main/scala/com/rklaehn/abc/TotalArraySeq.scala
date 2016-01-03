package com.rklaehn.abc

import algebra._
import algebra.ring._
import cats.Show
import cats.syntax.show._

class TotalArraySeq[@sp T] private[abc](private[abc] val elements: Array[T], val default: T) {
  def apply(index: Int): T =
    if(index >= 0 && index < elements.sl) elements(index)
    else default
  def withoutDefault: ArraySeq[T] = new ArraySeq[T](elements)

  override def equals(that: Any): Boolean = that match {
    case that: TotalArraySeq[T] => TotalArraySeq.eqv(Universal[T]).eqv(this, that)
    case _ => false
  }

  override def hashCode(): Int = TotalArraySeq.hash(Universal[T]).hash(this)

  override def toString: String = TotalArraySeq.show(Universal[T]).show(this)
}

private[abc] trait TotalArraySeq0 {

  implicit def eqv[A: Eq]: Eq[TotalArraySeq[A]] = new Eq[TotalArraySeq[A]] {
    override def eqv(x: TotalArraySeq[A], y: TotalArraySeq[A]): Boolean =
      Eq.eqv(x.default, y.default) && ArrayUtil.eqv(x.elements, y.elements)
  }

  implicit def semigroup[A: Semigroup: Eq]: Semigroup[TotalArraySeq[A]] = new Semigroup[TotalArraySeq[A]] {
    def combine(x: TotalArraySeq[A], y: TotalArraySeq[A]): TotalArraySeq[A] = TotalArraySeq.zipWith(x, y)(Semigroup.combine)
  }

  implicit def additiveSemigroup[A: AdditiveSemigroup: Eq]: AdditiveSemigroup[TotalArraySeq[A]] = new AdditiveSemigroup[TotalArraySeq[A]] {
    def plus(x: TotalArraySeq[A], y: TotalArraySeq[A]): TotalArraySeq[A] = TotalArraySeq.zipWith(x, y)(AdditiveSemigroup.plus)
  }
}

private[abc] trait TotalArraySeq1 extends TotalArraySeq0 {

  implicit def order[A: Order]: Order[TotalArraySeq[A]] = new Order[TotalArraySeq[A]] {
    override def eqv(x: TotalArraySeq[A], y: TotalArraySeq[A]): Boolean =
      Eq.eqv(x.default, y.default) && ArrayUtil.eqv(x.elements, y.elements)

    override def compare(x: TotalArraySeq[A], y: TotalArraySeq[A]): Int = {
      val r = Order.compare(x.default, y.default)
      if (r != 0) r
      else ArrayUtil.vectorCompare(x.elements, x.default, y.elements, y.default)
    }
  }

  implicit def monoid[A: Monoid : Eq]: Monoid[TotalArraySeq[A]] = new Monoid[TotalArraySeq[A]] {
    def combine(x: TotalArraySeq[A], y: TotalArraySeq[A]): TotalArraySeq[A] = TotalArraySeq.zipWith(x, y)(Semigroup.combine)
    def empty: TotalArraySeq[A] = TotalArraySeq.constant(Monoid.empty[A])
  }

  implicit def additiveMonoid[A: AdditiveMonoid : Eq]: AdditiveMonoid[TotalArraySeq[A]] = new AdditiveMonoid[TotalArraySeq[A]] {
    def zero: TotalArraySeq[A] = TotalArraySeq.constant(AdditiveMonoid.zero[A])
    def plus(x: TotalArraySeq[A], y: TotalArraySeq[A]): TotalArraySeq[A] = TotalArraySeq.zipWith(x, y)(AdditiveSemigroup.plus)
  }
}

private[abc] trait TotalArraySeq2 extends TotalArraySeq1 {

  implicit def hash[A: Hash]: Hash[TotalArraySeq[A]] = new Hash[TotalArraySeq[A]] {
    override def eqv(x: TotalArraySeq[A], y: TotalArraySeq[A]): Boolean =
      Eq.eqv(x.default, y.default) && ArrayUtil.eqv(x.elements, y.elements)
    override def hash(a: TotalArraySeq[A]): Int =
      (ArrayUtil.hash(a.elements), Hash.hash(a.default)).##
  }

  implicit def group[A: Group: Eq]: Group[TotalArraySeq[A]] = new Group[TotalArraySeq[A]] {
    def empty: TotalArraySeq[A] = TotalArraySeq.constant(Monoid.empty[A])
    def combine(x: TotalArraySeq[A], y: TotalArraySeq[A]): TotalArraySeq[A] = TotalArraySeq.zipWith(x, y)(Semigroup.combine)
    def inverse(a: TotalArraySeq[A]): TotalArraySeq[A] = TotalArraySeq.map(a)(Group.inverse)
    override def remove(x: TotalArraySeq[A], y: TotalArraySeq[A]): TotalArraySeq[A] = TotalArraySeq.zipWith(x, y)(Group.remove)
  }

  implicit def additiveGroup[A: AdditiveGroup: Eq]: AdditiveGroup[TotalArraySeq[A]] = new AdditiveGroup[TotalArraySeq[A]] {
    def zero: TotalArraySeq[A] = TotalArraySeq.constant(AdditiveMonoid.zero[A])
    def plus(x: TotalArraySeq[A], y: TotalArraySeq[A]): TotalArraySeq[A] = TotalArraySeq.zipWith(x, y)(AdditiveSemigroup.plus)
    def negate(a: TotalArraySeq[A]): TotalArraySeq[A] = TotalArraySeq.map(a)(AdditiveGroup.negate)
    override def minus(x: TotalArraySeq[A], y: TotalArraySeq[A]): TotalArraySeq[A] = TotalArraySeq.zipWith(x, y)(AdditiveGroup.minus)
  }
}

private[abc] trait TotalArraySeq3 extends TotalArraySeq2 {

  implicit def semiring[A: Semiring: Eq]: Semiring[TotalArraySeq[A]] = new Semiring[TotalArraySeq[A]] {
    def plus(x: TotalArraySeq[A], y: TotalArraySeq[A]): TotalArraySeq[A] = TotalArraySeq.zipWith(x, y)(Semiring.plus)
    def times(x: TotalArraySeq[A], y: TotalArraySeq[A]): TotalArraySeq[A] = TotalArraySeq.zipWith(x, y)(Semiring.times)
    def zero: TotalArraySeq[A] = TotalArraySeq.constant(Semiring.zero[A])
  }
}

object TotalArraySeq extends TotalArraySeq3 {

  implicit def show[A: Show]: Show[TotalArraySeq[A]] = new Show[TotalArraySeq[A]] {
    override def show(a: TotalArraySeq[A]): String = a.elements.safe.map(_.show).mkString("ArraySeq(", ",", ").withDefault(" + a.default.show + ")")
  }

  implicit def rig[A: Rig: Eq]: Rig[TotalArraySeq[A]] = new Rig[TotalArraySeq[A]] {
    override def zero: TotalArraySeq[A] = TotalArraySeq.constant(Semiring.zero[A])
    override def plus(x: TotalArraySeq[A], y: TotalArraySeq[A]): TotalArraySeq[A] = TotalArraySeq.zipWith(x, y)(Semiring.plus)
    override def times(x: TotalArraySeq[A], y: TotalArraySeq[A]): TotalArraySeq[A] = TotalArraySeq.zipWith(x, y)(Semiring.times)
    override def one: TotalArraySeq[A] = TotalArraySeq.constant(Rig.one[A])
  }

  def constant[A](value: A) = new TotalArraySeq[A](emptyArray[A], value)

  private[abc] def zipWith[A: Eq](x: TotalArraySeq[A], y: TotalArraySeq[A])(f: (A, A) => A): TotalArraySeq[A] = {
    val rd = f(x.default, y.default)
    val re = ArrayUtil.combine(x.elements, x.default, y.elements, y.default)(f)
    new TotalArraySeq[A](ArrayUtil.dropRightWhile(re, rd), rd)
  }

  private[abc] def map[A: Eq](a: TotalArraySeq[A])(f: A => A): TotalArraySeq[A] = {
    val rd = f(a.default)
    val re = newArray(a.elements.sl, a.elements)
    var i = 0
    while(i < a.elements.sl) {
      re(i) = f(a.elements(i))
      i += 1
    }
    new TotalArraySeq[A](ArrayUtil.dropRightWhile(re, rd), rd)
  }
}