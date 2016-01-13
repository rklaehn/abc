package com.rklaehn.abc

import algebra.Eq
import cats.Show

private object Universal extends Eq[Any] with Show[Any] with Hash[Any] {
  def apply[T]: Eq[T] with Hash[T] with Show[T] = this.asInstanceOf[Eq[T] with Hash[T] with Show[T]]
  override def eqv(x: Any, y: Any): Boolean = x == y
  override def hash(a: Any): Int = a.hashCode
  override def show(a: Any): String = a.toString
}
