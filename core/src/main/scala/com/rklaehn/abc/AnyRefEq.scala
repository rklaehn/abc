package com.rklaehn.abc

import spire.algebra.Eq

object AnyRefEq {

  implicit def anyRefEq[T <: AnyRef]: Eq[T] = new Eq[T] {
    def eqv(x: T, y: T) = x == y
  }
}
