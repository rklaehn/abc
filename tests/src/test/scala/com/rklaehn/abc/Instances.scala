package com.rklaehn.abc

import algebra.Eq

object Instances {

  implicit class OptionIntSyntax(lhs: Option[Int]) {
    def +(rhs: Option[Int]) : Option[Int] = (lhs, rhs) match {
      case (Some(x), Some(y)) ⇒ Some(x + y)
      case _ ⇒ None
    }
  }

  implicit class EqSyntax[T: Eq](lhs: T) {
    def ===(rhs: T) = Eq.eqv(lhs, rhs)
  }

  // todo: remove once algebra has array instances (or use spire for instances once that moves to algebra)?
  implicit def arrayEq[A](implicit aEq: Eq[A]): Eq[Array[A]] = new Eq[Array[A]] {
    def eqv(x: Array[A], y: Array[A]): Boolean = {
      x.sl == y.sl && {
        var i = 0
        while(i < x.sl) {
          if(!aEq.eqv(x(i), y(i)))
            return false
          i += 1
        }
        true
      }
    }
  }

  // todo: remove once algebra has unit instances (or use spire for instances once that moves to algebra)?
  implicit object unitEq extends Eq[Unit] {

    def eqv(x: Unit, y: Unit) = true
  }

  // todo: remove once algebra has unit instances (or use spire for instances once that moves to algebra)?
  implicit def tuple2Eq[A, B](implicit aEq: Eq[A], bEq: Eq[B]): Eq[(A, B)] = new Eq[(A, B)] {
    def eqv(x: (A, B), y: (A, B)) = aEq.eqv(x._1, y._1) && bEq.eqv(x._2, y._2)
  }
}
