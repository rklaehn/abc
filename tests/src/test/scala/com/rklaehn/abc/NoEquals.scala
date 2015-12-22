package com.rklaehn.abc

import cats._
import cats.implicits._

case class NoEquals(x: Int) {
  override def hashCode(): Int = throw new UnsupportedOperationException
  override def equals(x: Any): Boolean = throw new UnsupportedOperationException
  override def toString: String = throw new UnsupportedOperationException
}

trait NoEquals0 {
  implicit def eqv: Eq[NoEquals] = Eq.by(_.x)
}

trait NoEquals1 extends NoEquals0 {
  implicit def order: Order[NoEquals] = Order.by(_.x)
}

object NoEquals extends NoEquals1 {
  implicit def hash: Hash[NoEquals] = Hash.by(_.x)
  implicit def show: Show[NoEquals] = Show.show(_.x.toString)
}
