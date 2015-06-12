package com.rklaehn.abc
import scala.{specialized => sp}

import scala.reflect.ClassTag

class Seq[@sp T] private[abc] (private val _elements: Array[T]) {

}

object Seq {

  def empty[T: ClassTag]: Seq[T] = new Seq[T](Array.empty[T])

  def singleton[T: ClassTag](e: T): Seq[T] = new Seq[T](Array(e))

  def apply[T: ClassTag](elements: T*): Seq[T] =
    // todo: is this safe if elements is an array?
    new Seq[T](elements.toArray)
}
