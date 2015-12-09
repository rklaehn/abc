package com.rklaehn.abc

import algebra.Order

import scala.reflect.ClassTag
import scala.util.hashing.Hashing
import algebra.std.all._
import Instances._

object MapMergeTest extends App {

  import scala.util.hashing.Hashing._
  import OrderedArrayTag.generic
//  implicit val x = OrderedArrayTag.default[Int](
//    implicitly[Order[Int]],
//    implicitly[ClassTag[Int]],
//    implicitly[Hashing[Int]])

  val r = ArrayMap(1 -> 2) merge ArrayMap(2 -> 3)
  println(r)

  val r2 = ArrayMap(1L -> "2") merge ArrayMap(2L -> "3")
//  val r3 = r2.updated(1L, "x")
  println(r2)
}
