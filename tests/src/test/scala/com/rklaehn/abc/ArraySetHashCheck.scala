package com.rklaehn.abc

import algebra.{Eq, Order}
import cats.implicits._
import org.scalacheck.Arbitrary
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

import scala.reflect.ClassTag

class ArraySetHashCheck extends FunSuite with Checkers with Helpers {

  def checkHashing[T: ClassTag: Order: Hash: Arbitrary](): Unit = {
    val name = typeName[T]
    test(s"hashConsistency $name") {
      check { xs: Vector[T] ⇒
        Eq.eqv(ArraySet(xs: _*), ArraySet(xs.reverse: _*)) &&
        Hash.hash(ArraySet(xs: _*)) == Hash.hash(ArraySet(xs.reverse: _*))
      }
    }
  }
  checkHashing[Byte]()
  checkHashing[Short]()
  checkHashing[Int]()
  checkHashing[Long]()
  checkHashing[Float]()
  checkHashing[Double]()
  checkHashing[Boolean]()
  checkHashing[Char]()
  checkHashing[String]()
}