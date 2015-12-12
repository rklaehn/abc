package com.rklaehn.abc

import algebra.Order

import scala.{specialized ⇒ sp}

private object Searching {

  // scalastyle:off return
  final def search[@sp A: Order](as: Array[A], from: Int, until: Int, item: A): Int = {
    var first = from
    var last = until - 1
    while (first <= last) {
      val middle = (first + last) >>> 1

      val compare = Order.compare(as(middle), item)
      if (compare < 0) first = middle + 1
      else if (compare > 0) last = middle - 1
      else return middle
    }
    -first - 1
  }
  // scalastyle:on return
}
