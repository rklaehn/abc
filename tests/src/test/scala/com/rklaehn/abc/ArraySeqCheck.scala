package com.rklaehn.abc

import algebra.Eq
import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop._
import algebra.std.all._
import Instances._

object ArraySeqSampleCheck extends Properties("ArraySet") {
  import arb._

  property("hash") = forAll { (x: Seq[Int]) ⇒
    val a = ArraySeq(x: _*)
    val b = ArraySeq(x: _*)
    Eq.eqv(a, b) && Hash.hash(a) == Hash.hash(b)
  }

  property("flatMap") = forAll { as: ArraySeq[Int] ⇒
    as.flatMap(x ⇒ ArraySeq(x, x + 1)).length == as.length * 2
  }

  property("filter") = forAll { (as: ArraySeq[Int], m: Int) ⇒
    as.filter(_ < m).elements.corresponds(as.elements.filter(_ < m))(_ == _)
  }
}