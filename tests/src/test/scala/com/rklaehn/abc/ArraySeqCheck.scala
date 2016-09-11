package com.rklaehn.abc

import algebra.Eq
import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop._
 import cats.kernel.instances.all._
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
}