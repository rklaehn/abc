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
}