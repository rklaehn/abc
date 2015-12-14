package com.rklaehn.abc

import org.scalacheck.Properties
import org.scalacheck.Prop._
import algebra.lattice.Bool
import algebra.laws._
import algebra.std.all._
import Instances._
import arb._
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

class NegatableArraySetLawCheck extends FunSuite with Discipline {
  checkAll("LogicLaws[NegatableArraySet[Int]].bool", LogicLaws[NegatableArraySet[Int]].bool)
  checkAll("OrderLaws[NegatableArraySet[Int]].partialOrder", OrderLaws[NegatableArraySet[Int]].partialOrder)
}

object NegatableArraySetSampleCheck extends Properties("NegatableArraySet") {

  def unaryOp(a: NegatableArraySet[Int], r: NegatableArraySet[Int], op: Boolean ⇒ Boolean): Boolean = {
    val samples = a.elements :+ Int.MinValue
    samples.forall { e ⇒
      r(e) == op(a(e))
    }
  }

  def binaryOp(a: NegatableArraySet[Int], b: NegatableArraySet[Int], r: NegatableArraySet[Int], op: (Boolean, Boolean) ⇒ Boolean): Boolean = {
    val samples = (a.elements ++ b.elements).distinct :+ Int.MinValue
    samples.forall { e ⇒
      r(e) == op(a(e), b(e))
    }
  }
  
  val bool = implicitly[Bool[NegatableArraySet[Int]]]

  property("and") = forAll { (x: NegatableArraySet[Int], y: NegatableArraySet[Int]) ⇒
    binaryOp(x, y, bool.and(x, y), _ & _)
  }

  property("or") = forAll { (x: NegatableArraySet[Int], y: NegatableArraySet[Int]) ⇒
    binaryOp(x, y, bool.or(x, y), _ | _)
  }

  property("xor") = forAll { (x: NegatableArraySet[Int], y: NegatableArraySet[Int]) ⇒
    binaryOp(x, y, bool.xor(x, y), _ ^ _)
  }

  property("diff") = forAll { (x: NegatableArraySet[Int], y: NegatableArraySet[Int]) ⇒
    binaryOp(x, y, x diff y, (x,y) ⇒ x & !y)
  }

  property("not") = forAll { x: NegatableArraySet[Int] ⇒
    unaryOp(x, bool.complement(x), !_)
  }

  property("subsetOf") = forAll { (x: NegatableArraySet[Int], y: NegatableArraySet[Int]) ⇒
    val r1 = x subsetOf y
    val r0 = (x union y) === y
    r0 == r1
  }

  property("intersects") = forAll { (x: NegatableArraySet[Int], y: NegatableArraySet[Int]) ⇒
    val r1 = x intersects y
    val r0 = !(x intersect y).isEmpty
    r0 == r1
  }

  property("toString") = forAll { x: NegatableArraySet[Int] ⇒
    !x.toString.isEmpty
  }

  property("isEmpty") = forAll { x: NegatableArraySet[Int] ⇒
    if(x.isEmpty)
      x.elementsAsArraySeq.isEmpty
    else
      true
  }

  property("+") = forAll { (x: NegatableArraySet[Int], y: Int) ⇒
    (x + y).apply(y)
  }

  property("-") = forAll { (x: NegatableArraySet[Int], y: Int) ⇒
    !(x - y).apply(y)
  }
}