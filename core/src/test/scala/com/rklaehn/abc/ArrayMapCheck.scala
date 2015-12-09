package com.rklaehn.abc

import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop._
import algebra.Eq
import algebra.std.all._
import Instances._

object ArrayMapArbitrary {

  implicit def arbArrayMap = Arbitrary {
    for {
      x ← Arbitrary.arbContainer[Vector, (Int, Int)].arbitrary
    } yield
    ArrayMap(x: _*)
  }
}

object ArrayMapSampleCheck extends Properties("ArrayMap") {
  import ArrayMapArbitrary.arbArrayMap
  import ArraySetArbitrary.arbArraySet

  def unaryOp(a: ArrayMap[Int, Int], r: ArrayMap[Int, Int], op: (Int, Option[Int]) ⇒ Option[Int]): Boolean = {
    val samples = a.keys0 :+ Int.MinValue
    samples.forall { e ⇒
      r.get(e) === op(e, a.get(e))
    }
  }
  def binaryOp(a: ArrayMap[Int, Int], b: ArrayMap[Int, Int], r: ArrayMap[Int, Int], op: (Int, Option[Int], Option[Int]) ⇒ Option[Int]): Boolean = {
    val samples = (a.keys0 ++ b.keys0).distinct :+ Int.MinValue
    samples.forall { e ⇒
      r.get(e) === op(e, a.get(e), b.get(e))
    }
  }

  property("merge") = forAll { (x: ArrayMap[Int, Int], y: ArrayMap[Int, Int]) ⇒
    binaryOp(x, y, x merge y, (k, vx, vy) ⇒ vy orElse vx)
  }

  property("mergeWith") = forAll { (x: ArrayMap[Int, Int], y: ArrayMap[Int, Int]) ⇒
    binaryOp(x, y, x.mergeWith(y, _ + _), (k, vx, vy) ⇒
      (vx + vy) orElse vy orElse vx
    )
  }
  
  property("exceptKeys") = forAll { (x: ArrayMap[Int, Int], y: ArraySet[Int]) ⇒
    unaryOp(x, x exceptKeys y, (k, vo) ⇒ if(y.contains(k)) None else vo)
  }

  property("justKeys") = forAll { (x: ArrayMap[Int, Int], y: ArraySet[Int]) ⇒
    unaryOp(x, x justKeys y, (k, vo) ⇒ if(y.contains(k)) vo else None)
  }

  property("filterKeys") = forAll { (x: ArrayMap[Int, Int], y: ArraySet[Int]) ⇒
    val r1 = x filterKeys y.contains
    val r0 = x justKeys y
    r0 === r1
  }

  property("filter") = forAll { (x: ArrayMap[Int, Int], y: ArraySet[Int]) ⇒
    val r1 = x filter { case (k, v) ⇒ y.contains(k) }
    val r0 = x justKeys y
    r0 === r1
  }

  property("except") = forAll { (x: ArrayMap[Int, Int], y: ArrayMap[Int, Int]) ⇒
    binaryOp(x, y, x.except(y, (vx, vy) ⇒ if(vx + vy > 0) Option(vx + vy) else Option.empty[Int]), { (k, xv, yv) =>
      if(xv.isDefined && yv.isDefined)
        (xv + yv).filter(_ > 0)
      else
        xv
    })
  }

//  property("or") = forAll { (x: ArraySet[Int], y: ArraySet[Int]) ⇒
//    binaryOp(x, y, x union y, _ | _)
//  }
//
//  property("xor") = forAll { (x: ArraySet[Int], y: ArraySet[Int]) ⇒
//    binaryOp(x, y, x xor y, _ ^ _)
//  }
//
//  property("diff") = forAll { (x: ArraySet[Int], y: ArraySet[Int]) ⇒
//    binaryOp(x, y, x diff y, (x,y) ⇒ x & !y)
//  }
//
//  property("subsetOf") = forAll { (x: ArraySet[Int], y: ArraySet[Int]) ⇒
//    val r1 = x subsetOf y
//    val r0 = (x union y) === y
//    r0 == r1
//  }
//
//  property("intersects") = forAll { (x: ArraySet[Int], y: ArraySet[Int]) ⇒
//    val r1 = x intersects y
//    val r0 = !(x intersect y).isEmpty
//    r0 == r1
//  }

  property("toString") = forAll { x: ArrayMap[Int, Int] ⇒
    !x.toString.isEmpty
  }

  property("iterator") = forAll { x: ArrayMap[Int, Int] ⇒
    x.iterator.toArray === (x.keys0 zip x.values0)
  }

//  property("isEmpty") = forAll { x: ArraySet[Int] ⇒
//    if(x.isEmpty)
//      x.asArraySeq.isEmpty
//    else
//      true
//  }
//
//  property("+") = forAll { (x: ArraySet[Int], y: Int) ⇒
//    (x + y).contains(y)
//  }
//
//  property("-") = forAll { (x: ArraySet[Int], y: Int) ⇒
//    !(x - y).contains(y)
//  }
//
//  property("filter") = forAll { (x: ArraySet[Int], y: ArraySet[Int]) ⇒
//    (x diff y) === (x filter(e ⇒ !y.contains(e)))
//  }
}