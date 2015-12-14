package com.rklaehn.abc

import algebra.ring.{AdditiveMonoid, AdditiveSemigroup}
import cats.Show

import cats.syntax.show._
import scala.reflect.ClassTag
import scala.util.hashing.MurmurHash3
import scala.{ specialized ⇒ sp }
import algebra._

final class TotalArrayMap[@sp(Int, Long, Double) K, @sp(Int, Long, Double) V](
    private[abc] val keys0: Array[K],
    private[abc] val values0: Array[V],
    private[abc] val default: V
  ) extends NoEquals { self ⇒

  import TotalArrayMap._

  def iterator = keys0.iterator zip values0.iterator

  def size: Int = keys0.length

  def keys: ArraySet[K] = new ArraySet[K](keys0)

  def values: ArraySeq[V] = new ArraySeq[V](values0)

  def apply(k: K)(implicit kOrder: Order[K]): V = {
    val i = Searching.search(keys0, 0, keys0.length, k)
    if (i >= 0) values0(i)
    else default
  }

  def merge(that: TotalArrayMap[K, V])(implicit kOrder: Order[K], kClassTag: ClassTag[K], vClassTag: ClassTag[V]): TotalArrayMap[K, V] =
    new Merge[K, V](this, that).result

  def unionWith(that: TotalArrayMap[K, V], f: (V, V) ⇒ V)(implicit kOrder: Order[K], kClassTag: ClassTag[K], vClassTag: ClassTag[V]): TotalArrayMap[K, V] =
    new UnionWith[K, V](this, that, f).result

  def filterKeys(f: K ⇒ Boolean)(implicit kOrder: Order[K], kClassTag: ClassTag[K], vClassTag: ClassTag[V]): TotalArrayMap[K, V] = {
    val rk = new Array[K](keys0.length)
    val rv = new Array[V](values0.length)
    var ri = 0
    var i = 0
    while(i < keys0.length) {
      if (f(keys0(i))) {
        rk(ri) = keys0(i)
        rv(ri) = values0(i)
        ri += 1
      }
      i += 1
    }
    if(ri == rk.length) this
    else new TotalArrayMap[K, V](rk.resizeInPlace(ri), rv.resizeInPlace(ri), default)
  }

//  def filterValues(f: V ⇒ Boolean)(implicit kOrder: Order[K], kClassTag: ClassTag[K], vClassTag: ClassTag[V]): TotalArrayMap[K, V] = {
//    val rk = new Array[K](keys0.length)
//    val rv = new Array[V](values0.length)
//    var ri = 0
//    var i = 0
//    while(i < keys0.length) {
//      if (f(values0(i))) {
//        rk(ri) = keys0(i)
//        rv(ri) = values0(i)
//        ri += 1
//      }
//      i += 1
//    }
//    if(ri == rk.length) this
//    else new TotalArrayMap[K, V](rk.resizeInPlace(ri), rv.resizeInPlace(ri))
//  }

  def filter(f: ((K, V)) ⇒ Boolean)(implicit kOrder: Order[K], kClassTag: ClassTag[K], vClassTag: ClassTag[V]): TotalArrayMap[K, V] = {
    val rk = kClassTag.newArray(keys0.length)
    val rv = vClassTag.newArray(values0.length)
    var ri = 0
    var i = 0
    while(i < keys0.length) {
      if (f((keys0(i), values0(i)))) {
        rk(ri) = keys0(i)
        rv(ri) = values0(i)
        ri += 1
      }
      i += 1
    }
    if(ri == rk.length) this
    else new TotalArrayMap[K, V](rk.resizeInPlace(ri), rv.resizeInPlace(ri), default)
  }

  def justKeys(keys: ArraySet[K])(implicit kOrder: Order[K], kClassTag: ClassTag[K], vClassTag: ClassTag[V]): TotalArrayMap[K, V] =
    new JustKeys[K, V](this, keys).result

  def exceptKeys(keys: ArraySet[K])(implicit kOrder: Order[K], kClassTag: ClassTag[K], vClassTag: ClassTag[V]): TotalArrayMap[K, V] =
    new ExceptKeys[K, V](this, keys).result

  def mapValues[@sp(Int, Long, Double) V2: ClassTag](f: V => V2): TotalArrayMap[K, V2] =
    new TotalArrayMap(keys0, values0.map(f), f(default))

  override def toString: String =
    keys0.indices.map(i => s"${keys0(i)}->${values0(i)}").mkString("Map(", ",", ")")
}

private[abc] trait TotalArrayMap1 {

  implicit def eqv[K: Eq, V: Eq]: Eq[TotalArrayMap[K, V]] = new Eq[TotalArrayMap[K, V]] {
    def eqv(x: TotalArrayMap[K, V], y: TotalArrayMap[K, V]) =
      Eq.eqv(x.keys0, y.keys0) && Eq.eqv(x.values0, y.values0) && Eq.eqv(x.default, y.default)
  }

  implicit def monoid[K: ClassTag : Order, V: ClassTag: Monoid]: Monoid[TotalArrayMap[K, V]] =
    new ArrayTotalMapMonoid[K, V]

  implicit def additiveMonoid[K: ClassTag: Order, V: ClassTag: AdditiveMonoid]: AdditiveMonoid[TotalArrayMap[K, V]] =
    new ArrayTotalMapAdditiveMonoid[K, V]

//  implicit def group[K: ClassTag: Order, V: ClassTag: Group: Eq]: Group[TotalArrayMap[K, V]] =
//    new MapGroup[K, V]
}

private class ArrayTotalMapMonoid[K: ClassTag : Order, V: ClassTag: Monoid] extends Monoid[TotalArrayMap[K, V]] {
  override def empty: TotalArrayMap[K, V] = TotalArrayMap.fromDefault[K, V](Monoid[V].empty)
  override def combine(x: TotalArrayMap[K, V], y: TotalArrayMap[K, V]): TotalArrayMap[K, V] =
    x.unionWith(y, (x, y) ⇒ Semigroup.combine(x, y))
}

//private final class MapGroup[K: ClassTag : Order, V: ClassTag : Eq: Group] extends MapMonoid[K, V] with Group[TotalArrayMap[K, V]] {
//
//  def inverse(x: TotalArrayMap[K, V]): TotalArrayMap[K, V] =
//    x.mapValues(v ⇒ Group.inverse(v)).filterValues(x ⇒ Eq.neqv(x, Group.empty))
//
//  override def remove(x: TotalArrayMap[K, V], y: TotalArrayMap[K, V]): TotalArrayMap[K, V] = {
//    // everything that is in y but not in x must be inverted
//    val justY = y.exceptKeys(x.keys).mapValues(x ⇒ Group.inverse(x))
//    // everything that is in x but not in y can be kept
//    val justX = x.exceptKeys(y.keys)
//    // everything that is in both needs to be removed
//    val intersection = x.intersectWith(y, (x, y) ⇒ Group.remove(x, y))
//    // merge the results (they do not overlap)
//    (justX merge justY merge intersection).filterValues(x ⇒ Eq.neqv(x, Group.empty))
//  }
//}

private final class ArrayTotalMapAdditiveMonoid[K: ClassTag : Order, V: ClassTag: AdditiveMonoid] extends AdditiveMonoid[TotalArrayMap[K, V]] {
  override def zero: TotalArrayMap[K, V] = TotalArrayMap.fromDefault[K, V](AdditiveMonoid[V].zero)
  override def plus(x: TotalArrayMap[K, V], y: TotalArrayMap[K, V]): TotalArrayMap[K, V] =
    x.unionWith(y, (x, y) ⇒ AdditiveSemigroup.plus(x, y))
}

object TotalArrayMap extends TotalArrayMap1 {

  implicit def show[K: Show, V: Show]: Show[TotalArrayMap[K, V]] = Show.show(m ⇒
    m.iterator
      .map { case (k,v) ⇒ s"${k.show}->${v.show}" }
      .mkString(s"TotalArrayMap((", ",", s"), ${m.default.show})")
  )

  implicit def hash[K: Hash, V: Hash]: Hash[TotalArrayMap[K, V]] = new Hash[TotalArrayMap[K, V]] {
    def hash(x: TotalArrayMap[K, V]) =
      MurmurHash3.mix(Hash.hash(x.keys0), Hash.hash(x.values0))

    def eqv(x: TotalArrayMap[K, V], y: TotalArrayMap[K, V]) = {
      Eq.eqv(x.keys0, y.keys0) && Eq.eqv(x.values0, y.values0)
    }
  }

  private final class Merge[@sp(Int, Long, Double) K: Order: ClassTag, @sp(Int, Long, Double) V: ClassTag](
    a: TotalArrayMap[K, V],
    b: TotalArrayMap[K, V]
  ) extends BinaryMerge {

    @inline def ak = a.keys0
    @inline def av = a.values0
    @inline def bk = b.keys0
    @inline def bv = b.values0

    val rd = b.default
    val rk = new Array[K](a.size + b.size)
    val rv = new Array[V](a.size + b.size)
    var ri = 0

    def compare(ai: Int, bi: Int) = Order.compare(ak(ai), bk(bi))

    def fromA(a0: Int, a1: Int, bi: Int) = {
      System.arraycopy(ak, a0, rk, ri, a1 - a0)
      System.arraycopy(av, a0, rv, ri, a1 - a0)
      ri += a1 - a0
    }

    def fromB(ai: Int, b0: Int, b1: Int) = {
      System.arraycopy(bk, b0, rk, ri, b1 - b0)
      System.arraycopy(bv, b0, rv, ri, b1 - b0)
      ri += b1 - b0
    }

    def collision(ai: Int, bi: Int) = {
      rk(ri) = bk(bi)
      rv(ri) = bv(bi)
      ri += 1
    }

    merge0(0, ak.length, 0, bk.length)

    def result: TotalArrayMap[K, V] = new TotalArrayMap[K, V](rk.resizeInPlace(ri), rv.resizeInPlace(ri), rd)
  }

  private final class UnionWith[@sp(Int, Long, Double) K: Order: ClassTag, @sp(Int, Long, Double) V: ClassTag](
    a: TotalArrayMap[K, V], b: TotalArrayMap[K, V], f: (V, V) => V)
    extends BinaryMerge {

    @inline def ak = a.keys0
    @inline def av = a.values0
    @inline def bk = b.keys0
    @inline def bv = b.values0

    val rd = f(a.default, b.default)
    val rk = new Array[K](a.size + b.size)
    val rv = new Array[V](a.size + b.size)
    var ri = 0

    def compare(ai: Int, bi: Int) = Order.compare(ak(ai), bk(bi))

    def fromA(a0: Int, a1: Int, bi: Int) = {
      System.arraycopy(ak, a0, rk, ri, a1 - a0)
      System.arraycopy(av, a0, rv, ri, a1 - a0)
      ri += a1 - a0
    }

    def fromB(ai: Int, b0: Int, b1: Int) = {
      System.arraycopy(bk, b0, rk, ri, b1 - b0)
      System.arraycopy(bv, b0, rv, ri, b1 - b0)
      ri += b1 - b0
    }

    def collision(ai: Int, bi: Int) = {
      rk(ri) = bk(bi)
      rv(ri) = f(av(ai), bv(bi))
      ri += 1
    }

    merge0(0, ak.length, 0, bk.length)

    def result: TotalArrayMap[K, V] = new TotalArrayMap[K, V](rk.resizeInPlace(ri), rv.resizeInPlace(ri), rd)
  }

//  private final class IntersectWith[@sp(Int, Long, Double) K: Order: ClassTag, @sp(Int, Long, Double) V: ClassTag](
//    a: TotalArrayMap[K, V], b: TotalArrayMap[K, V], f: (V, V) => V)
//    extends BinaryMerge {
//
//    @inline def ak = a.keys0
//    @inline def av = a.values0
//    @inline def bk = b.keys0
//    @inline def bv = b.values0
//
//    val rk = new Array[K](a.size min b.size)
//    val rv = new Array[V](a.size min b.size)
//    var ri = 0
//
//    def compare(ai: Int, bi: Int) = Order.compare(ak(ai), bk(bi))
//
//    def fromA(a0: Int, a1: Int, bi: Int) = {}
//
//    def fromB(ai: Int, b0: Int, b1: Int) = {}
//
//    def collision(ai: Int, bi: Int) = {
//      rk(ri) = bk(bi)
//      rv(ri) = f(av(ai), bv(bi))
//      ri += 1
//    }
//
//    merge0(0, ak.length, 0, bk.length)
//
//    def result: TotalArrayMap[K, V] = new TotalArrayMap[K, V](rk.resizeInPlace(ri), rv.resizeInPlace(ri))
//  }

  private final class JustKeys[@sp(Int, Long, Double) K: Order: ClassTag, @sp(Int, Long, Double) V: ClassTag](a: TotalArrayMap[K, V], b: ArraySet[K]) extends BinaryMerge {

    @inline def ak = a.keys0
    @inline def av = a.values0
    @inline def bk = b.elements

    val rk = new Array[K](ak.length min bk.length)
    val rv = new Array[V](ak.length min bk.length)
    var ri = 0

    def compare(ai: Int, bi: Int) = Order.compare(ak(ai), bk(bi))

    def fromA(a0: Int, a1: Int, bi: Int) = {}

    def fromB(ai: Int, b0: Int, b1: Int) = {}

    def collision(ai: Int, bi: Int) = {
      rk(ri) = ak(ai)
      rv(ri) = av(ai)
      ri += 1
    }

    merge0(0, ak.length, 0, bk.length)

    def result: TotalArrayMap[K, V] =
      if(ri == ak.length) a
      else new TotalArrayMap[K, V](rk.resizeInPlace(ri), rv.resizeInPlace(ri), a.default)
  }

  private final class ExceptKeys[@sp(Int, Long, Double) K: Order: ClassTag, @sp(Int, Long, Double) V: ClassTag](a: TotalArrayMap[K, V], b: ArraySet[K]) extends BinaryMerge {

    @inline def ak = a.keys0
    @inline def av = a.values0
    @inline def bk = b.elements

    val rk = new Array[K](ak.length)
    val rv = new Array[V](ak.length)
    var ri = 0

    def compare(ai: Int, bi: Int) = Order.compare(ak(ai), bk(bi))

    def fromA(a0: Int, a1: Int, bi: Int) = {
      System.arraycopy(ak, a0, rk, ri, a1 - a0)
      System.arraycopy(av, a0, rv, ri, a1 - a0)
      ri += a1 - a0
    }

    def fromB(ai: Int, b0: Int, b1: Int) = {}

    def collision(ai: Int, bi: Int) = {}

    merge0(0, ak.length, 0, bk.length)

    def result: TotalArrayMap[K, V] =
      if(ri == ak.length) a
      else new TotalArrayMap[K, V](rk.resizeInPlace(ri), rv.resizeInPlace(ri), a.default)
  }

  def fromDefault[@sp(Int, Long, Double) K: ClassTag, @sp(Int, Long, Double) V: ClassTag](default: V): TotalArrayMap[K, V] =
    new TotalArrayMap(Array.empty[K], Array.empty[V], default)
}
