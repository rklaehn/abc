package com.rklaehn.abc

import algebra._
import algebra.ring.{AdditiveMonoid, AdditiveSemigroup}
import cats.Show
import cats.syntax.show._
import com.rklaehn.sonicreducer.Reducer
import scala.util.hashing.MurmurHash3

final class ArrayMap[@sp(ILD) K, @sp(ILD) V](
  private[abc] val keys0: Array[K],
  private[abc] val values0: Array[V]) extends NoEquals { self ⇒
  import ArrayMap._

  def withDefaultValue(default: V)(implicit kOrder: Order[K], kClassTag: ClassTag[K], vEq: Eq[V], vClassTag: ClassTag[V]): TotalArrayMap[K, V] = {
    val keep = (x: V) ⇒ vEq.neqv(x, default)
    val filtered = self.filterValues(keep)
    new TotalArrayMap[K, V](filtered.keys0, filtered.values0, default)
  }

  def iterator = keys0.iterator zip values0.iterator

  def size: Int = keys0.length

  def keys: ArraySet[K] = new ArraySet[K](keys0)

  def values: ArraySeq[V] = new ArraySeq[V](values0)

  def apply(k: K)(implicit kOrder: Order[K]): V = {
    val i = Searching.search(keys0, 0, keys0.length, k)
    if (i >= 0) values0(i)
    else throw new NoSuchElementException
  }

  def get(k: K)(implicit kOrder: Order[K]): Option[V] = {
    val i = Searching.search(keys0, 0, keys0.length, k)
    if (i < 0) None else Some(values0(i))
  }

  def merge(that: ArrayMap[K, V])(implicit kOrder: Order[K], kClassTag: ClassTag[K], vClassTag: ClassTag[V]): ArrayMap[K, V] =
    new Merge[K, V](this, that).result

  def unionWith(that: ArrayMap[K, V], f: (V, V) ⇒ V)(implicit kOrder: Order[K], kClassTag: ClassTag[K], vClassTag: ClassTag[V]): ArrayMap[K, V] =
    new UnionWith[K, V](this, that, f).result

//  def intersectWith(that: ArrayMap[K, V], f: (V, V) ⇒ V)(implicit kOrder: Order[K], kClassTag: ClassTag[K], vClassTag: ClassTag[V]): ArrayMap[K, V] =
//    new IntersectWith[K, V](this, that, f).result

  def except(that: ArrayMap[K, V], f: (V, V) ⇒ Option[V])(implicit kOrder: Order[K], kClassTag: ClassTag[K], vClassTag: ClassTag[V]): ArrayMap[K, V] =
    new Except[K, V](this, that, f).result

  def filterKeys(f: K ⇒ Boolean)(implicit kOrder: Order[K], kClassTag: ClassTag[K], vClassTag: ClassTag[V]): ArrayMap[K, V] = {
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
    else new ArrayMap[K, V](rk.resizeInPlace(ri), rv.resizeInPlace(ri))
  }

  def filterValues(f: V ⇒ Boolean)(implicit kOrder: Order[K], kClassTag: ClassTag[K], vClassTag: ClassTag[V]): ArrayMap[K, V] = {
    val rk = new Array[K](keys0.length)
    val rv = new Array[V](values0.length)
    var ri = 0
    var i = 0
    while(i < keys0.length) {
      if (f(values0(i))) {
        rk(ri) = keys0(i)
        rv(ri) = values0(i)
        ri += 1
      }
      i += 1
    }
    if(ri == rk.length) this
    else new ArrayMap[K, V](rk.resizeInPlace(ri), rv.resizeInPlace(ri))
  }

  def filter(f: ((K, V)) ⇒ Boolean)(implicit kOrder: Order[K], kClassTag: ClassTag[K], vClassTag: ClassTag[V]): ArrayMap[K, V] = {
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
    else new ArrayMap[K, V](rk.resizeInPlace(ri), rv.resizeInPlace(ri))
  }

  def justKeys(keys: ArraySet[K])(implicit kOrder: Order[K], kClassTag: ClassTag[K], vClassTag: ClassTag[V]): ArrayMap[K, V] =
    new JustKeys[K, V](this, keys).result

  def exceptKeys(keys: ArraySet[K])(implicit kOrder: Order[K], kClassTag: ClassTag[K], vClassTag: ClassTag[V]): ArrayMap[K, V] =
    new ExceptKeys[K, V](this, keys).result

  def mapValues[@sp(ILD) V2: ClassTag](f: V => V2): ArrayMap[K, V2] =
    new ArrayMap(keys0, values0.map(f))

  override def toString: String =
    keys0.indices.map(i => s"${keys0(i)}->${values0(i)}").mkString("Map(", ",", ")")
}

private[abc] trait ArrayMap1 {

  implicit def eqv[K: Eq, V: Eq]: Eq[ArrayMap[K, V]] = new Eq[ArrayMap[K, V]] {
    def eqv(x: ArrayMap[K, V], y: ArrayMap[K, V]) = Eq[Array[K]].eqv(x.keys0, y.keys0) && Eq[Array[V]].eqv(x.values0, y.values0)
  }

  implicit def monoid[K: ClassTag : Order, V: ClassTag: Semigroup]: Monoid[ArrayMap[K, V]] =
    new MapMonoid[K, V]

  implicit def additiveMonoid[K: ClassTag: Order, V: ClassTag: AdditiveSemigroup]: AdditiveMonoid[ArrayMap[K, V]] =
    new MapAdditiveMonoid[K, V]

//  implicit def group[K: ClassTag: Order, V: ClassTag: Group: Eq]: Group[ArrayMap[K, V]] =
//    new MapGroup[K, V]
}

private class MapMonoid[K: ClassTag : Order, V: ClassTag: Semigroup] extends Monoid[ArrayMap[K, V]] {
  override def empty: ArrayMap[K, V] = ArrayMap.empty[K, V]

  override def combine(x: ArrayMap[K, V], y: ArrayMap[K, V]): ArrayMap[K, V] =
    x.unionWith(y, (x, y) ⇒ Semigroup.combine(x, y))
}

//private final class MapGroup[K: ClassTag : Order, V: ClassTag : Eq: Group] extends MapMonoid[K, V] with Group[ArrayMap[K, V]] {
//
//  def inverse(x: ArrayMap[K, V]): ArrayMap[K, V] =
//    x.mapValues(v ⇒ Group.inverse(v)).filterValues(x ⇒ Eq.neqv(x, Group.empty))
//
//  override def remove(x: ArrayMap[K, V], y: ArrayMap[K, V]): ArrayMap[K, V] = {
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

private final class MapAdditiveMonoid[K: ClassTag : Order, V: ClassTag: AdditiveSemigroup] extends AdditiveMonoid[ArrayMap[K, V]] {
  override def zero: ArrayMap[K, V] = ArrayMap.empty[K, V]

  override def plus(x: ArrayMap[K, V], y: ArrayMap[K, V]): ArrayMap[K, V] =
    x.unionWith(y, (x, y) ⇒ AdditiveSemigroup.plus(x, y))
}

object ArrayMap extends ArrayMap1 {

  implicit def show[K: Show, V: Show]: Show[ArrayMap[K, V]] = Show.show(
    _.iterator.map { case (k,v) ⇒ s"${k.show}->${v.show}" }.mkString("ArrayMap(", ",", ")")
  )

  implicit def hash[K: Hash, V: Hash]: Hash[ArrayMap[K, V]] = new Hash[ArrayMap[K, V]] {
    def hash(x: ArrayMap[K, V]) =
      MurmurHash3.mix(Hash.hash(x.keys0), Hash.hash(x.values0))

    def eqv(x: ArrayMap[K, V], y: ArrayMap[K, V]) = {
      Eq.eqv(x.keys0, y.keys0) && Eq.eqv(x.values0, y.values0)
    }
  }

  private final class Merge[@sp(ILD) K: Order: ClassTag, @sp(ILD) V: ClassTag](
    a: ArrayMap[K, V],
    b: ArrayMap[K, V]
  ) extends BinaryMerge {

    @inline def ak = a.keys0
    @inline def av = a.values0
    @inline def bk = b.keys0
    @inline def bv = b.values0

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

    def result: ArrayMap[K, V] = new ArrayMap[K, V](rk.resizeInPlace(ri), rv.resizeInPlace(ri))
  }

  private final class UnionWith[@sp(ILD) K: Order: ClassTag, @sp(ILD) V: ClassTag](
    a: ArrayMap[K, V], b: ArrayMap[K, V], f: (V, V) => V)
    extends BinaryMerge {

    @inline def ak = a.keys0
    @inline def av = a.values0
    @inline def bk = b.keys0
    @inline def bv = b.values0

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

    def result: ArrayMap[K, V] = new ArrayMap[K, V](rk.resizeInPlace(ri), rv.resizeInPlace(ri))
  }

//  private final class IntersectWith[@sp(ILD) K: Order: ClassTag, @sp(ILD) V: ClassTag](
//    a: ArrayMap[K, V], b: ArrayMap[K, V], f: (V, V) => V)
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
//    def result: ArrayMap[K, V] = new ArrayMap[K, V](rk.resizeInPlace(ri), rv.resizeInPlace(ri))
//  }

  private final class Except[@sp(ILD) K: Order: ClassTag, @sp(ILD) V: ClassTag](
      a: ArrayMap[K, V], b: ArrayMap[K, V], f: (V, V) => Option[V])
    extends BinaryMerge {

    @inline def ak = a.keys0
    @inline def av = a.values0
    @inline def bk = b.keys0
    @inline def bv = b.values0

    val rk = new Array[K](a.size)
    val rv = new Array[V](a.size)
    var ri = 0

    def compare(ai: Int, bi: Int) = Order.compare(ak(ai), bk(bi))

    def fromA(a0: Int, a1: Int, bi: Int) = {
      System.arraycopy(ak, a0, rk, ri, a1 - a0)
      System.arraycopy(av, a0, rv, ri, a1 - a0)
      ri += a1 - a0
    }

    def fromB(ai: Int, b0: Int, b1: Int) = {}

    def collision(ai: Int, bi: Int) = {
      f(av(ai), bv(bi)) match {
        case Some(v) ⇒
          rk(ri) = bk(bi)
          rv(ri) = v
          ri += 1
        case _ ⇒
      }
    }

    merge0(0, ak.length, 0, bk.length)

    def result: ArrayMap[K, V] = new ArrayMap[K, V](rk.resizeInPlace(ri), rv.resizeInPlace(ri))
  }

  private final class JustKeys[@sp(ILD) K: Order: ClassTag, @sp(ILD) V: ClassTag](a: ArrayMap[K, V], b: ArraySet[K]) extends BinaryMerge {

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

    def result: ArrayMap[K, V] =
      if(ri == ak.length) a
      else new ArrayMap[K, V](rk.resizeInPlace(ri), rv.resizeInPlace(ri))
  }

  private final class ExceptKeys[@sp(ILD) K: Order: ClassTag, @sp(ILD) V: ClassTag](a: ArrayMap[K, V], b: ArraySet[K]) extends BinaryMerge {

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

    def result: ArrayMap[K, V] =
      if(ri == ak.length) a
      else new ArrayMap[K, V](rk.resizeInPlace(ri), rv.resizeInPlace(ri))
  }

  def empty[@sp(ILD) K: ClassTag, @sp(ILD) V: ClassTag]: ArrayMap[K, V] =
    new ArrayMap(Array.empty[K], Array.empty[V])

  def singleton[@sp(ILD) K: ClassTag, @sp(ILD) V: ClassTag](k: K, v: V): ArrayMap[K, V] =
    new ArrayMap[K, V](Array.singleton(k), Array.singleton(v))

  def apply[@sp(ILD) K: Order : ClassTag, @sp(ILD) V: ClassTag](kvs: (K, V)*): ArrayMap[K, V] = {
    kvs.length match {
      case 0 ⇒ empty
      case 1 ⇒ singleton(kvs.head._1, kvs.head._2)
      case _ ⇒
        val reducer = Reducer[ArrayMap[K, V]](_ merge _)
        for((k,v) ← kvs)
          reducer(singleton(k, v))
        reducer.result
    }
  }
}
