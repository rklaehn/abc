package com.rklaehn.abc

import spire.math.BinaryMerge
import spire.util.Opt

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.collection.{mutable, SortedMapLike}
import scala.collection.immutable.SortedMap
import scala.util.hashing.MurmurHash3
import scala.{ specialized => sp }
import spire.algebra.Order

final class ArrayMap[@sp(Int, Long, Double) K, @sp(Int, Long, Double) V](
  private[abc] val keys0: Array[K],
  private[abc] val values0: Array[V])(implicit val kArrayTag: OrderedArrayTag[K], val vArrayTag: ArrayTag[V])
  extends SortedMap[K, V] with SortedMapLike[K, V, ArrayMap[K, V]]
{ self ⇒
  import ArrayMap._
  import kArrayTag.order

  override def empty = ArrayMap.empty[K, V]

  override protected[this] def newBuilder: mutable.Builder[(K, V), ArrayMap[K, V]] =
    new ArrayBuffer[(K, V)].mapResult(x ⇒ ArrayMap(x: _*))

  implicit def ordering = Order.ordering(kArrayTag.order)

  def iteratorFrom(start: K) = ???

  def keysIteratorFrom(start: K) = ???

  def valuesIteratorFrom(start: K) = ???

  def rangeImpl(from: Option[K], until: Option[K]) = ???

  def iterator = keys0.iterator zip values0.iterator

  override def size: Int = keys0.length

  override def lastKey: K = keys0.last

  override def firstKey: K = keys0.head

  override def keys: ArraySet[K] = new ArraySet[K](keys0)

  override def values: ArraySeq[V] = new ArraySeq[V](values0)

  override def apply(k: K): V = {
    val i = kArrayTag.binarySearch(keys0, 0, keys0.length, k)
    if (i >= 0)
      values0(i)
    else
      throw new NoSuchElementException
  }

  def get(k: K): Option[V] = {
    val i = kArrayTag.binarySearch(keys0, 0, keys0.length, k)
    if (i < 0) None else Some(values0(i))
  }

  def +(kv: (K, V)) = updated(kv._1, kv._2)

  def updated(k: K, v: V) = merge(new ArrayMap[K, V](singletonArray(k), singletonArray(v)))

  def -(k: K) = exceptKeys(new ArraySet(singletonArray(k)))

  def merge(that: ArrayMap[K, V]): ArrayMap[K, V] =
    new MapMerger[K, V](this, that).result

  def merge(that: ArrayMap[K, V], f: (V, V) => V): ArrayMap[K, V] =
    new MapMerger2[K, V](this, that, f).result

  def except(that: ArrayMap[K, V], f: (V, V) ⇒ Opt[V]): ArrayMap[K, V] =
    new Except[K, V](this, that, f).result

  override def filterKeys(f: K ⇒ Boolean): ArrayMap[K, V] = {
    val rk = kArrayTag.newArray(keys0.length)
    val rv = vArrayTag.newArray(values0.length)
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
    new ArrayMap[K, V](kArrayTag.resize(rk, ri), vArrayTag.resize(rv, ri))
  }

  def filter(f: (K, V) ⇒ Boolean): ArrayMap[K, V] = {
    val rk = kArrayTag.newArray(keys0.length)
    val rv = vArrayTag.newArray(values0.length)
    var ri = 0
    var i = 0
    while(i < keys0.length) {
      if (f(keys0(i), values0(i))) {
        rk(ri) = keys0(i)
        rv(ri) = values0(i)
        ri += 1
      }
      i += 1
    }
    new ArrayMap[K, V](kArrayTag.resize(rk, ri), vArrayTag.resize(rv, ri))
  }

  def justKeys(keys: ArraySet[K]): ArrayMap[K, V] =
    new JustKeys[K, V](this, keys).result

  def exceptKeys(keys: ArraySet[K]): ArrayMap[K, V] =
    new ExceptKeys[K, V](this, keys).result

  def mapValues[@sp(Int, Long, Double) V2: ArrayTag](f: V => V2): ArrayMap[K, V2] = {
    new ArrayMap(keys0, values0.map(f).toArray(implicitly[ArrayTag[V2]].classTag))
  }

  override def hashCode() = MurmurHash3.mixLast(
    kArrayTag.hash(keys0),
    vArrayTag.hash(values0))

  override def equals(that: Any) = that match {
    case that: ArrayMap[K, V] =>
      kArrayTag.eqv(this.keys0, that.keys0) && vArrayTag.eqv(this.values0, that.values0)
    case _ => false
  }

  override def toString: String =
    keys0.indices.map(i => s"${keys0(i)}->${values0(i)}").mkString("Map(", ",", ")")
}

object ArrayMap {

  implicit def cbf[CC, @sp(Int, Long, Double) K: OrderedArrayTag, @sp(Int, Long, Double) V: ArrayTag]: CanBuildFrom[CC, (K, V), ArrayMap[K, V]] = new CanBuildFrom[CC, (K, V), ArrayMap[K, V]] {
    def apply(from: CC) = apply()

    def apply() = new ArrayBuffer[(K, V)].mapResult(x ⇒ ArrayMap(x: _*))
  }

  private class MapMerger[@sp(Int, Long, Double) K, @sp(Int, Long, Double) V](a: ArrayMap[K, V], b: ArrayMap[K, V]) extends BinaryMerge {

    @inline def ak = a.keys0
    @inline def av = a.values0
    @inline def bk = b.keys0
    @inline def bv = b.values0
    @inline implicit def kArrayTag = a.kArrayTag
    @inline implicit def vArrayTag = a.vArrayTag

    val rk = ak.newArray(a.size + b.size)

    val rv = av.newArray(a.size + b.size)

    var ri = 0

    def compare(ai: Int, bi: Int) = kArrayTag.compare(ak, ai, bk, bi)

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

    def result: ArrayMap[K, V] = new ArrayMap[K, V](kArrayTag.resize(rk, ri), vArrayTag.resize(rv, ri))
  }

  private class MapMerger2[@sp(Int, Long, Double) K: Order, @sp(Int, Long, Double) V](
    a: ArrayMap[K, V], b: ArrayMap[K, V], f: (V, V) => V)
    extends BinaryMerge {

    @inline def ak = a.keys0
    @inline def av = a.values0
    @inline def bk = b.keys0
    @inline def bv = b.values0
    @inline implicit def kArrayTag = a.kArrayTag
    @inline implicit def vArrayTag = a.vArrayTag

    val rk = ak.newArray(a.size + b.size)

    val rv = av.newArray(a.size + b.size)

    var ri = 0

    def compare(ai: Int, bi: Int) = kArrayTag.compare(ak, ai, bk, bi)

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

    def result: ArrayMap[K, V] = new ArrayMap[K, V](kArrayTag.resize(rk, ri), vArrayTag.resize(rv, ri))
  }

  private class Except[@sp(Int, Long, Double) K: Order, @sp(Int, Long, Double) V](
      a: ArrayMap[K, V], b: ArrayMap[K, V], f: (V, V) => Opt[V])
    extends BinaryMerge {

    @inline def ak = a.keys0
    @inline def av = a.values0
    @inline def bk = b.keys0
    @inline def bv = b.values0
    @inline implicit def kArrayTag = a.kArrayTag
    @inline implicit def vArrayTag = a.vArrayTag

    val rk = ak.newArray(a.size)

    val rv = av.newArray(a.size)

    var ri = 0

    def compare(ai: Int, bi: Int) = kArrayTag.compare(ak, ai, bk, bi)

    def fromA(a0: Int, a1: Int, bi: Int) = {
      System.arraycopy(ak, a0, rk, ri, a1 - a0)
      System.arraycopy(av, a0, rv, ri, a1 - a0)
      ri += a1 - a0
    }

    def fromB(ai: Int, b0: Int, b1: Int) = {}

    def collision(ai: Int, bi: Int) = {
      f(av(ai), bv(bi)) match {
        case Opt(v) ⇒
          rk(ri) = bk(bi)
          rv(ri) = v
          ri += 1
        case _ ⇒
      }
    }

    merge0(0, ak.length, 0, bk.length)

    def result: ArrayMap[K, V] = new ArrayMap[K, V](kArrayTag.resize(rk, ri), vArrayTag.resize(rv, ri))
  }

  private class JustKeys[@sp(Int, Long, Double) K: Order, @sp(Int, Long, Double) V](a: ArrayMap[K, V], b: ArraySet[K]) extends BinaryMerge {

    @inline def ak = a.keys0
    @inline def av = a.values0
    @inline def bk = b.elements
    @inline implicit def kArrayTag = a.kArrayTag
    @inline implicit def vArrayTag = a.vArrayTag

    val rk = ak.newArray(ak.length + bk.length)
    val rv = av.newArray(ak.length + bk.length)
    var ri = 0

    def compare(ai: Int, bi: Int) = kArrayTag.compare(ak, ai, bk, bi)

    def fromA(a0: Int, a1: Int, bi: Int) = {}

    def fromB(ai: Int, b0: Int, b1: Int) = {}

    def collision(ai: Int, bi: Int) = {
      rk(ri) = ak(bi)
      rv(ri) = av(bi)
      ri += 1
    }

    merge0(0, ak.length, 0, bk.length)

    def result: ArrayMap[K, V] = new ArrayMap[K, V](kArrayTag.resize(rk, ri), vArrayTag.resize(rv, ri))
  }

  private class ExceptKeys[@sp(Int, Long, Double) K: Order, @sp(Int, Long, Double) V](a: ArrayMap[K, V], b: ArraySet[K]) extends BinaryMerge {

    @inline def ak = a.keys0
    @inline def av = a.values0
    @inline def bk = b.elements
    @inline implicit def kArrayTag = a.kArrayTag
    @inline implicit def vArrayTag = a.vArrayTag

    val rk = ak.newArray(ak.length + bk.length)
    val rv = av.newArray(ak.length + bk.length)
    var ri = 0

    def compare(ai: Int, bi: Int) = kArrayTag.compare(ak, ai, bk, bi)

    def fromA(a0: Int, a1: Int, bi: Int) = {
      System.arraycopy(ak, a0, rk, ri, a1 - a0)
      System.arraycopy(av, a0, rv, ri, a1 - a0)
      ri += a1 - a0
    }

    def fromB(ai: Int, b0: Int, b1: Int) = {}

    def collision(ai: Int, bi: Int) = {}

    merge0(0, ak.length, 0, bk.length)

    def result: ArrayMap[K, V] = new ArrayMap[K, V](kArrayTag.resize(rk, ri), vArrayTag.resize(rv, ri))
  }

  def empty[@sp(Int, Long, Double) K, @sp(Int, Long, Double) V](
    implicit kArrayTag: OrderedArrayTag[K], vArrayTag:ArrayTag[V]): ArrayMap[K, V] = new ArrayMap(kArrayTag.empty, vArrayTag.empty)

  def singleton[@sp(Int, Long, Double) K, @sp(Int, Long, Double) V](k: K, v: V)(
    implicit kArrayTag: OrderedArrayTag[K], vArrayTag:ArrayTag[V]): ArrayMap[K, V] =
    new ArrayMap[K, V](kArrayTag.singleton(k), vArrayTag.singleton(v))

  def apply[@sp(Int, Long, Double) K, @sp(Int, Long, Double) V](
    kvs: (K, V)*)(
    implicit kArrayTag: OrderedArrayTag[K], vArrayTag:ArrayTag[V]): ArrayMap[K, V] = {
    implicit val order = kArrayTag.order
    val reducer = Reducer.create[ArrayMap[K, V]](_ merge _)
    for ((k, v) <- kvs)
      reducer.apply(singleton(k, v))
    reducer.result().getOrElse(empty[K, V])
  }
}
