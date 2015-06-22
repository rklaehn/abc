package com.rklaehn.abc

import spire.math.BinaryMerge

import scala.collection.immutable.SortedMap
import scala.reflect.ClassTag
import scala.util.hashing.{ MurmurHash3, Hashing }
import scala.{ specialized => sp }
import spire.algebra.{ Eq, Order }
import spire.implicits._

final class ArrayMap[@sp(Int, Long, Double) K, @sp(Int, Long, Double) V](
  private[abc] val keys0: Array[K],
  private[abc] val values0: Array[V])(implicit val kArrayTag: OrderedArrayTag[K], val vArrayTag: ArrayTag[V]) { self ⇒
  import ArrayMap._
  import kArrayTag.tOrder

  def size: Int = keys0.length

  def keys: ArraySet[K] = new ArraySet[K](keys0)

  def values: ArraySeq[V] = new ArraySeq[V](values0)

  def asSortedMap: SortedMap[K,V] = {
    val pairs = keys0 zip values0
    SortedMap(pairs: _*)(Order.ordering(kArrayTag.tOrder))
  }

  def apply(k: K): V = {
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

  def +(kv: (K, V)) = update(kv._1, kv._2)

  def update(k: K, v: V) = merge(new ArrayMap[K, V](singletonArray(k), singletonArray(v)))

  def -(k: K) = filterNotKeys(new ArraySet(singletonArray(k)))

  def merge(that: ArrayMap[K, V]): ArrayMap[K, V] =
    new MapMerger[K, V](this, that).result

  def merge(that: ArrayMap[K, V], f: (V, V) => V): ArrayMap[K, V] =
    new MapMerger2[K, V](this, that, f).result

  def filterKeys(keys: ArraySet[K]): ArrayMap[K, V] =
    new FilterKeys[K, V](this, keys).result

  def filterNotKeys(keys: ArraySet[K]): ArrayMap[K, V] =
    new FilterNotKeys[K, V](this, keys).result

  def mapValues[@sp(Int, Long, Double) V2: ArrayTag](f: V => V2): ArrayMap[K, V2] = {
    new ArrayMap(keys0, values0.map(f).toArray(implicitly[ArrayTag[V2]].tClassTag))
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

  private class FilterKeys[@sp(Int, Long, Double) K: Order, @sp(Int, Long, Double) V](a: ArrayMap[K, V], b: ArraySet[K]) extends BinaryMerge {

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

  private class FilterNotKeys[@sp(Int, Long, Double) K: Order, @sp(Int, Long, Double) V](a: ArrayMap[K, V], b: ArraySet[K]) extends BinaryMerge {

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
    implicit val order = kArrayTag.tOrder
    val reducer = Reducer.create[ArrayMap[K, V]](_ merge _)
    for ((k, v) <- kvs)
      reducer.apply(singleton(k, v))
    reducer.result().getOrElse(empty[K, V])
  }
}
