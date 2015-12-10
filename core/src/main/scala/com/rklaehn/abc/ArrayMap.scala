package com.rklaehn.abc

import com.rklaehn.sonicreducer.Reducer

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.collection.{mutable, SortedMapLike}
import scala.collection.immutable.SortedMap
import scala.util.hashing.MurmurHash3
import scala.{ specialized => sp }
import algebra.{Eq, Order}

final class ArrayMap[@sp(Int, Long, Double) K, @sp(Int, Long, Double) V](
  private[abc] val keys0: Array[K],
  private[abc] val values0: Array[V]) extends NoEquals { self ⇒
  import ArrayMap._

  // $COVERAGE-OFF$
  def asCollection(implicit kArrayTag: OrderedArrayTag[K], vArrayTag: ArrayTag[V]): AsCollection[K, V] = AsCollection.wrap(this)
  // $COVERAGE-ON$

  def iterator = keys0.iterator zip values0.iterator

  def size: Int = keys0.length

  def keys: ArraySet[K] = new ArraySet[K](keys0)

  def values: ArraySeq[V] = new ArraySeq[V](values0)

  def apply(k: K)(implicit kArrayTag: OrderedArrayTag[K]): V = {
    val i = kArrayTag.binarySearch(keys0, 0, keys0.length, k)
    if (i >= 0) values0(i)
    else throw new NoSuchElementException
  }

  def get(k: K)(implicit kArrayTag: OrderedArrayTag[K]): Option[V] = {
    val i = kArrayTag.binarySearch(keys0, 0, keys0.length, k)
    if (i < 0) None else Some(values0(i))
  }

  def merge(that: ArrayMap[K, V])(implicit kArrayTag: OrderedArrayTag[K], vArrayTag: ArrayTag[V]): ArrayMap[K, V] =
    new MapMerger[K, V](this, that).result

  def mergeWith(that: ArrayMap[K, V], f: (V, V) => V)(implicit kArrayTag: OrderedArrayTag[K], vArrayTag: ArrayTag[V]): ArrayMap[K, V] =
    new MapMerger2[K, V](this, that, f).result

  def except(that: ArrayMap[K, V], f: (V, V) ⇒ Option[V])(implicit kArrayTag: OrderedArrayTag[K], vArrayTag: ArrayTag[V]): ArrayMap[K, V] =
    new Except[K, V](this, that, f).result

  def filterKeys(f: K ⇒ Boolean)(implicit kArrayTag: OrderedArrayTag[K], vArrayTag: ArrayTag[V]): ArrayMap[K, V] = {
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
    new ArrayMap[K, V](kArrayTag.resizeInPlace(rk, ri), vArrayTag.resizeInPlace(rv, ri))
  }

  def filter(f: ((K, V)) ⇒ Boolean)(implicit kArrayTag: OrderedArrayTag[K], vArrayTag: ArrayTag[V]): ArrayMap[K, V] = {
    val rk = kArrayTag.newArray(keys0.length)
    val rv = vArrayTag.newArray(values0.length)
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
    new ArrayMap[K, V](kArrayTag.resizeInPlace(rk, ri), vArrayTag.resizeInPlace(rv, ri))
  }

  def justKeys(keys: ArraySet[K])(implicit kArrayTag: OrderedArrayTag[K], vArrayTag: ArrayTag[V]): ArrayMap[K, V] =
    new JustKeys[K, V](this, keys).result

  def exceptKeys(keys: ArraySet[K])(implicit kArrayTag: OrderedArrayTag[K], vArrayTag: ArrayTag[V]): ArrayMap[K, V] =
    new ExceptKeys[K, V](this, keys).result

  def mapValues[@sp(Int, Long, Double) V2: ArrayTag](f: V => V2): ArrayMap[K, V2] = {
    new ArrayMap(keys0, values0.map(f).toArray(ArrayTag[V2].classTag))
  }

  override def toString: String =
    keys0.indices.map(i => s"${keys0(i)}->${values0(i)}").mkString("Map(", ",", ")")
}

object ArrayMap {

  implicit def hash[K: ArrayTag, V: ArrayTag]: Hash[ArrayMap[K, V]] = new Hash[ArrayMap[K, V]] {
    def hash(x: ArrayMap[K, V]) =
      MurmurHash3.mix(ArrayTag[K].hash(x.keys0), ArrayTag[V].hash(x.values0))

    def eqv(x: ArrayMap[K, V], y: ArrayMap[K, V]) =
      ArrayTag[K].eqv(x.keys0, y.keys0) && ArrayTag[V].eqv(x.values0, y.values0)
  }

//  implicit def eqv[K: Eq, V: Eq]: Eq[ArrayMap[K, V]] = new Eq[ArrayMap[K, V]] {
//    def eqv(x: ArrayMap[K, V], y: ArrayMap[K, V]) = Eq[Array[K]].eqv(x.keys0, y.keys0) && Eq[Array[V]].eqv(x.values0, y.values0)
//  }

  // $COVERAGE-OFF$
  class AsCollection[K, V](underlying: ArrayMap[K, V])(implicit kArrayTag: OrderedArrayTag[K], vArrayTag: ArrayTag[V]) extends SortedMap[K, V] with SortedMapLike[K, V, AsCollection[K, V]] {
    import AsCollection._

    implicit def ordering = Order.ordering(kArrayTag.order)

    override def newBuilder : mutable.Builder[(K, V), AsCollection[K, V]] =
      new ArrayMapBuilder[K, V].mapResult(x ⇒ wrap(x))

    override def empty = wrap(ArrayMap.empty[K, V])

    def valuesIteratorFrom(start: K) = ???

    def rangeImpl(from: Option[K], until: Option[K]) = ???

    def iteratorFrom(start: K) = ???

    def get(key: K) = underlying.get(key)

    def iterator = underlying.iterator

    override def +[V1 >: V](kv: (K, V1)) = {
      try {
        val k = kv._1
        val v = kv._2.asInstanceOf[V]
        wrap(underlying.merge(ArrayMap.singleton(k, v)))
      } catch {
        case _: ClassCastException ⇒
          val k = kv._1
          val v = kv._2.asInstanceOf[AnyRef]
          wrap(underlying.mapValues(_.asInstanceOf[AnyRef]).merge(ArrayMap.singleton(k, v)))
            .asInstanceOf[SortedMap[K, V1]]
      }
    }

    def -(key: K) = wrap(underlying.exceptKeys(ArraySet.singleton(key)))

    def keysIteratorFrom(start: K) = ???
  }

  object AsCollection {

    implicit def cbf[CC, @sp(Int, Long, Double) K: OrderedArrayTag, @sp(Int, Long, Double) V: ArrayTag]: CanBuildFrom[CC, (K, V), AsCollection[K, V]] = new CanBuildFrom[CC, (K, V), AsCollection[K, V]] {
      def apply(from: CC) = apply()

      def apply() = new ArrayBuffer[(K, V)].mapResult(x ⇒ AsCollection.wrap(ArrayMap(x: _*)))
    }

    private[abc] def wrap[K, V](underlying: ArrayMap[K, V])(implicit kArrayTag: OrderedArrayTag[K], vArrayTag: ArrayTag[V]): AsCollection[K, V] =
      new AsCollection[K, V](underlying)
  }
  // $COVERAGE-ON$

  // $COVERAGE-OFF$
  private[this] class ArrayMapBuilder[@sp(Int, Long, Double) K: OrderedArrayTag, @sp(Int, Long, Double) V: ArrayTag] extends scala.collection.mutable.Builder[(K, V), ArrayMap[K, V]] {

    private[this] var reducer = Reducer[ArrayMap[K, V]](_ merge _)

    def +=(elem: (K, V)) = {
      reducer.apply(singleton(elem._1, elem._2))
      this
    }

    def clear() =
      reducer = Reducer[ArrayMap[K, V]](_ merge _)

    def result() =
      reducer.result.getOrElse(empty)
  }
  // $COVERAGE-ON$

  private class MapMerger[
    @sp(Int, Long, Double) K,
    @sp(Int, Long, Double) V
  ](
    a: ArrayMap[K, V],
    b: ArrayMap[K, V]
  )( implicit
    kArrayTag: OrderedArrayTag[K],
    vArrayTag: ArrayTag[V]
  ) extends BinaryMerge {

    @inline def ak = a.keys0
    @inline def av = a.values0
    @inline def bk = b.keys0
    @inline def bv = b.values0

    val rk = kArrayTag.newArray(a.size + b.size)
    val rv = vArrayTag.newArray(a.size + b.size)
    var ri = 0

    def binarySearchB(ai: Int, b0: Int, b1: Int) = kArrayTag.binarySearch(bk, b0, b1, ak(ai))

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

    def result: ArrayMap[K, V] = new ArrayMap[K, V](kArrayTag.resizeInPlace(rk, ri), vArrayTag.resizeInPlace(rv, ri))
  }

  private class MapMerger2[@sp(Int, Long, Double) K, @sp(Int, Long, Double) V](
    a: ArrayMap[K, V], b: ArrayMap[K, V], f: (V, V) => V)(implicit kArrayTag: OrderedArrayTag[K], vArrayTag: ArrayTag[V])
    extends BinaryMerge {

    @inline def ak = a.keys0
    @inline def av = a.values0
    @inline def bk = b.keys0
    @inline def bv = b.values0

    val rk = kArrayTag.newArray(a.size + b.size)

    val rv = vArrayTag.newArray(a.size + b.size)

    var ri = 0

    def binarySearchB(ai: Int, b0: Int, b1: Int) = kArrayTag.binarySearch(bk, b0, b1, ak(ai))

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

    def result: ArrayMap[K, V] = new ArrayMap[K, V](kArrayTag.resizeInPlace(rk, ri), vArrayTag.resizeInPlace(rv, ri))
  }

  private class Except[@sp(Int, Long, Double) K, @sp(Int, Long, Double) V](
      a: ArrayMap[K, V], b: ArrayMap[K, V], f: (V, V) => Option[V])(implicit kArrayTag: OrderedArrayTag[K], vArrayTag: ArrayTag[V])
    extends BinaryMerge {

    @inline def ak = a.keys0
    @inline def av = a.values0
    @inline def bk = b.keys0
    @inline def bv = b.values0

    val rk = kArrayTag.newArray(a.size)

    val rv = vArrayTag.newArray(a.size)

    var ri = 0

    def binarySearchB(ai: Int, b0: Int, b1: Int) = kArrayTag.binarySearch(bk, b0, b1, ak(ai))

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

    def result: ArrayMap[K, V] = new ArrayMap[K, V](kArrayTag.resizeInPlace(rk, ri), vArrayTag.resizeInPlace(rv, ri))
  }

  private class JustKeys[@sp(Int, Long, Double) K, @sp(Int, Long, Double) V](a: ArrayMap[K, V], b: ArraySet[K])(implicit kArrayTag: OrderedArrayTag[K], vArrayTag: ArrayTag[V]) extends BinaryMerge {

    @inline def ak = a.keys0
    @inline def av = a.values0
    @inline def bk = b.elements

    val rk = kArrayTag.newArray(ak.length + bk.length)
    val rv = vArrayTag.newArray(ak.length + bk.length)
    var ri = 0

    def binarySearchB(ai: Int, b0: Int, b1: Int) = kArrayTag.binarySearch(bk, b0, b1, ak(ai))

    def fromA(a0: Int, a1: Int, bi: Int) = {}

    def fromB(ai: Int, b0: Int, b1: Int) = {}

    def collision(ai: Int, bi: Int) = {
      rk(ri) = ak(ai)
      rv(ri) = av(ai)
      ri += 1
    }

    merge0(0, ak.length, 0, bk.length)

    def result: ArrayMap[K, V] = new ArrayMap[K, V](kArrayTag.resizeInPlace(rk, ri), vArrayTag.resizeInPlace(rv, ri))
  }

  private class ExceptKeys[@sp(Int, Long, Double) K, @sp(Int, Long, Double) V](a: ArrayMap[K, V], b: ArraySet[K])(implicit kArrayTag: OrderedArrayTag[K], vArrayTag: ArrayTag[V]) extends BinaryMerge {

    @inline def ak = a.keys0
    @inline def av = a.values0
    @inline def bk = b.elements

    val rk = kArrayTag.newArray(ak.length + bk.length)
    val rv = vArrayTag.newArray(ak.length + bk.length)
    var ri = 0

    def binarySearchB(ai: Int, b0: Int, b1: Int) = kArrayTag.binarySearch(bk, b0, b1, ak(ai))

    def fromA(a0: Int, a1: Int, bi: Int) = {
      System.arraycopy(ak, a0, rk, ri, a1 - a0)
      System.arraycopy(av, a0, rv, ri, a1 - a0)
      ri += a1 - a0
    }

    def fromB(ai: Int, b0: Int, b1: Int) = {}

    def collision(ai: Int, bi: Int) = {}

    merge0(0, ak.length, 0, bk.length)

    def result: ArrayMap[K, V] = new ArrayMap[K, V](kArrayTag.resizeInPlace(rk, ri), vArrayTag.resizeInPlace(rv, ri))
  }

  def empty[@sp(Int, Long, Double) K, @sp(Int, Long, Double) V](
    implicit kArrayTag: OrderedArrayTag[K], vArrayTag:ArrayTag[V]): ArrayMap[K, V] = new ArrayMap(kArrayTag.empty, vArrayTag.empty)

  def singleton[@sp(Int, Long, Double) K, @sp(Int, Long, Double) V](k: K, v: V)(
    implicit kArrayTag: OrderedArrayTag[K], vArrayTag:ArrayTag[V]): ArrayMap[K, V] =
    new ArrayMap[K, V](kArrayTag.singleton(k), vArrayTag.singleton(v))

  def apply[@sp(Int, Long, Double) K: OrderedArrayTag, @sp(Int, Long, Double) V: ArrayTag](kvs: (K, V)*): ArrayMap[K, V] = {
    val b = new ArrayMapBuilder[K, V]
    b ++= kvs
    b.result()
  }
}
