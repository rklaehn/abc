//package com.rklaehn.abc
//
//import spire.math.BinaryMerge
//import spire.util.Opt
//
//final class ArrayMap2[K, V](
//    private[abc] val keys0: Array[K],
//    private[abc] val values0: Array[V]) { self ⇒
//  import ArrayMap2._
//
//  def iterator = keys0.iterator zip values0.iterator
//
//  def size: Int = keys0.length
//
//  def lastKey: K = keys0.last
//
//  def firstKey: K = keys0.head
//
//  def keys: ArraySet2[K] = new ArraySet2[K](keys0)
//
//  def values: ArraySeq2[V] = new ArraySeq2[V](values0)
//
//  def apply(k: K)(implicit kt: OrderedArrayTag[K]): V = {
//    val i = kt.binarySearch(keys0, 0, keys0.length, k)
//    if (i >= 0) values0(i)
//    else throw new NoSuchElementException
//  }
//
//  def get(k: K)(implicit kt: OrderedArrayTag[K]): Opt[V] = {
//    val i = kt.binarySearch(keys0, 0, keys0.length, k)
//    if (i < 0) Opt.empty else Opt(values0(i))
//  }
//
//  def +(kv: (K, V))(implicit kt: OrderedArrayTag[K], vt:ArrayTag[V]) = updated(kv._1, kv._2)
//
//  def updated(k: K, v: V)(implicit kt: OrderedArrayTag[K], vt:ArrayTag[V]) = merge(ArrayMap2.singleton(k, v))
//
//  def -(k: K)(implicit kt: OrderedArrayTag[K], vt:ArrayTag[V]) = exceptKeys(ArraySet2.singleton(k))
//
//  def merge(that: ArrayMap2[K, V])(implicit kt: OrderedArrayTag[K], vt:ArrayTag[V]): ArrayMap2[K, V] =
//    new MapMerger[K, V](this.keys0, this.values0, that.keys0, that.values0, kt, vt, null).result
//
//  def mergeWith(that: ArrayMap2[K, V], f: (V, V) => V)(implicit kt: OrderedArrayTag[K], vt:ArrayTag[V]): ArrayMap2[K, V] =
//    new MapMerger[K, V](this.keys0, this.values0, that.keys0, that.values0, kt, vt, f).result
//
//  def except(that: ArrayMap2[K, V], f: (V, V) ⇒ Opt[V])(implicit kt: OrderedArrayTag[K], vt:ArrayTag[V]): ArrayMap2[K, V] =
//    new Except[K, V](this.keys0, this.values0, that.keys0, that.values0, kt, vt, f).result
//
//  def filterKeys(f: K ⇒ Boolean)(implicit kt: OrderedArrayTag[K], vt:ArrayTag[V]): ArrayMap2[K, V] = {
//    val rk = kt.newArray(keys0.length)
//    val rv = vt.newArray(values0.length)
//    var ri = 0
//    var i = 0
//    while(i < keys0.length) {
//      if (f(keys0(i))) {
//        rk(ri) = keys0(i)
//        rv(ri) = values0(i)
//        ri += 1
//      }
//      i += 1
//    }
//    new ArrayMap2[K, V](kt.resizeInPlace(rk, ri), vt.resizeInPlace(rv, ri))
//  }
//
//  def filter(f: ((K, V)) ⇒ Boolean)(implicit kt: OrderedArrayTag[K], vt:ArrayTag[V]): ArrayMap2[K, V] = {
//    val rk = kt.newArray(keys0.length)
//    val rv = vt.newArray(values0.length)
//    var ri = 0
//    var i = 0
//    while(i < keys0.length) {
//      if (f((keys0(i), values0(i)))) {
//        rk(ri) = keys0(i)
//        rv(ri) = values0(i)
//        ri += 1
//      }
//      i += 1
//    }
//    new ArrayMap2[K, V](kt.resizeInPlace(rk, ri), vt.resizeInPlace(rv, ri))
//  }
//
//  def justKeys(keys: ArraySet2[K])(implicit kt: OrderedArrayTag[K], vt:ArrayTag[V]): ArrayMap2[K, V] =
//    new JustKeys[K, V](this.keys0, this.values0, keys.elements0, kt, vt).result
//
//  def exceptKeys(keys: ArraySet2[K])(implicit kt: OrderedArrayTag[K], vt:ArrayTag[V]): ArrayMap2[K, V] =
//    new ExceptKeys[K, V](this.keys0, this.values0, keys.elements0, kt, vt).result
//
//  def mapValues[V2: ArrayTag](f: V => V2): ArrayMap2[K, V2] = {
//    new ArrayMap2(keys0, values0.map(f).toArray(ArrayTag[V2].classTag))
//  }
//
//  override def toString: String =
//    keys0.indices.map(i => s"${keys0(i)}->${values0(i)}").mkString("Map(", ",", ")")
//}
//
//object ArrayMap2 {
//
//  private[this] class ArrayMap2Builder[K: OrderedArrayTag, V: ArrayTag] extends scala.collection.mutable.Builder[(K, V), ArrayMap2[K, V]] {
//
//    private[this] var reducer = Reducer.create[ArrayMap2[K, V]](_ merge _)
//
//    def +=(elem: (K, V)) = {
//      reducer.apply(singleton(elem._1, elem._2))
//      this
//    }
//
//    def clear() =
//      reducer = Reducer.create[ArrayMap2[K, V]](_ merge _)
//
//    def result() =
//      reducer.result().getOrElse(empty)
//  }
//
//  private sealed abstract class ArrayBinaryMerge[K, V] extends BinaryMerge {
//    def kt: OrderedArrayTag[K]
//    def vt: ArrayTag[V]
//    def ak: Array[K]
//    def bk: Array[K]
//    def av: Array[V]
//    def bv: Array[V]
//    def rs: Int
//    val rk = kt.newArray(rs)
//    val rv = vt.newArray(rs)
//    var ri = 0
//    def binarySearchB(ai: Int, b0: Int, b1: Int) = kt.binarySearch(bk, b0, b1, ak(ai))
//    merge0(0, ak.length, 0, bk.length)
//    def copyFromA(a0: Int, a1: Int) = {
//      System.arraycopy(ak, a0, rk, ri, a1 - a0)
//      System.arraycopy(av, a0, rv, ri, a1 - a0)
//      ri += a1 - a0
//    }
//    def copyFromB(b0: Int, b1: Int) = {
//      System.arraycopy(bk, b0, rk, ri, b1 - b0)
//      System.arraycopy(bv, b0, rv, ri, b1 - b0)
//      ri += b1 - b0
//    }
//    def result: ArrayMap2[K, V] = new ArrayMap2[K, V](kt.resizeInPlace(rk, ri), vt.resizeInPlace(rv, ri))
//  }
//
//  private final class MapMerger[K, V](
//      val ak: Array[K], val av: Array[V],
//      val bk: Array[K], val bv: Array[V],
//      val kt: OrderedArrayTag[K], val vt:ArrayTag[V],
//      f: (V, V) => V) extends ArrayBinaryMerge[K, V] {
//    def rs: Int = ak.length + bk.length
//    def fromA(a0: Int, a1: Int, bi: Int) = copyFromA(a0, a1)
//    def fromB(ai: Int, b0: Int, b1: Int) = copyFromB(b0, b1)
//    def collision(ai: Int, bi: Int) = {
//      rk(ri) = bk(bi)
//      rv(ri) = if(f eq null) bv(bi) else f(av(ai), bv(bi))
//      ri += 1
//    }
//  }
//
//  private final class Except[K, V](
//      val ak: Array[K], val av: Array[V],
//      val bk: Array[K], val bv: Array[V],
//      val kt: OrderedArrayTag[K], val vt:ArrayTag[V],
//      f: (V, V) => Opt[V]) extends ArrayBinaryMerge[K, V] {
//    def rs = ak.length
//    def fromA(a0: Int, a1: Int, bi: Int) = copyFromA(a0, a1)
//    def fromB(ai: Int, b0: Int, b1: Int) = ()
//    def collision(ai: Int, bi: Int) = {
//      f(av(ai), bv(bi)) match {
//        case Opt(v) ⇒
//          rk(ri) = bk(bi)
//          rv(ri) = v
//          ri += 1
//        case _ ⇒
//      }
//    }
//  }
//
//  private final class JustKeys[K, V](
//      val ak: Array[K], val av: Array[V],
//      val bk: Array[K],
//      val kt: OrderedArrayTag[K], val vt:ArrayTag[V]) extends ArrayBinaryMerge[K, V] {
//    def rs: Int = ak.length
//    def bv = null
//    def fromA(a0: Int, a1: Int, bi: Int) = ()
//    def fromB(ai: Int, b0: Int, b1: Int) = ()
//    def collision(ai: Int, bi: Int) = {
//      rk(ri) = ak(ai)
//      rv(ri) = av(ai)
//      ri += 1
//    }
//  }
//
//  private final class ExceptKeys[K, V](
//      val ak: Array[K], val av: Array[V],
//      val bk: Array[K],
//      val kt: OrderedArrayTag[K], val vt:ArrayTag[V]) extends ArrayBinaryMerge[K, V] {
//    def rs: Int = ak.length
//    def bv = null
//    def fromA(a0: Int, a1: Int, bi: Int) = copyFromA(a0, a1)
//    def fromB(ai: Int, b0: Int, b1: Int) = ()
//    def collision(ai: Int, bi: Int) = ()
//  }
//
//  def empty[K, V](implicit kt: OrderedArrayTag[K], vt:ArrayTag[V]): ArrayMap2[K, V] = new ArrayMap2(kt.empty, vt.empty)
//
//  def singleton[K, V](k: K, v: V)(implicit kt: OrderedArrayTag[K], vt:ArrayTag[V]): ArrayMap2[K, V] =
//    new ArrayMap2[K, V](kt.singleton(k), vt.singleton(v))
//
//  def apply[K: OrderedArrayTag, V: ArrayTag](kvs: (K, V)*): ArrayMap2[K, V] = {
//    val b = new ArrayMap2Builder[K, V]
//    b ++= kvs
//    b.result()
//  }
//}
