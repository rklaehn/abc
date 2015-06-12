package com.rklaehn.abc

import spire.math.BinaryMerge

import scala.reflect.ClassTag
import scala.{specialized => sp}
import spire.algebra.Order

final class Map[@sp K, @sp V](private val _keys: Array[K], private val _values:Array[V]) {
  import Map._
//
//  private implicit def keyClassTag: ClassTag[K] = ClassTag[K](_keys.getClass.getComponentType)
//
//  private implicit def valueClassTag: ClassTag[V] = ClassTag[V](_values.getClass.getComponentType)

  def size: Int = _keys.length

  def keys: Set[K] = new Set[K](_keys)

  def values: Seq[V] = new Seq[V](_values)

  def apply(k: K)(implicit o:Order[K]): V = {
    val i = SetOps.binarySearch(_keys, k, 0, _keys.length)
    if(i >= 0)
      _values(i)
    else
      throw new NoSuchElementException
  }

  def get(k: K)(implicit o:Order[K]): Option[V] = {
    val i = SetOps.binarySearch(_keys, k, 0, _keys.length)
    if(i < 0) None else Some(_values(i))
  }

  def +(kv: (K, V))(implicit o:Order[K]) = update(kv._1, kv._2)

  def update(k: K, v: V)(implicit o:Order[K]) = merge(new Map[K,V](singletonArray(k), singletonArray(v)))

  def -(k: K)(implicit o:Order[K]) = filterNotKeys(new Set(singletonArray(k)))

  def merge(that: Map[K, V])(implicit o:Order[K]): Map[K, V] =
    new MapMerger[K, V](this, that).result

  def merge(that: Map[K, V], f: (V, V) => V)(implicit o:Order[K]): Map[K, V] =
    new MapMerger2[K,V](this, that, f).result
  
  def filterKeys(keys: Set[K])(implicit o:Order[K]): Map[K, V] =
    new FilterKeys[K, V](this, keys).result

  def filterNotKeys(keys: Set[K])(implicit o:Order[K]): Map[K, V] =
    new FilterNotKeys[K, V](this, keys).result

  def mapValues[@sp V2: ClassTag](f: V => V2): Map[K, V2] = {
    new Map(_keys, _values.map(f).toArray)
  }

  override def toString: String =
    _keys.indices.map(i => s"${_keys(i)}->${_values(i)}").mkString("Map(", ",", ")")
}

object Map {

  private class MapMerger[@sp K: Order, @sp V](a: Map[K, V], b: Map[K, V]) extends BinaryMerge {

    @inline def ak = a._keys
    @inline def av = a._values
    @inline def bk = b._keys
    @inline def bv = b._values

    val rk = ak.newArray(a.size + b.size)

    val rv = av.newArray(a.size + b.size)

    var ri = 0

    def compare(ai: Int, bi: Int) = implicitly[Order[K]].compare(ak(ai), bk(bi))

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
    
    def result: Map[K, V] = new Map[K,V](rk.resizeInPlace(ri), rv.resizeInPlace(ri))
  }

  private class MapMerger2[@sp K: Order, @sp V](a: Map[K, V], b: Map[K, V], f: (V, V) => V) extends BinaryMerge {

    @inline def ak = a._keys
    @inline def av = a._values
    @inline def bk = b._keys
    @inline def bv = b._values

    val rk = ak.newArray(a.size + b.size)

    val rv = av.newArray(a.size + b.size)

    var ri = 0

    def compare(ai: Int, bi: Int) = implicitly[Order[K]].compare(ak(ai), bk(bi))

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

    def result: Map[K, V] = new Map[K,V](rk.resizeInPlace(ri), rv.resizeInPlace(ri))
  }
  
  private class FilterKeys[@sp K: Order, @sp V](a: Map[K, V], b: Set[K]) extends BinaryMerge {

    @inline def ak = a._keys
    @inline def av = a._values
    @inline def bk = b.elements

    val rk = ak.newArray(ak.length + bk.length)
    val rv = av.newArray(ak.length + bk.length)
    var ri = 0

    def compare(ai: Int, bi: Int) = implicitly[Order[K]].compare(ak(ai), bk(bi))

    def fromA(a0: Int, a1: Int, bi: Int) = {}

    def fromB(ai: Int, b0: Int, b1: Int) = {}

    def collision(ai: Int, bi: Int) = {
      rk(ri) = ak(bi)
      rv(ri) = av(bi)
      ri += 1
    }

    merge0(0, ak.length, 0, bk.length)

    def result: Map[K, V] = new Map[K,V](rk.resizeInPlace(ri), rv.resizeInPlace(ri))
  }

  private class FilterNotKeys[@sp K: Order, @sp V](a: Map[K, V], b: Set[K]) extends BinaryMerge {

    @inline def ak = a._keys
    @inline def av = a._values
    @inline def bk = b.elements

    val rk = ak.newArray(ak.length + bk.length)
    val rv = av.newArray(ak.length + bk.length)
    var ri = 0

    def compare(ai: Int, bi: Int) = implicitly[Order[K]].compare(ak(ai), bk(bi))

    def fromA(a0: Int, a1: Int, bi: Int) = {
      System.arraycopy(ak, a0, rk, ri, a1 - a0)
      System.arraycopy(av, a0, rv, ri, a1 - a0)
      ri += a1 - a0
    }

    def fromB(ai: Int, b0: Int, b1: Int) = {}

    def collision(ai: Int, bi: Int) = {}

    merge0(0, ak.length, 0, bk.length)

    def result: Map[K, V] = new Map[K,V](rk.resizeInPlace(ri), rv.resizeInPlace(ri))
  }

  def empty[K: ClassTag, V:ClassTag]: Map[K, V] = new Map[K, V](Array.empty[K], Array.empty[V])

  def singleton[K: ClassTag, V:ClassTag](k: K, v: V): Map[K, V] = new Map[K, V](Array(k), Array(v))

  def apply[K: Order: ClassTag, V:ClassTag](kvs: (K,V)*): Map[K, V] = {
    val reducer = Reducer.create[Map[K,V]](_ merge _)
    for((k, v) <- kvs)
      reducer.apply(singleton(k, v))
    reducer.result().getOrElse(empty[K,V])
  }
}
