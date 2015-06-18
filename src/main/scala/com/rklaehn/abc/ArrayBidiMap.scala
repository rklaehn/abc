package com.rklaehn.abc

import spire.algebra.Eq

import scala.{specialized => sp}

class ArrayBidiMap[@sp(Int, Long, Double) K, @sp(Int, Long, Double) V] private[abc] (
    val kv:ArrayMultiMap[K,V],
    val vk:ArrayMultiMap[V,K]) {

  def swap: ArrayBidiMap[V, K] = new ArrayBidiMap[V, K](vk, kv)

  def merge(that: ArrayBidiMap[K, V]): ArrayBidiMap[K,V] =
    new ArrayBidiMap(
      kv.merge(that.kv),
      vk.merge(that.vk))
//
//  def updated(k: K, v: V) = merge(ArrayBidiMap.single(k, v)(kv.f, vk.f))
//
//  def -(k: K) = removeKeys(ArraySet(k)(vk.f.vSetFamily))

  def removeKeys(keys: ArraySet[K]): ArrayBidiMap[K,V] = {
    val keys1 = keys intersection kv.keys
    val kv1 = kv.filterNotKeys(keys1)
    var s = ArraySet.empty[V](vk.f.mapFamily.kSetFamily)
    for(k <- keys1.elements)
      s = s.union(kv.apply(k))
    val vk1 = vk.filterNotKeys(s)
    new ArrayBidiMap[K, V](kv1, vk1)
  }

  override def equals(that: Any)= that match {
    case that: ArrayBidiMap[K, V] => this.kv == that.kv
    case _ => false
  }

  override def hashCode =
    kv.hashCode

  override def toString =
    s"ArrayBidiMap($kv)"
}

object ArrayBidiMap {

  def empty[@sp(Int, Long, Double) K, @sp(Int, Long, Double) V](implicit fkv: ArrayMultiMap.Family[K, V], fvk: ArrayMultiMap.Family[V, K]): ArrayBidiMap[K, V] =
    new ArrayBidiMap[K, V](fkv.empty, fvk.empty)

  def single[@sp(Int, Long, Double) K, @sp(Int, Long, Double) V](k: K, v: V)(implicit fkv: ArrayMultiMap.Family[K, V], fvk: ArrayMultiMap.Family[V, K]): ArrayBidiMap[K, V] =
    new ArrayBidiMap[K, V](
      ArrayMultiMap.single[K,V](k, ArraySet.singleton(v)(fvk.mapFamily.kSetFamily)),
      ArrayMultiMap.single[V,K](v, ArraySet.singleton(k)(fkv.mapFamily.kSetFamily))
    )

  def apply[@sp(Int, Long, Double) K, @sp(Int, Long, Double) V](kvs: (K, V)*)(implicit fkv: ArrayMultiMap.Family[K, V], fvk: ArrayMultiMap.Family[V, K]): ArrayBidiMap[K, V] = {
    new ArrayBidiMap[K, V](
      ArrayMultiMap.fromKVs(kvs: _*)(fkv),
      ArrayMultiMap.fromKVs(kvs.map(_.swap): _*)(fvk)
    )
  }

  implicit class Equals[K, V](v: ArrayBidiMap[K, V])(implicit m: Eq[ArrayMultiMap[K, V]]) extends Eq[ArrayBidiMap[K,V]] {

    def eqv(x: ArrayBidiMap[K, V], y: ArrayBidiMap[K, V]) = m.eqv(x.kv, y.kv)
  }
}
