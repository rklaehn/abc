package com.rklaehn.abc

import scala.{specialized => sp}

class ArrayBiMultiMap[@sp(Int, Long, Double) K, @sp(Int, Long, Double) V] private[abc] (
    val kv:ArrayMultiMap[K,V],
    val vk:ArrayMultiMap[V,K]) {

  def swap: ArrayBiMultiMap[V, K] = new ArrayBiMultiMap[V, K](vk, kv)

  def merge(that: ArrayBiMultiMap[K, V]): ArrayBiMultiMap[K,V] =
    new ArrayBiMultiMap(
      kv.merge(that.kv),
      vk.merge(that.vk))
//
//  def updated(k: K, v: V) = merge(ArrayBidiMap.single(k, v)(kv.f, vk.f))
//
//  def -(k: K) = removeKeys(ArraySet(k)(vk.f.vSetFamily))

  def removeKeys(keys: ArraySet[K]): ArrayBiMultiMap[K,V] = {
    val keys1 = keys intersection kv.keys
    val kv1 = kv.filterNotKeys(keys1)
    var s = ArraySet.empty[V](vk.f.mapFamily.kSetFamily)
    for(k <- keys1.elements)
      s = s.union(kv.apply(k))
    val vk1 = vk.filterNotKeys(s)
    new ArrayBiMultiMap[K, V](kv1, vk1)
  }

  override def equals(that: Any)= that match {
    case that: ArrayBiMultiMap[K, V] => this.kv == that.kv
    case _ => false
  }

  override def hashCode =
    kv.hashCode

  override def toString =
    s"ArrayBiMultiMap($kv)"
}

object ArrayBiMultiMap {

  def empty[@sp(Int, Long, Double) K, @sp(Int, Long, Double) V](implicit fkv: ArrayMultiMap.Family[K, V], fvk: ArrayMultiMap.Family[V, K]): ArrayBiMultiMap[K, V] =
    new ArrayBiMultiMap[K, V](fkv.empty, fvk.empty)

  def single[@sp(Int, Long, Double) K, @sp(Int, Long, Double) V](k: K, v: V)(implicit fkv: ArrayMultiMap.Family[K, V], fvk: ArrayMultiMap.Family[V, K]): ArrayBiMultiMap[K, V] =
    new ArrayBiMultiMap[K, V](
      ArrayMultiMap.single[K,V](k, ArraySet.singleton(v)(fvk.mapFamily.kSetFamily)),
      ArrayMultiMap.single[V,K](v, ArraySet.singleton(k)(fkv.mapFamily.kSetFamily))
    )

  def apply[@sp(Int, Long, Double) K, @sp(Int, Long, Double) V](kvs: (K, V)*)(implicit fkv: ArrayMultiMap.Family[K, V], fvk: ArrayMultiMap.Family[V, K]): ArrayBiMultiMap[K, V] = {
    new ArrayBiMultiMap[K, V](
      ArrayMultiMap.fromKVs(kvs: _*)(fkv),
      ArrayMultiMap.fromKVs(kvs.map(_.swap): _*)(fvk)
    )
  }
}
