package com.rklaehn.abc

import algebra.Order

final class ArrayBiMap[@sp(ILD) K, @sp(ILD) V] private[abc] (
  val kv: ArrayMap[K, V],
  val vk: ArrayMap[V, K]) {

  def swap: ArrayBiMap[V, K] = new ArrayBiMap[V, K](vk, kv)

  def merge(that: ArrayBiMap[K, V])(implicit kOrder: Order[K], kClassTag: ClassTag[K], vOrder: Order[V], vClassTag: ClassTag[V]): ArrayBiMap[K, V] =
    new ArrayBiMap(
      kv.merge(that.kv),
      vk.merge(that.vk))

  def exceptKeys(keys: ArraySet[K])(implicit kOrder: Order[K], kClassTag: ClassTag[K], vOrder: Order[V], vClassTag: ClassTag[V]): ArrayBiMap[K, V] = {
    val removedKeys = keys intersect kv.keys
    val kv1 = kv.exceptKeys(removedKeys)
    val values = removedKeys.elements.map(kv.apply)
    val vk1 = vk.exceptKeys(ArraySet(values: _*))
    new ArrayBiMap[K, V](kv1, vk1)
  }

  override def toString =
    s"ArrayBiMap($kv)"
}

object ArrayBiMap {

  implicit def hash[K: Hash, V: Hash]: Hash[ArrayBiMap[K, V]] = Hash.by(_.kv)

  def empty[@sp(ILD) K:ClassTag, @sp(ILD) V:ClassTag]: ArrayBiMap[K, V] =
    new ArrayBiMap[K, V](ArrayMap.empty[K,V], ArrayMap.empty[V, K])

  def singleton[@sp(ILD) K, @sp(ILD) V](k: K, v: V)(
    implicit kOrder: Order[K], kClassTag: ClassTag[K], vOrder: Order[V], vClassTag: ClassTag[V]): ArrayBiMap[K, V] =
    new ArrayBiMap[K, V](
      ArrayMap.singleton[K, V](k, v),
      ArrayMap.singleton[V, K](v, k))

  def apply[@sp(ILD) K, @sp(ILD) V](kvs: (K, V)*)(
    implicit kOrder: Order[K], kClassTag: ClassTag[K], vOrder: Order[V], vClassTag: ClassTag[V]): ArrayBiMap[K, V] = {
    new ArrayBiMap[K, V](
      ArrayMap(kvs: _*),
      ArrayMap(kvs.map(_.swap): _*))
  }
}
