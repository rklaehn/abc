package com.rklaehn.abc

import scala.{ specialized => sp }

class ArrayBiMultiMap[@sp(Int, Long, Double) K: OrderedArrayTag, @sp(Int, Long, Double) V: OrderedArrayTag] private[abc] (
  val kv: ArrayMultiMap[K, V],
  val vk: ArrayMultiMap[V, K]) {

  def swap: ArrayBiMultiMap[V, K] = new ArrayBiMultiMap[V, K](vk, kv)

  def merge(that: ArrayBiMultiMap[K, V]): ArrayBiMultiMap[K, V] =
    new ArrayBiMultiMap(
      kv.merge(that.kv),
      vk.merge(that.vk))
  //
  //  def updated(k: K, v: V) = merge(ArrayBidiMap.single(k, v)(kv.f, vk.f))
  //
  //  def -(k: K) = removeKeys(ArraySet(k)(vk.f.vSetFamily))

  def exceptKeys(keys: ArraySet[K]): ArrayBiMultiMap[K, V] = {
    val removedKeys = keys intersection kv.keys
    val kv1 = kv.exceptKeys(removedKeys)
    var s = ArraySet.empty[V]
    for (k <- removedKeys.elements)
      s = s.union(kv.apply(k))
    val vk1 = vk.exceptKeys(s)
    new ArrayBiMultiMap[K, V](kv1, vk1)
  }

  override def equals(that: Any) = that match {
    case that: ArrayBiMultiMap[K, V] => this.kv == that.kv
    case _ => false
  }

  override def hashCode =
    kv.hashCode

  override def toString =
    s"ArrayBiMultiMap($kv)"
}

object ArrayBiMultiMap {

  def empty[@sp(Int, Long, Double) K: OrderedArrayTag, @sp(Int, Long, Double) V: OrderedArrayTag] =
    new ArrayBiMultiMap[K, V](ArrayMultiMap.empty[K, V], ArrayMultiMap.empty[V, K])

  def singleton[@sp(Int, Long, Double) K: OrderedArrayTag, @sp(Int, Long, Double) V: OrderedArrayTag](k: K, v: V) =
    new ArrayBiMultiMap[K, V](
      ArrayMultiMap.singleton[K, V](k, ArraySet.singleton(v)),
      ArrayMultiMap.singleton[V, K](v, ArraySet.singleton(k)))

  def apply[@sp(Int, Long, Double) K: OrderedArrayTag, @sp(Int, Long, Double) V: OrderedArrayTag](kvs: (K, V)*) = {
    new ArrayBiMultiMap[K, V](
      ArrayMultiMap.fromKVs(kvs: _*),
      ArrayMultiMap.fromKVs(kvs.map(_.swap): _*))
  }
}
