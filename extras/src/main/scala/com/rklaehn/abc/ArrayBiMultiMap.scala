package com.rklaehn.abc

import algebra.{Eq, Order}

final class ArrayBiMultiMap[@sp(ILD) K, @sp(ILD) V] private[abc] (
  val kv: ArrayMultiMap[K, V],
  val vk: ArrayMultiMap[V, K]) {

  def swap: ArrayBiMultiMap[V, K] = new ArrayBiMultiMap[V, K](vk, kv)

  def merge(that: ArrayBiMultiMap[K, V])(implicit kOrder: Order[K], kClassTag: ClassTag[K], vOrder: Order[V], vClassTag: ClassTag[V]): ArrayBiMultiMap[K, V] =
    new ArrayBiMultiMap(
      kv.merge(that.kv),
      vk.merge(that.vk))
  //
  //  def updated(k: K, v: V) = merge(ArrayBidiMap.single(k, v)(kv.f, vk.f))
  //
  //  def -(k: K) = removeKeys(ArraySet(k)(vk.f.vSetFamily))

  def except(that: ArrayBiMultiMap[K, V])(implicit kOrder: Order[K], kClassTag: ClassTag[K], vOrder: Order[V], vClassTag: ClassTag[V]): ArrayBiMultiMap[K, V] = {
    val kv1 = kv.except(that.kv)
    ArrayBiMultiMap.fromMultiMap(kv1)
  }

  def exceptValues(values: ArraySet[V])(implicit kOrder: Order[K], kClassTag: ClassTag[K], vOrder: Order[V], vClassTag: ClassTag[V]): ArrayBiMultiMap[K, V] = {
    swap.exceptKeys(values).swap
  }

  def exceptKeys(keys: ArraySet[K])(implicit kOrder: Order[K], kClassTag: ClassTag[K], vOrder: Order[V], vClassTag: ClassTag[V]): ArrayBiMultiMap[K, V] = {
    val removedKeys = keys intersect kv.keys
    val kv1 = kv.exceptKeys(removedKeys)
    ArrayBiMultiMap.fromMultiMap(kv1)
  }

  override def equals(that: Any): Boolean = that match {
    case that: ArrayBiMultiMap[K, V] => ArrayBiMultiMap.eqv(Universal[K], Universal[V]).eqv(this, that)
    case _ => false
  }
}

object ArrayBiMultiMap {

  implicit def eqv[K: Eq, V: Eq]: Eq[ArrayBiMultiMap[K, V]] = Eq.by(_.kv)

  def fromMultiMap[@sp(ILD) K: Order: ClassTag, @sp(ILD) V: Order: ClassTag](kv: ArrayMultiMap[K, V]) =
    new ArrayBiMultiMap[K,V](kv, kv.inverse)

  def empty[@sp(ILD) K: Order: ClassTag, @sp(ILD) V: Order: ClassTag] =
    new ArrayBiMultiMap[K, V](ArrayMultiMap.empty[K, V], ArrayMultiMap.empty[V, K])

  def singleton[@sp(ILD) K: Order: ClassTag, @sp(ILD) V: Order: ClassTag](k: K, v: V) =
    new ArrayBiMultiMap[K, V](
      ArrayMultiMap.singleton[K, V](k, ArraySet.singleton(v)),
      ArrayMultiMap.singleton[V, K](v, ArraySet.singleton(k)))

  def apply[@sp(ILD) K: Order: ClassTag, @sp(ILD) V: Order: ClassTag](kvs: (K, V)*) = {
    new ArrayBiMultiMap[K, V](
      ArrayMultiMap.fromEntries(kvs: _*),
      ArrayMultiMap.fromEntries(kvs.map(_.swap): _*))
  }
}
