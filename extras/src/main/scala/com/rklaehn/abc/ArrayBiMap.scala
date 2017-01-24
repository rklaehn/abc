package com.rklaehn.abc

import algebra.{Eq, Order}
import cats.Show

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
    val values = removedKeys.elements.map(kv.apply0)
    val vk1 = vk.exceptKeys(ArraySet(values: _*))
    new ArrayBiMap[K, V](kv1, vk1)
  }

  override def equals(that: Any): Boolean = that match {
    case that: ArrayBiMap[K, V] => ArrayBiMap.eqv(Universal[K], Universal[V]).eqv(this, that)
    case _ => false
  }

  override def hashCode: Int = ArrayBiMap.hash(Universal[K], Universal[V]).hash(this)

  override def toString = ArrayBiMap.show(Universal[K], Universal[V]).show(this)
}

object ArrayBiMap {

  implicit def eqv[K: Eq, V: Eq]: Eq[ArrayBiMap[K, V]] = Eq.by(_.kv)

  implicit def hash[K: Hash, V: Hash]: Hash[ArrayBiMap[K, V]] = Hash.by(_.kv)

  implicit def show[K: Show, V: Show]: Show[ArrayBiMap[K, V]] = Show.show(_.kv.show)

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
