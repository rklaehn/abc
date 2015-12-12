package com.rklaehn.abc

import algebra.{Order, Eq}

import scala.reflect.ClassTag
import scala.{ specialized => sp }

final class ArrayBiMap[@sp(Int, Long, Double) K, @sp(Int, Long, Double) V] private[abc] (
  val kv: ArrayMap[K, V],
  val vk: ArrayMap[V, K]) extends NoEquals {

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

  def empty[@sp(Int, Long, Double) K:ClassTag, @sp(Int, Long, Double) V:ClassTag]: ArrayBiMap[K, V] =
    new ArrayBiMap[K, V](ArrayMap.empty[K,V], ArrayMap.empty[V, K])

  def singleton[@sp(Int, Long, Double) K, @sp(Int, Long, Double) V](k: K, v: V)(
    implicit kOrder: Order[K], kClassTag: ClassTag[K], vOrder: Order[V], vClassTag: ClassTag[V]): ArrayBiMap[K, V] =
    new ArrayBiMap[K, V](
      ArrayMap.singleton[K, V](k, v),
      ArrayMap.singleton[V, K](v, k))

  def apply[@sp(Int, Long, Double) K, @sp(Int, Long, Double) V](kvs: (K, V)*)(
    implicit kOrder: Order[K], kClassTag: ClassTag[K], vOrder: Order[V], vClassTag: ClassTag[V]): ArrayBiMap[K, V] = {
    new ArrayBiMap[K, V](
      ArrayMap(kvs: _*),
      ArrayMap(kvs.map(_.swap): _*))
  }
}
