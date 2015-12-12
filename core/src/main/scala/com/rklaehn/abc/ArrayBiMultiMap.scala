package com.rklaehn.abc

import algebra.{Order, Eq}

import scala.reflect.ClassTag
import scala.{ specialized => sp }

final class ArrayBiMultiMap[@sp(Int, Long, Double) K, @sp(Int, Long, Double) V] private[abc] (
  val kv: ArrayMultiMap[K, V],
  val vk: ArrayMultiMap[V, K]) extends NoEquals {

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

  override def toString =
    s"ArrayBiMultiMap($kv, $vk)"
}

object ArrayBiMultiMap {

  implicit def eqv[K: Eq, V: Eq]: Eq[ArrayBiMultiMap[K, V]] = Eq.by(_.kv)

  def fromMultiMap[@sp(Int, Long, Double) K: Order: ClassTag, @sp(Int, Long, Double) V: Order: ClassTag](kv: ArrayMultiMap[K, V]) =
    new ArrayBiMultiMap[K,V](kv, kv.inverse)

  def empty[@sp(Int, Long, Double) K: Order: ClassTag, @sp(Int, Long, Double) V: Order: ClassTag] =
    new ArrayBiMultiMap[K, V](ArrayMultiMap.empty[K, V], ArrayMultiMap.empty[V, K])

  def singleton[@sp(Int, Long, Double) K: Order: ClassTag, @sp(Int, Long, Double) V: Order: ClassTag](k: K, v: V) =
    new ArrayBiMultiMap[K, V](
      ArrayMultiMap.singleton[K, V](k, ArraySet.singleton(v)),
      ArrayMultiMap.singleton[V, K](v, ArraySet.singleton(k)))

  def apply[@sp(Int, Long, Double) K: Order: ClassTag, @sp(Int, Long, Double) V: Order: ClassTag](kvs: (K, V)*) = {
    new ArrayBiMultiMap[K, V](
      ArrayMultiMap.fromEntries(kvs: _*),
      ArrayMultiMap.fromEntries(kvs.map(_.swap): _*))
  }
}
