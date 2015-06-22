package com.rklaehn.abc

import spire.algebra.{ Eq, Order }
import scala.reflect.ClassTag
import scala.util.hashing.Hashing
import scala.{ specialized ⇒ sp }

final class ArrayMultiMap[@sp(Int, Long, Double) K: OrderedArrayTag, @sp(Int, Long, Double) V: OrderedArrayTag] private[abc] (
  private[abc] val map: ArrayMap[K, ArraySet[V]]) {

  def keys: ArraySet[K] = map.keys

  def filterKeys(keys: ArraySet[K]): ArrayMultiMap[K, V] =
    new ArrayMultiMap[K, V](map.filterKeys(keys))

  def filterNotKeys(keys: ArraySet[K]): ArrayMultiMap[K, V] =
    new ArrayMultiMap[K, V](map.filterNotKeys(keys))

  def merge(that: ArrayMultiMap[K, V]): ArrayMultiMap[K, V] = {
    def mergeElements(a: ArraySet[V], b: ArraySet[V]): ArraySet[V] = a.union(b)
    new ArrayMultiMap[K, V](map.merge(that.map, mergeElements))
  }

  def apply(k: K): ArraySet[V] = map.apply(k)

  override def toString: String = map.toString
}

object ArrayMultiMap {

  implicit def arrayTag[T]: ArrayTag[ArraySet[T]] = implicitly[ArrayTag[ArraySet[T]]]

  def empty[@sp(Int, Long, Double) K: OrderedArrayTag, @sp(Int, Long, Double) V: OrderedArrayTag]: ArrayMultiMap[K, V] =
    new ArrayMultiMap[K, V](ArrayMap.empty[K, ArraySet[V]])

  def singleton[@sp(Int, Long, Double) K: OrderedArrayTag, @sp(Int, Long, Double) V: OrderedArrayTag](k: K, v: ArraySet[V]) =
    new ArrayMultiMap[K, V](ArrayMap.singleton(k, v))

  def apply[@sp(Int, Long, Double) K: OrderedArrayTag, @sp(Int, Long, Double) V: OrderedArrayTag](kvs: (K, ArraySet[V])*) = {
    val reducer = Reducer.create[ArrayMultiMap[K, V]](_ merge _)
    for ((k, v) <- kvs)
      reducer(singleton(k, v))
    reducer.result().getOrElse(empty[K, V])
  }

  def fromKVs[@sp(Int, Long, Double) K: OrderedArrayTag, @sp(Int, Long, Double) V: OrderedArrayTag](kvs: (K, V)*) = {
    val reducer = Reducer.create[ArrayMultiMap[K, V]](_ merge _)
    for ((k, v) <- kvs)
      reducer(singleton(k, ArraySet.singleton(v)))
    reducer.result().getOrElse(empty[K, V])
  }
}
