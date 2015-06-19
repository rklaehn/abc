package com.rklaehn.abc

import spire.algebra.{Eq, Order}
import scala.reflect.ClassTag
import scala.util.hashing.Hashing
import scala.{specialized ⇒ sp}

final class ArrayMultiMap[@sp(Int, Long, Double) K, @sp(Int, Long, Double) V] private[abc] (
    private[abc] val map: ArrayMap[K, ArraySet[V]])(
    implicit val f: ArrayMultiMap.Family[K, V]) {

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

object ArrayMultiMap extends App {

  def empty[@sp(Int, Long, Double) K, @sp(Int, Long, Double) V](implicit f: ArrayMultiMap.Family[K, V]): ArrayMultiMap[K, V] = f.empty

  def singleton[@sp(Int, Long, Double) K, @sp(Int, Long, Double) V](k: K, v: ArraySet[V])(implicit f: ArrayMultiMap.Family[K, V]): ArrayMultiMap[K, V]
    = new ArrayMultiMap[K, V](ArrayMap.singleton(k, v)(f.mapFamily))

  def apply[@sp(Int, Long, Double) K, @sp(Int, Long, Double) V](kvs: (K, ArraySet[V])*)(implicit f: ArrayMultiMap.Family[K, V]) = {
    val reducer = Reducer.create[ArrayMultiMap[K,V]](_ merge _)
    for((k, v) <- kvs)
      reducer(singleton(k, v))
    reducer.result().getOrElse(empty[K,V])
  }

  def fromKVs[@sp(Int, Long, Double) K, @sp(Int, Long, Double) V](kvs: (K, V)*)(implicit f: ArrayMultiMap.Family[K, V]) = {
    val reducer = Reducer.create[ArrayMultiMap[K,V]](_ merge _)
    for((k, v) <- kvs)
      reducer(singleton(k, ArraySet.singleton(v)(f.vSetFamily)))
    reducer.result().getOrElse(empty[K,V])
  }

  trait Family[K,V] {

    def empty: ArrayMultiMap[K, V]

    def kOrder: Order[K]

    def vOrder: Order[V]

    def mapFamily: ArrayMap.Family[K, ArraySet[V]]

    def vSetFamily: ArraySet.Family[V]
  }

  implicit def genericFamily[K, V](
      implicit mapFamily: ArrayMap.Family[K, ArraySet[V]],
      vSetFamily: ArraySet.Family[V]) : Family[K, V] =
    new GenericFamily[K, V](mapFamily, vSetFamily)

  private class GenericFamily[K, V](
      val mapFamily: ArrayMap.Family[K, ArraySet[V]],
      val vSetFamily: ArraySet.Family[V]) extends Family[K, V] {

    val kOrder = mapFamily.kOrder

    val vOrder = vSetFamily.tOrder

    val empty = new ArrayMultiMap[K,V](ArrayMap.empty[K, ArraySet[V]](mapFamily))(this)
  }
}
