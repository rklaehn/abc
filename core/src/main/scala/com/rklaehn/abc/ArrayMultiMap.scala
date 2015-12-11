package com.rklaehn.abc

import algebra.Order
import com.rklaehn.sonicreducer.Reducer

import scala.reflect.ClassTag
import scala.{ specialized ⇒ sp }

final class ArrayMultiMap[@sp(Int, Long, Double) K, @sp(Int, Long, Double) V] private[abc] (
  private[abc] val map: ArrayMap[K, ArraySet[V]]) extends NoEquals {

  def keys: ArraySet[K] = map.keys

  def justKeys(keys: ArraySet[K])(implicit kOrder: Order[K], kClassTag: ClassTag[K], vOrder: Order[V], vClassTag: ClassTag[V]): ArrayMultiMap[K, V] =
    new ArrayMultiMap[K, V](map.justKeys(keys))

  def exceptKeys(keys: ArraySet[K])(implicit kOrder: Order[K], kClassTag: ClassTag[K], vOrder: Order[V], vClassTag: ClassTag[V]): ArrayMultiMap[K, V] =
    new ArrayMultiMap[K, V](map.exceptKeys(keys))

  def filterKeys(p: K ⇒ Boolean)(implicit kOrder: Order[K], kClassTag: ClassTag[K], vOrder: Order[V], vClassTag: ClassTag[V]): ArrayMultiMap[K, V] =
    new ArrayMultiMap[K, V](map.filterKeys(p))

  def merge(that: ArrayMultiMap[K, V])(implicit kOrder: Order[K], kClassTag: ClassTag[K], vOrder: Order[V], vClassTag: ClassTag[V]): ArrayMultiMap[K, V] = {
    def mergeElements(a: ArraySet[V], b: ArraySet[V]): ArraySet[V] = a.union(b)
    new ArrayMultiMap[K, V](map.mergeWith(that.map, mergeElements))
  }

  def inverse(implicit kOrder: Order[K], kClassTag: ClassTag[K], vOrder: Order[V], vClassTag: ClassTag[V]): ArrayMultiMap[V, K] = {
    val swappedPairs: Iterator[(V, K)] = for {
      (k, vs) ← map.iterator
      v ← vs.iterator
    } yield (v, k)
    ArrayMultiMap.fromEntries(swappedPairs.toArray: _*)
  }

  def except(that: ArrayMultiMap[K, V])(implicit kOrder: Order[K], kClassTag: ClassTag[K], vOrder: Order[V], vClassTag: ClassTag[V]): ArrayMultiMap[K, V] = {
    val map1 = map.except(that.map, (x,y) ⇒ {
      val r = x diff y
      if(r.isEmpty) Option.empty[ArraySet[V]]
      else Option(r)
    })
    new ArrayMultiMap[K, V](map1)
  }

  def apply(k: K)(implicit kOrder: Order[K]): ArraySet[V] = map.apply(k)

  // def getOrEmpty(k: K)(implicit kOrder: Order[K], kClassTag: ClassTag[K]): ArraySet[V] = map.getOrElse(k, ArraySet.empty[V])

  override def toString: String = map.toString
}

object ArrayMultiMap {

  implicit def hash[K: Hash, V: Hash]: Hash[ArrayMultiMap[K, V]] = Hash.by(_.map)

  def empty[@sp(Int, Long, Double) K:  ClassTag, @sp(Int, Long, Double) V: ClassTag]: ArrayMultiMap[K, V] =
    new ArrayMultiMap[K, V](ArrayMap.empty[K, ArraySet[V]])

  def singleton[@sp(Int, Long, Double) K: ClassTag, @sp(Int, Long, Double) V: ClassTag](k: K, v: ArraySet[V]) = {
    new ArrayMultiMap[K, V](ArrayMap.singleton(k, v))
  }

  def apply[@sp(Int, Long, Double) K: Order: ClassTag, @sp(Int, Long, Double) V: Order: ClassTag](kvs: (K, ArraySet[V])*) = {
    val reducer = Reducer[ArrayMultiMap[K, V]](_ merge _)
    for ((k, v) <- kvs)
      if(!v.isEmpty)
        reducer(singleton(k, v))
    reducer.result().getOrElse(empty[K, V])
  }

  def fromEntries[@sp(Int, Long, Double) K: Order: ClassTag, @sp(Int, Long, Double) V: Order: ClassTag](kvs: (K, V)*) = {
    val reducer = Reducer[ArrayMultiMap[K, V]](_ merge _)
    for ((k, v) <- kvs)
      reducer(singleton(k, ArraySet.singleton(v)))
    reducer.result().getOrElse(empty[K, V])
  }
}
