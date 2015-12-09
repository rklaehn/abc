package com.rklaehn.abc

import algebra.Eq
import com.rklaehn.sonicreducer.Reducer

import scala.{ specialized ⇒ sp }

final class ArrayMultiMap[@sp(Int, Long, Double) K, @sp(Int, Long, Double) V] private[abc] (
  private[abc] val map: ArrayMap[K, ArraySet[V]]) {

  def keys: ArraySet[K] = map.keys

  def justKeys(keys: ArraySet[K])(implicit kArrayTag: OrderedArrayTag[K], vArrayTag: OrderedArrayTag[V]): ArrayMultiMap[K, V] =
    new ArrayMultiMap[K, V](map.justKeys(keys))

  def exceptKeys(keys: ArraySet[K])(implicit kArrayTag: OrderedArrayTag[K], vArrayTag: OrderedArrayTag[V]): ArrayMultiMap[K, V] =
    new ArrayMultiMap[K, V](map.exceptKeys(keys))

  def filterKeys(p: K ⇒ Boolean)(implicit kArrayTag: OrderedArrayTag[K], vArrayTag: OrderedArrayTag[V]): ArrayMultiMap[K, V] =
    new ArrayMultiMap[K, V](map.filterKeys(p))

  def merge(that: ArrayMultiMap[K, V])(implicit kArrayTag: OrderedArrayTag[K], vArrayTag: OrderedArrayTag[V]): ArrayMultiMap[K, V] = {
    def mergeElements(a: ArraySet[V], b: ArraySet[V]): ArraySet[V] = a.union(b)
    new ArrayMultiMap[K, V](map.mergeWith(that.map, mergeElements))
  }

  def inverse: ArrayMultiMap[V, K] = {
    ???
//    val swappedPairs = for {
//      (k, vs) ← map
//      v ← vs
//    } yield (v, k)
//    ArrayMultiMap.fromKVs(swappedPairs: _*)
  }

  def except(that: ArrayMultiMap[K, V])(implicit kArrayTag: OrderedArrayTag[K], vArrayTag: OrderedArrayTag[V]): ArrayMultiMap[K, V] = {
    val map1 = map.except(that.map, (x,y) ⇒ {
      val r = x diff y
      if(r.isEmpty) Option.empty[ArraySet[V]]
      else Option(r)
    })
    new ArrayMultiMap[K, V](map1)
  }

  def apply(k: K)(implicit kArrayTag: OrderedArrayTag[K]): ArraySet[V] = map.apply(k)

  // def getOrEmpty(k: K)(implicit kArrayTag: OrderedArrayTag[K]): ArraySet[V] = map.getOrElse(k, ArraySet.empty[V])

  override def toString: String = map.toString
}

object ArrayMultiMap {

  // implicit def eqv[K: ArrayTag, V: ArrayTag]: Eq[ArrayMultiMap[K, V]] = Eq.by[ArrayMultiMap[K, V], ArrayMap[K, ArraySet[V]]](_.map)(ArrayMap.eqv[K, ArraySet[V]])

  def empty[@sp(Int, Long, Double) K: OrderedArrayTag, @sp(Int, Long, Double) V: OrderedArrayTag]: ArrayMultiMap[K, V] =
    new ArrayMultiMap[K, V](ArrayMap.empty[K, ArraySet[V]])

  def singleton[@sp(Int, Long, Double) K: OrderedArrayTag, @sp(Int, Long, Double) V: OrderedArrayTag](k: K, v: ArraySet[V]) =
    new ArrayMultiMap[K, V](ArrayMap.singleton(k, v))

  def apply[@sp(Int, Long, Double) K: OrderedArrayTag, @sp(Int, Long, Double) V: OrderedArrayTag](kvs: (K, ArraySet[V])*) = {
    val reducer = Reducer[ArrayMultiMap[K, V]](_ merge _)
    for ((k, v) <- kvs)
      reducer(singleton(k, v))
    reducer.result().getOrElse(empty[K, V])
  }

  def fromEntries[@sp(Int, Long, Double) K: OrderedArrayTag, @sp(Int, Long, Double) V: OrderedArrayTag](kvs: (K, V)*) = {
    val reducer = Reducer[ArrayMultiMap[K, V]](_ merge _)
    for ((k, v) <- kvs)
      reducer(singleton(k, ArraySet.singleton(v)))
    reducer.result().getOrElse(empty[K, V])
  }
}
