package com.rklaehn.abc

import algebra.{Eq, Order}
import cats.Show
import cats.implicits._
import com.rklaehn.sonicreducer.Reducer

final class ArrayMultiMap[@sp(ILD) K, @sp(ILD) V] private[abc] (
  private[abc] val map: ArrayMap[K, ArraySet[V]]) {

  def keys: ArraySet[K] = map.keys

  def entries: Iterator[(K, V)] =
    map.iterator.flatMap { case (k, vs) ⇒ vs.iterator.map(v ⇒ k → v) }

  def justKeys(keys: ArraySet[K])(implicit kOrder: Order[K], kClassTag: ClassTag[K], vOrder: Order[V], vClassTag: ClassTag[V]): ArrayMultiMap[K, V] =
    new ArrayMultiMap[K, V](map.justKeys(keys))

  def exceptKeys(keys: ArraySet[K])(implicit kOrder: Order[K], kClassTag: ClassTag[K], vOrder: Order[V], vClassTag: ClassTag[V]): ArrayMultiMap[K, V] =
    new ArrayMultiMap[K, V](map.exceptKeys(keys))

  def filterKeys(p: K ⇒ Boolean)(implicit kOrder: Order[K], kClassTag: ClassTag[K], vOrder: Order[V], vClassTag: ClassTag[V]): ArrayMultiMap[K, V] =
    new ArrayMultiMap[K, V](map.filterKeys(p))

  def merge(that: ArrayMultiMap[K, V])(implicit kOrder: Order[K], vOrder: Order[V]): ArrayMultiMap[K, V] = {
    def mergeElements(a: ArraySet[V], b: ArraySet[V]): ArraySet[V] = a.union(b)
    new ArrayMultiMap[K, V](map.unionWith(that.map, mergeElements))
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

  def apply(k: K)(implicit kOrder: Order[K]): ArraySet[V] = map.apply0(k)

  override def equals(that: Any): Boolean = that match {
    case that: ArrayMultiMap[K, V] => ArrayMultiMap.eqv(Universal[K], Universal[V]).eqv(this, that)
    case _ => false
  }

  override def hashCode(): Int = ArrayMultiMap.hash(Universal[K], Universal[V]).hash(this)

  override def toString: String = ArrayMultiMap.show(Universal[K], Universal[V]).show(this)
}

private[abc] trait ArrayMultiMap0 {

  implicit def eqv[K: Eq, V: Eq]: Eq[ArrayMultiMap[K, V]] = Eq.by(_.map)
}

object ArrayMultiMap extends ArrayMultiMap0 {

  implicit def show[K: Show, V: Show]: Show[ArrayMultiMap[K, V]] = Show.show {
    _.map
      .iterator
      .map { case (k, v) => k.show + "->" + v.show }
      .mkString("ArrayMultiMap(",",",")")
  }

  implicit def hash[K: Hash, V: Hash]: Hash[ArrayMultiMap[K, V]] = Hash.by(_.map)

  def empty[@sp(ILD) K, @sp(ILD) V]: ArrayMultiMap[K, V] =
    new ArrayMultiMap[K, V](ArrayMap.empty[K, ArraySet[V]])

  def singleton[@sp(ILD) K, @sp(ILD) V](k: K, v: ArraySet[V]) = {
    new ArrayMultiMap[K, V](ArrayMap.singleton(k, v))
  }

  def apply[@sp(ILD) K: Order: ClassTag, @sp(ILD) V: Order: ClassTag](kvs: (K, ArraySet[V])*) = {
    val reducer = Reducer[ArrayMultiMap[K, V]](_ merge _)
    for ((k, v) <- kvs)
      if(!v.isEmpty)
        reducer(singleton(k, v))
    reducer.resultOrElse(empty[K, V])
  }

  def fromEntries[@sp(ILD) K: Order, @sp(ILD) V: Order](kvs: (K, V)*) = {
    val reducer = Reducer[ArrayMultiMap[K, V]](_ merge _)
    for ((k, v) <- kvs)
      reducer(singleton(k, ArraySet.singleton(v)))
    reducer.resultOrElse(empty[K, V])
  }
}
