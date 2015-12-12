package com.rklaehn.abc

import algebra.{Order, Eq}
import com.rklaehn.sonicreducer.Reducer

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.{SortedMap, SortedSet}
import scala.collection.mutable.ArrayBuffer
import scala.collection._
import scala.reflect.ClassTag

// $COVERAGE-OFF$
object ScalaCollectionConverters {

  implicit class ArraySeqInterop[@specialized T](value: ArraySeq[T]) {
    def asCollection(implicit eq:Eq[T], hash: Hash[T], classTag: ClassTag[T]): ArraySeqCollection[T] =
      new ArraySeqCollection[T](value)
  }
  
  implicit class ArraySetInterop[@specialized T](value: ArraySet[T]) {
    def asCollection(implicit order: Order[T], classTag: ClassTag[T], hash: Hash[T]): ArraySetCollection[T] = ArraySetCollection.wrap(value)
  }

  implicit class ArrayMapInterop[@specialized K: Order: Hash: ClassTag, @specialized V: Hash: ClassTag](value: ArrayMap[K, V]) {
    def asCollection: ArrayMapCollection[K, V] = ArrayMapCollection.wrap(value)
  }
}

final class ArraySeqCollection[T : Eq: Hash: ClassTag](val underlying: ArraySeq[T]) extends IndexedSeq[T] with IndexedSeqOptimized[T, ArraySeqCollection[T]] {

  override protected[this] def newBuilder: mutable.Builder[T, ArraySeqCollection[T]] =
    new ArrayBuffer[T].mapResult(x ⇒ new ArraySeqCollection(new ArraySeq(x.toArray)))

  def apply(idx: Int) = underlying.apply(idx)

  def length = underlying.length

  override def equals(that: Any) = that match {
    case that: ArraySeqCollection[T] => Eq.eqv(this.underlying.elements, that.underlying.elements)
    case _ => false
  }

  override def hashCode: Int = Hash.hash(underlying.elements)
}

object ArraySeqCollection {

  implicit def cbf[T, U: Eq:Hash:ClassTag]: CanBuildFrom[ArraySeqCollection[T], U, ArraySeqCollection[U]] = new CanBuildFrom[ArraySeqCollection[T], U, ArraySeqCollection[U]] {
    def apply(from: ArraySeqCollection[T]) = apply()

    def apply() = new ArrayBuffer[U].mapResult(x ⇒ new ArraySeqCollection(new ArraySeq[U](x.toArray)))
  }
}

final class ArraySetCollection[T](val underlying: ArraySet[T])(implicit tOrder: Order[T], tClassTag: ClassTag[T], tHash: Hash[T]) extends SortedSet[T] with SortedSetLike[T, ArraySetCollection[T]] {
  import ArraySetCollection.wrap
  implicit def ordering = Order.ordering(tOrder)

  def +(elem: T) = wrap(underlying + elem)

  def -(elem: T) = wrap(underlying - elem)

  def contains(elem: T) = underlying contains elem

  def iterator = underlying.iterator

  def rangeImpl(from: Option[T], until: Option[T]) = ???

  def keysIteratorFrom(start: T) = ???

  override def union(that: GenSet[T]) = that match {
    case that: ArraySetCollection[T] ⇒ wrap(underlying union that.underlying)
    case _ ⇒ super.union(that)
  }

  override def diff(that: GenSet[T]) = that match {
    case that: ArraySetCollection[T] ⇒ wrap(underlying diff that.underlying)
    case _ ⇒ super.diff(that)
  }

  override def intersect(that: GenSet[T]) = that match {
    case that: ArraySetCollection[T] ⇒ wrap(underlying intersect that.underlying)
    case _ ⇒ super.intersect(that)
  }

  override def subsetOf(that: GenSet[T]) = that match {
    case that: ArraySetCollection[T] ⇒ underlying subsetOf that.underlying
    case _ ⇒ super.subsetOf(that)
  }

  override def filter(p: T => Boolean) = new ArraySetCollection(underlying.filter(p))

  override def isEmpty: Boolean = underlying.isEmpty

  override def equals(that: Any) = that match {
    case that: ArraySetCollection[T] => Eq.eqv(underlying.elements, that.underlying.elements)
    case _ => false
  }

  override def toString = underlying.toString

  override def hashCode: Int = Hash.hash(underlying.elements)

  override def apply(e: T): Boolean = underlying.apply(e)

  override def empty = new ArraySetCollection(ArraySet.empty[T])
}

object ArraySetCollection {

  private[abc] def wrap[U: Order: ClassTag: Hash](underlying: ArraySet[U]) = new ArraySetCollection[U](underlying)

  implicit def cbf[CC, U: Order: ClassTag: Hash]: CanBuildFrom[CC, U, ArraySetCollection[U]] = new CanBuildFrom[CC, U, ArraySetCollection[U]] {
    def apply(from: CC) = apply()

    def apply(): mutable.Builder[U, ArraySetCollection[U]] = new ArraySetBuilder[U].mapResult(x ⇒ wrap(x))
  }

  private[this] class ArraySetBuilder[T](implicit order: Order[T], classTag: ClassTag[T]) extends scala.collection.mutable.Builder[T, ArraySet[T]] {

    private[this] def union(a: Array[T], b: Array[T]) = {
      SetUtils.union(a, b)
    }

    private[this] var reducer = Reducer[Array[T]](union)

    def +=(elem: T) = {
      reducer.apply(Array.singleton(elem))
      this
    }

    def clear() = {
      reducer = Reducer[Array[T]](union)
    }

    def result() = {
      reducer.result().map(x ⇒ new ArraySet(x)).getOrElse(ArraySet.empty)
    }
  }
}

class ArrayMapCollection[K: Order: ClassTag, V: ClassTag](underlying: ArrayMap[K, V]) extends SortedMap[K, V] with SortedMapLike[K, V, ArrayMapCollection[K, V]] {
  import ArrayMapCollection._

  implicit def ordering = Order.ordering[K]

  override def newBuilder : mutable.Builder[(K, V), ArrayMapCollection[K, V]] =
    new ArrayMapBuilder[K, V].mapResult(x ⇒ wrap(x))

  override def empty = wrap(ArrayMap.empty[K, V])

  def valuesIteratorFrom(start: K) = ???

  def rangeImpl(from: Option[K], until: Option[K]) = ???

  def iteratorFrom(start: K) = ???

  def get(key: K) = underlying.get(key)

  def iterator = underlying.iterator

  override def +[V1 >: V](kv: (K, V1)) = {
    try {
      val k = kv._1
      val v = kv._2.asInstanceOf[V]
      wrap(underlying.merge(ArrayMap.singleton(k, v)))
    } catch {
      case _: ClassCastException ⇒
        val k = kv._1
        val v = kv._2.asInstanceOf[AnyRef]
        wrap(underlying.mapValues(_.asInstanceOf[AnyRef]).merge(ArrayMap.singleton(k, v)))
          .asInstanceOf[SortedMap[K, V1]]
    }
  }

  def -(key: K) = wrap(underlying.exceptKeys(ArraySet.singleton(key)))

  def keysIteratorFrom(start: K) = ???
}

object ArrayMapCollection {

  implicit def cbf[CC, K: Order: ClassTag, V: ClassTag]: CanBuildFrom[CC, (K, V), ArrayMapCollection[K, V]] = new CanBuildFrom[CC, (K, V), ArrayMapCollection[K, V]] {
    def apply(from: CC) = apply()

    def apply() = new ArrayBuffer[(K, V)].mapResult(x ⇒ ArrayMapCollection.wrap(ArrayMap(x: _*)))
  }

  private[abc] def wrap[K: Order : ClassTag, V: ClassTag](underlying: ArrayMap[K, V]): ArrayMapCollection[K, V] =
    new ArrayMapCollection[K, V](underlying)

  private class ArrayMapBuilder[@specialized(Int, Long, Double) K: Order: ClassTag, @specialized(Int, Long, Double) V: ClassTag] extends scala.collection.mutable.Builder[(K, V), ArrayMap[K, V]] {

    private[this] var reducer = Reducer[ArrayMap[K, V]](_ merge _)

    def +=(elem: (K, V)) = {
      reducer.apply(ArrayMap.singleton(elem._1, elem._2))
      this
    }

    def clear() =
      reducer = Reducer[ArrayMap[K, V]](_ merge _)

    def result() =
      reducer.result.getOrElse(ArrayMap.empty)
  }
}
// $COVERAGE-ON$
