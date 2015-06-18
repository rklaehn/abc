package com.rklaehn.abc

import ichi.bench.Thyme
import ichi.bench.Thyme.HowWarm
import spire.algebra.Eq
import spire.implicits._

import scala.collection.immutable.{HashMap, SortedMap}
import scala.util.hashing.Hashing

object Memo {
  def simple[@specialized A](implicit e:Eq[A], h:Hashing[A]): A => A = new (A => A) {
    
    case class Element(@specialized value: A) {
      override def equals(that: Any): Boolean = that match {
        case that: Element => e.eqv(this.value, that.value)
        case _ => false
      }
      
      override val hashCode: Int = h.hash(value)
    }
    
    val memo = new scala.collection.mutable.AnyRefMap[Element, Element]
    
    def apply(a: A): A = {
      val k = Element(a)
      memo.getOrElseUpdate(k, k).value
    }
  }
}

object RadixTreeBench extends App {

  implicit object UnitEq extends Eq[Unit] {

    override def eqv(x: Unit, y: Unit): Boolean = true
  }

  implicit object EqHashing extends Hashing[Unit] {
    override def hash(x: Unit): Int = 0
  }

  lazy val th = Thyme.warmed(verbose = println, warmth = HowWarm.BenchOff)
  // val th = new Thyme()

  val kvs = (0 until 10000).map(i => NumberToWord(i) -> (())).toArray

  val kvs2 = (10000 until 20000).map(i => NumberToWord(i) -> (())).toArray

  val radixTree = RadixTree(kvs: _*).packed

  val sortedMap = SortedMap(kvs: _*)

  val hashMap = HashMap(kvs: _*)

  val radixTree2 = RadixTree(kvs2: _*)

  val sortedMap2 = SortedMap(kvs2: _*)

  val hashMap2 = HashMap(kvs2: _*)

  def create0[K: Ordering, V](kvs: Array[(K, V)]): Int = {
    SortedMap(kvs: _*).size
  }

  def create1[K, V](kvs: Array[(K, V)])(implicit f:RadixTree.Family[K, V]): Int = {
    RadixTree[K,V](kvs: _*).count
  }

  def lookup0(): Boolean = {
    kvs.forall {
      case (k,v) => radixTree.contains(k)
    }
  }

  def lookup1(): Boolean = {
    kvs.forall {
      case (k,v) => hashMap.contains(k)
    }
  }

  def lookup2(): Boolean = {
    kvs.forall {
      case (k,v) => sortedMap.contains(k)
    }
  }

  def mergeS(): AnyRef = {
    sortedMap ++ sortedMap2
  }

  def mergeH(): AnyRef = {
    hashMap ++ hashMap2
  }

  def mergeR(): AnyRef = {
    radixTree merge radixTree2
  }


  def filterPrefixS(): AnyRef = {
    sortedMap.filter { case (k,v) => k.startsWith("one") }
  }

  def filterPrefixH(): AnyRef = {
    hashMap.filter { case (k,v) => k.startsWith("one") }
  }

  def filterPrefixR(): AnyRef = {
    radixTree.filterPrefix("one")
  }

  def filterContainsS(): AnyRef = {
    sortedMap.filter { case (k,v) => k.contains("one") }
  }

  def filterContainsH(): AnyRef = {
    hashMap.filter { case (k,v) => k.contains("one") }
  }

  def filterContainsR(): AnyRef = {
    radixTree.filterKeysContaining("one")
  }

  th.pbenchOffWarm("Create 1000 SortedMap vs. RadixTree")(th.Warm(create0(kvs)))(th.Warm(create1(kvs)))
  th.pbenchOffWarm("Lookup 1000 SortedMap vs. RadixTree")(th.Warm(lookup0()))(th.Warm(lookup1()))

  th.pbenchOffWarm("Merge 2000 HashMap vs. RadixTree")(th.Warm(mergeH()))(th.Warm(mergeR()))
  th.pbenchOffWarm("Merge 2000 SortedMap vs. RadixTree")(th.Warm(mergeS()))(th.Warm(mergeR()))

  th.pbenchOffWarm("FilterPrefix HashMap vs. RadixTree")(th.Warm(filterPrefixH()))(th.Warm(filterPrefixR()))
  th.pbenchOffWarm("FilterPrefix SortedMap vs. RadixTree")(th.Warm(filterPrefixS()))(th.Warm(filterPrefixR()))

  th.pbenchOffWarm("FilterContains HashMap vs. RadixTree")(th.Warm(filterContainsH()))(th.Warm(filterContainsR()))
  th.pbenchOffWarm("FilterContains SortedMap vs. RadixTree")(th.Warm(filterContainsS()))(th.Warm(filterContainsR()))
}
