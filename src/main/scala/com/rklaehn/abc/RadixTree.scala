package com.rklaehn.abc

import spire.algebra.Eq
import spire.util.Opt

import scala.annotation.tailrec
import scala.collection.AbstractTraversable

final class RadixTree[K,V](val prefix:K, val children:Array[RadixTree[K,V]], val valueOpt:Opt[V])(implicit e: RadixTree.Element[K]) {

  private def childrenAsAnyRefArray = children.asInstanceOf[Array[AnyRef]]
  
  def count: Int = {
    var n = if (valueOpt.isDefined) 1 else 0
    var i = 0
    while (i < children.length) {
      n += children(i).count
      i += 1
    }
    n
  }

  override def equals(obj: Any) = obj match {
    case that: RadixTree[K, V] =>
      this.hashCode == that.hashCode &&
        e.eqv(this.prefix, that.prefix) &&
        this.valueOpt == that.valueOpt &&
        java.util.Arrays.equals(this.childrenAsAnyRefArray, that.childrenAsAnyRefArray)
    case _ => false
  }
  
  override lazy val hashCode = {
    import scala.util.hashing.MurmurHash3._

    mixLast(
      mix(
        e.hash(prefix),
        java.util.Arrays.hashCode(this.childrenAsAnyRefArray)),
      valueOpt.##)
  }

  override def toString: String = pairs
    .toArray
    .map { case (k,v) => s"$k -> $v"}
    .mkString("RadixTree(", ",", ")")

  def printStructure: String = children
    .mkString(s"RadixTree($prefix, $valueOpt, [",",","])")

  def isEmpty = e.size(prefix) == 0

  def prepend(prefix: K)(implicit e:RadixTree.Element[K]): RadixTree[K, V] =
    new RadixTree[K, V](e.concat(prefix, this.prefix), children, valueOpt)

  def startsWith(prefix:K) =
    filterPrefix(prefix) eq this

  def filterPrefix(prefix:K) : RadixTree[K, V] =
    filterPrefix0(prefix,0)

  def subtreeWithPrefix(prefix:K) = {
    val tree1 = filterPrefix(prefix)
    if (e.startsWith(tree1.prefix, prefix, 0))
      tree1.copy(prefix = e.substring(tree1.prefix, e.size(prefix), e.size(tree1.prefix)))
    else
      e.emptyTree[V]
  }

  def pairs: Traversable[(K, V)] = new AbstractTraversable[(K, V)] {
    def foreach[U](f: ((K, V)) => U) = foreachPair(e.emptyTree[V].prefix, f)
  }

  def values: Traversable[V] = new AbstractTraversable[V] {
    def foreach[U](f: V => U) = foreachValue(f)
  }

  def keys: Traversable[K] = new AbstractTraversable[K] {
    def foreach[U](f: K => U) = foreachKey(e.emptyTree[V].prefix, f)
  }

  private def foreachChild[U](f:RadixTree[K, V]=>U) {
    var i=0
    while(i < children.length) {
      f(children(i))
      i+=1
    }
  }

  private def foreachPair[U](prefix:K, f:((K,V))=>U) {
    val newPrefix=e.concat(prefix, this.prefix)
    if(valueOpt.isDefined)
      f((newPrefix,valueOpt.get))
    foreachChild(_.foreachPair(newPrefix,f))
  }

  private def foreachValue[U](f:V=>U) {
    if(valueOpt.isDefined)
      f(valueOpt.get)
    foreachChild(_.foreachValue(f))
  }

  private def foreachKey[U](prefix:K, f:K=>U) {
    val newPrefix=e.concat(prefix, this.prefix)
    if(valueOpt.isDefined)
      f(newPrefix)
    foreachChild(_.foreachKey(newPrefix, f))
  }

  private def filterPrefix0(pre: K, offset: Int): RadixTree[K, V] = {
    val ps = e.size(prefix)
    val pres = e.size(pre)
    val maxFd = ps min (pres - offset)
    val fd = e.indexOfFirstDifference(prefix, 0, pre, offset, maxFd)
    if (fd == maxFd) {
      if (maxFd < ps || pres - offset == ps)
        this
      else {
        val index = e.binarySearch(children, pre, offset + ps)
        if (index >= 0) {
          val child1 = children(index).filterPrefix0(pre, offset + ps)
          val children1 =
            if (child1.isEmpty) e.emptyTree[V].children
            else Array(child1)
          copy(valueOpt = Opt.empty[V], children = children1)
        } else
          e.emptyTree[V]
      }
    } else
      e.emptyTree[V]
  }

  def modifyOrRemove(f: (K, V, Int) => Opt[V]): RadixTree[K, V] =
    modifyOrRemove0(f, e.emptyTree[V].prefix)

  private def modifyOrRemove0(f: (K, V, Int) => Opt[V], prefix: K): RadixTree[K, V] = {
    val newPrefix = e.concat(prefix, this.prefix)
    val builder = Array.newBuilder[RadixTree[K, V]]
    builder.sizeHint(children.size)
    for (child <- children) {
      val newChild = child.modifyOrRemove0(f, newPrefix)
      if (!newChild.isEmpty)
        builder += newChild
    }
    val temp = builder.result()
    val newChildArray =
      if (children.length == temp.length && children.corresponds(temp)(_ eq _)) children
      else temp
    val newValueOpt = if (valueOpt.isDefined) f(newPrefix, valueOpt.get, newChildArray.length) else Opt.empty
    copy(children = newChildArray, valueOpt = newValueOpt)
  }

  def filter(f: (K, V) => Boolean): RadixTree[K, V] =
    filter0(f, e.emptyTree[V].prefix)

  private def filter0(f: (K, V) => Boolean, prefix: K): RadixTree[K, V] = {
    val newPrefix = e.concat(prefix, this.prefix)
    val builder = Array.newBuilder[RadixTree[K, V]]
    builder.sizeHint(children.size)
    for (child <- children) {
      val newChild = child.filter0(f, newPrefix)
      if (!newChild.isEmpty)
        builder += newChild
    }
    val temp = builder.result()
    val newChildArray =
      if (children.length == temp.length && children.corresponds(temp)(_ eq _)) children
      else temp
    val newValueOpt = if (valueOpt.isDefined && f(newPrefix, valueOpt.get)) valueOpt else Opt.empty
    copy(children = newChildArray, valueOpt = newValueOpt)
  }

  private def copy(prefix: K = this.prefix, valueOpt: Opt[V] = this.valueOpt, children: Array[RadixTree[K, V]] = this.children): RadixTree[K, V] = {
    def same(a: Opt[V], b:Opt[V]): Boolean =
      if(a.isDefined && b.isDefined)
        a.get.asInstanceOf[AnyRef] eq b.get.asInstanceOf[AnyRef]
      else a.isDefined == b.isDefined
    if (e.eqv(prefix, this.prefix) && same(valueOpt, this.valueOpt) && ((children eq this.children) || (children.length == 0 && this.children.length == 0)))
      this
    else if (valueOpt.isEmpty)
      children.length match {
        case 0 => e.emptyTree[V]
        case 1 => children(0).prepend(prefix)
        case _ => e.mkNode(e.intern(prefix), valueOpt, children)
      }
    else
      e.mkNode(e.intern(prefix), valueOpt, children)
  }

  def merge(other:RadixTree[K, V]) : RadixTree[K, V] =
    merge0(other, 0, null)

  def merge(other:RadixTree[K, V], collision:(V,V)=>V) : RadixTree[K, V] =
    merge0(other, 0, collision)

  def apply(key:K) = get(key).get

  def contains(key: K) = get(key).isDefined

  def get(key: K): Opt[V] = get0(key, 0)

  @tailrec
  private def get0(key: K, offset: Int): Opt[V] =
    if (e.startsWith(key, prefix, offset)) {
      val newOffset = offset + e.size(prefix)
      if (e.size(key) == newOffset) valueOpt
      else {
        val index = e.binarySearch(children, key, newOffset)
        if (index >= 0) children(index).get0(key, newOffset)
        else Opt.empty
      }
    } else
      Opt.empty

  private def merge0(that: RadixTree[K, V], offset: Int, collision: (V, V) => V): RadixTree[K, V] = {
    val ps = e.size(prefix)
    val tps = e.size(that.prefix)
    val maxFd = ps min (tps - offset)
    val fd = e.indexOfFirstDifference(prefix, 0, that.prefix, offset, maxFd)
    if (fd == maxFd) {
      // prefixes match
      if (maxFd < ps) {
        // this.prefix is longer than (that.prefix - offset)
        val prefix0 = e.substring(prefix, 0, fd)
        val prefix1 = e.substring(prefix, fd, ps)
        val this1 = copy(prefix = prefix1)
        val childArray1 = e.mergeChildren(Array(this1), that.children, collision)
        copy(prefix = prefix0, valueOpt = that.valueOpt, children = childArray1)
      }
      else if (tps - offset == ps) {
        // this.prefix is the same as other.prefix when adjusted by offset
        // merge the values using the collision function if necessary
        val mergedValueOpt =
          if(this.valueOpt.isDefined) {
            if((collision ne null) && that.valueOpt.isDefined)
              Opt(collision(this.valueOpt.get,that.valueOpt.get))
            else
              this.valueOpt
          }
          else
            that.valueOpt
        e.mkNode(this.prefix, mergedValueOpt, e.mergeChildren(this.children, that.children, collision))
      }
      else {
        val childOffset = offset + e.size(prefix)
        val index = e.binarySearch(children, that.prefix, childOffset)
        val childArray1 = if (index >= 0) {
          val child1 = children(index).merge0(that, childOffset, collision)
          ArrayOps(children).updated(index, child1)
        }
        else {
          val tp1 = e.substring(that.prefix, childOffset, tps)
          val child1 = that.copy(prefix = tp1)
          ArrayOps(children).patched(-index - 1, child1)
        }
        copy(children = childArray1)
      }
    } else {
      // both trees have a common prefix (might be "")
      val commonPrefix = e.substring(prefix, 0, fd)
      val p1 = e.substring(this.prefix, fd, ps)
      val tp1 = e.substring(that.prefix, offset + fd, tps)
      val childA = this.copy(prefix = p1)
      val childB = that.copy(prefix = tp1)
      val children1 =
        if (e.compareAt(childA.prefix, 0, childB.prefix, 0) < 0)
          Array(childA, childB)
        else
          Array(childB, childA)
      e.mkNode(commonPrefix, Opt.empty, children1)
    }
  }

}

object RadixTree {

  def empty[K: Element, V]: RadixTree[K, V] =
    implicitly[Element[K]].emptyTree.asInstanceOf[RadixTree[K, V]]

  def singleton[K: Element, V](key: K, value: V): RadixTree[K, V] =
    new RadixTree[K, V](key, empty[K, V].children, Opt(value))

  def apply[K: Element, V](kvs: (K,V)*): RadixTree[K, V] = {
    val reducer = Reducer.create[RadixTree[K,V]](_ merge _)
    for((k, v) <- kvs)
      reducer.apply(singleton(k, v))
    reducer.result().getOrElse(empty[K,V])
  }

  trait Element[K] extends Any with Eq[K] {

    def emptyTree[V]: RadixTree[K, V]

    def size(c: K): Int

    def intern(e: K): K

    def concat(a: K, b: K): K

    def substring(a: K, from: Int, until: Int): K

    def compareAt(a: K, ai: Int, b: K, bi: Int): Int

    def indexOfFirstDifference(a: K, ai: Int, b: K, bi: Int, count: Int): Int

    def hash(e: K): Int

    def startsWith(a: K, b: K, bi:Int): Boolean = {
      val bs = size(b)
      indexOfFirstDifference(a, 0, b, bi, bs) == bs
    }

    final def mkNode[V](prefix: K, valueOpt: Opt[V], children: Array[RadixTree[K,V]]): RadixTree[K,V] =
      new RadixTree[K, V](prefix, children, valueOpt)(this)

    final def binarySearch[V](elems: Array[RadixTree[K, V]], elem: K, offset: Int): Int = {

      @tailrec
      def binarySearch0(low: Int, high: Int): Int =
        if (low <= high) {
          val mid = (low + high) >>> 1
          val c = compareAt(elem, offset, elems(mid).prefix, 0)
          if (c > 0)
            binarySearch0(mid + 1, high)
          else if (c < 0)
            binarySearch0(low, mid - 1)
          else
            mid
        } else -(low + 1)
      binarySearch0(0, elems.length - 1)
    }

    final def mergeChildren[V](a: Array[RadixTree[K, V]], b: Array[RadixTree[K, V]], f: (V, V) => V): Array[RadixTree[K, V]] = {
      val r = a.newArray(a.length + b.length)
      var ri: Int = 0
      new spire.math.BinaryMerge {

        def compare(ai: Int, bi: Int) = compareAt(a(ai).prefix, 0, b(bi).prefix, 0)

        def collision(ai: Int, bi: Int): Unit = {
          r(ri) = a(ai).merge(b(bi), f)
          ri += 1
        }

        def fromA(a0: Int, a1: Int, bi: Int): Unit = {
          System.arraycopy(a, a0, r, ri, a1 - a0)
          ri += a1 - a0
        }

        def fromB(ai: Int, b0: Int, b1: Int): Unit = {
          System.arraycopy(b, b0, r, ri, b1 - b0)
          ri += b1 - b0
        }

        merge0(0, a.length, 0, b.length)
      }
      r.resizeInPlace(ri)
    }
  }

  implicit object StringIsRadixTreeElement extends Element[String] {

    private val _emptyTree = new RadixTree[String, Nothing]("", Array.empty, Opt.empty)

    override def emptyTree[V]: RadixTree[String, V] = _emptyTree.asInstanceOf[RadixTree[String, V]]

    override def size(c: String): Int =
      c.length

    override def substring(a: String, from: Int, until: Int): String =
      a.substring(from, until)

    @tailrec
    override def indexOfFirstDifference(a: String, ai: Int, b: String, bi: Int, count:Int): Int =
      if (count == 0 || a(ai) != b(bi)) ai
      else indexOfFirstDifference(a, ai + 1, b, bi + 1, count - 1)

    override def concat(a: String, b: String): String =
      a + b

    override def eqv(a: String, b: String): Boolean =
      a == b

    override def intern(s: String): String = s

    override def compareAt(a: String, ai: Int, b: String, bi: Int): Int =
      a(ai) compare b(bi)

    override def hash(e: String): Int =
      scala.util.hashing.MurmurHash3.stringHash(e)
  }
}