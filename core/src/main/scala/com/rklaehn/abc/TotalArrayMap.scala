package com.rklaehn.abc

import algebra._
import algebra.ring._
import cats.Show
import cats.syntax.show._

final class TotalArrayMap[K, V](
    private[abc] val keys0: Array[K],
    private[abc] val values0: Array[V],
    val default: V
  ) { lhs ⇒

  import TotalArrayMap._

  def withoutDefault: ArrayMap[K, V] = new ArrayMap[K, V](keys0, values0)

  def iterator = keys0.iterator zip values0.iterator

  def size: Int = keys0.length

  def keys: ArraySet[K] = new ArraySet[K](keys0)

  def values: ArraySeq[V] = new ArraySeq[V](values0)

  def apply(k: K)(implicit K: Order[K]): V = {
    val i = Searching.search(keys0, 0, keys0.length, k)
    if (i >= 0) values0(i)
    else default
  }

  def combine(rhs: TotalArrayMap[K, V], f: (V, V) ⇒ V)(implicit kOrder: Order[K], vEq: Eq[V]): TotalArrayMap[K, V] =
    new Combine[K, V](lhs, rhs, f(lhs.default, rhs.default), f).result

  def mapValues[@sp(ILD) W](f: V ⇒ W)(implicit wEq: Eq[W]): TotalArrayMap[K, W] = {
    val rk = newArray[K](size, keys0)
    val rv = UnboxedArrayBuilder[W](size)
    val rd = f(default)
    var i = 0
    var ri = 0
    while(i < keys0.length) {
      val r = f(values0(i))
      if(Eq.neqv(r, rd)) {
        rk(ri) = keys0(i)
        rv.add(r)
        ri += 1
      }
      i += 1
    }
    val keys1 = if(ri == keys0.length) keys0 else rk.resizeInPlace(ri)
    val values1 = rv.result
    new TotalArrayMap[K, W](keys1, values1, rd)
  }

  private[abc] def mapValues0(f: V ⇒ V)(implicit V: Eq[V]): TotalArrayMap[K, V] = {
    val rk = newArray[K](size, keys0)
    val rv = newArray[V](size, values0)
    val rd = f(default)
    var i = 0
    var ri = 0
    while(i < keys0.length) {
      val r = f(values0(i))
      if(Eq.neqv(r, rd)) {
        rk(ri) = keys0(i)
        rv(ri) = r
        ri += 1
      }
      i += 1
    }
    val keys1 = if(ri == keys0.length) keys0 else rk.resizeInPlace(ri)
    val values1 = rv.resizeInPlace(ri)
    new TotalArrayMap[K, V](keys1, values1, rd)
  }

  override def equals(that: Any): Boolean = that match {
    case that: TotalArrayMap[K, V] => TotalArrayMap.eqv(Universal[K], Universal[V]).eqv(this, that)
    case _ => false
  }

  override def hashCode(): Int = TotalArrayMap.hash(Universal[K], Universal[V]).hash(this)

  override def toString: String = TotalArrayMap.show(Universal[K], Universal[V]).show(this)
}

private[abc] trait TotalArrayMap1 {

  implicit def eqv[K: Eq, V: Eq]: Eq[TotalArrayMap[K, V]] = new Eq[TotalArrayMap[K, V]] {
    def eqv(x: TotalArrayMap[K, V], y: TotalArrayMap[K, V]) =
      Eq.eqv(x.default, y.default) && ArrayUtil.eqv(x.keys0, y.keys0) && ArrayUtil.eqv(x.values0, y.values0)
  }
}

private[abc] trait TotalArrayMap2 extends TotalArrayMap1 {
  import TotalArrayMap._

  implicit def order[K: Order, V: Order]: Order[TotalArrayMap[K, V]] = new Order[TotalArrayMap[K, V]] {
    override def eqv(x: TotalArrayMap[K, V], y: TotalArrayMap[K, V]) =
      Eq.eqv(x.default, y.default) && ArrayUtil.eqv(x.keys0, y.keys0) && ArrayUtil.eqv(x.values0, y.values0)

    def compare(x: TotalArrayMap[K, V], y: TotalArrayMap[K, V]) =
      TotalArrayMap.compare(x, y)
  }

  implicit def monoid[K : Order, V: Monoid: Eq]: Monoid[TotalArrayMap[K, V]] =
    new ArrayTotalMapMonoid[K, V]

  implicit def additiveMonoid[K: Order, V: AdditiveMonoid: Eq]: AdditiveMonoid[TotalArrayMap[K, V]] =
    new ArrayTotalMapAdditiveMonoid[K, V]

  implicit def multiplicativeMonoid[K: Order, V: MultiplicativeMonoid: Eq]: MultiplicativeMonoid[TotalArrayMap[K, V]] =
    new ArrayTotalMapMultiplicativeMonoid[K, V]
}

private[abc] trait TotalArrayMap3 extends TotalArrayMap2 {
  import TotalArrayMap._

  implicit def group[K: Order, V: Group: Eq]: Group[TotalArrayMap[K, V]] =
    new ArrayTotalMapGroup[K, V]

  implicit def additiveGroup[K: Order, V: AdditiveGroup: Eq]: AdditiveGroup[TotalArrayMap[K, V]] =
    new ArrayTotalMapAdditiveGroup[K, V]

  implicit def multiplicativeGroup[K: Order, V: MultiplicativeGroup: Eq]: MultiplicativeGroup[TotalArrayMap[K, V]] =
    new ArrayTotalMapMultiplicativeGroup[K, V]

  implicit def semiring[K: Order, V: Semiring: Eq]: Semiring[TotalArrayMap[K, V]] =
    new ArrayTotalMapSemiring[K, V]
}

object TotalArrayMap extends TotalArrayMap3 {

  private def fastCombine[@sp(ILD) K: Order, @sp(ILD) V: Eq](lhs: TotalArrayMap[K,V], rhs: TotalArrayMap[K, V], f: (V, V) ⇒ V): TotalArrayMap[K, V] =
    new FastCombine[K, V](lhs, rhs, lhs.default, f).result

  implicit def rng[K: Order, V: Rng: Eq]: Rng[TotalArrayMap[K, V]] =
    new TotalArrayMapRng[K, V]

  private[abc] class ArrayTotalMapMonoid[K : Order, V: Monoid: Eq]
    extends Monoid[TotalArrayMap[K, V]] {
    override def combine(x: TotalArrayMap[K, V], y: TotalArrayMap[K, V]): TotalArrayMap[K, V] = {
      val m = Monoid[V]
      if(m.isEmpty(x.default) && m.isEmpty(y.default))
        fastCombine(x, y, m.combine)
      else
        x.combine(y, m.combine)
    }
    override def empty: TotalArrayMap[K, V] = TotalArrayMap.fromDefault[K, V](Monoid[V].empty)
  }

  private[abc] final class ArrayTotalMapGroup[K: Order, V: Group: Eq]
    extends ArrayTotalMapMonoid[K, V] with Group[TotalArrayMap[K, V]] {
    override def inverse(x: TotalArrayMap[K, V]): TotalArrayMap[K, V] = x.mapValues0(x ⇒ Group.inverse(x))
    override def remove(x: TotalArrayMap[K, V], y: TotalArrayMap[K, V]): TotalArrayMap[K, V] =
      x.combine(y, (x,y) ⇒ Group.remove(x, y))
  }

  private[abc] class ArrayTotalMapAdditiveMonoid[K: Order, V: AdditiveMonoid: Eq]
    extends AdditiveMonoid[TotalArrayMap[K, V]] {
    override def zero: TotalArrayMap[K, V] = TotalArrayMap.fromDefault[K, V](AdditiveMonoid[V].zero)
    override def plus(x: TotalArrayMap[K, V], y: TotalArrayMap[K, V]): TotalArrayMap[K, V] = {
      val m = AdditiveMonoid[V]
      if(m.isZero(x.default) && m.isZero(y.default))
        fastCombine(x, y, m.plus)
      else
        x.combine(y, m.plus)
    }
  }

  private[abc] final class ArrayTotalMapAdditiveGroup[K: Order, V: AdditiveGroup: Eq]
    extends ArrayTotalMapAdditiveMonoid[K, V] with AdditiveGroup[TotalArrayMap[K, V]] {
    override def negate(x: TotalArrayMap[K, V]): TotalArrayMap[K, V] = x.mapValues0(x ⇒ AdditiveGroup.negate(x))
    override def minus(x: TotalArrayMap[K, V], y: TotalArrayMap[K, V]): TotalArrayMap[K, V] =
      x.combine(y, (x,y) ⇒ AdditiveGroup.minus(x, y))
  }

  private[abc] class ArrayTotalMapMultiplicativeMonoid[K: Order, V: MultiplicativeMonoid: Eq]
    extends MultiplicativeMonoid[TotalArrayMap[K, V]] {
    override def one: TotalArrayMap[K, V] = TotalArrayMap.fromDefault[K, V](MultiplicativeMonoid[V].one)
    override def times(x: TotalArrayMap[K, V], y: TotalArrayMap[K, V]): TotalArrayMap[K, V] = {
      val m = MultiplicativeMonoid[V]
      if(m.isOne(x.default) && m.isOne(y.default))
        fastCombine(x, y, m.times)
      else
        x.combine(y, m.times)
    }
  }

  private[abc] final class ArrayTotalMapMultiplicativeGroup[K: Order, V: MultiplicativeGroup: Eq]
    extends ArrayTotalMapMultiplicativeMonoid[K, V] with MultiplicativeGroup[TotalArrayMap[K, V]] {
    override def reciprocal(x: TotalArrayMap[K, V]): TotalArrayMap[K, V] = x.mapValues0(x ⇒ MultiplicativeGroup.reciprocal(x))
    override def div(x: TotalArrayMap[K, V], y: TotalArrayMap[K, V]): TotalArrayMap[K, V] =
      x.combine(y, (x,y) ⇒ MultiplicativeGroup.div(x, y))
  }

  private[abc] class ArrayTotalMapSemiring[K: Order, V: Semiring: Eq]
    extends Semiring[TotalArrayMap[K, V]] {
    def plus(x: TotalArrayMap[K, V], y: TotalArrayMap[K, V]): TotalArrayMap[K, V] = {
      val m = AdditiveCommutativeMonoid[V]
      if(m.isZero(x.default) && m.isZero(y.default))
        fastCombine(x, y, m.plus)
      else
        x.combine(y, m.plus)
    }
    def times(x: TotalArrayMap[K, V], y: TotalArrayMap[K, V]): TotalArrayMap[K, V] =  {
      val m = MultiplicativeSemigroup[V]
      x.combine(y, m.times)
    }
    def zero = TotalArrayMap.fromDefault[K, V](AdditiveCommutativeMonoid[V].zero)
  }

  private[abc] class TotalArrayMapRng[K: Order, V: Rng: Eq]
    extends ArrayTotalMapSemiring[K, V] with Rng[TotalArrayMap[K, V]] {
    def negate(x: TotalArrayMap[K, V]) = x.mapValues0(x ⇒ Rng.negate(x))
    override def minus(x: TotalArrayMap[K, V], y: TotalArrayMap[K, V]) =
      x.combine(y, (x, y) ⇒ Rng.minus(x, y))
  }

  implicit def show[K: Show, V: Show]: Show[TotalArrayMap[K, V]] = Show.show(m ⇒
    m.iterator
      .map { case (k,v) ⇒ s"${k.show}->${v.show}" }
      .mkString(s"TotalArrayMap((", ",", s"), ${m.default.show})")
  )

  implicit def hash[K: Hash, V: Hash]: Hash[TotalArrayMap[K, V]] = new Hash[TotalArrayMap[K, V]] {
    def hash(x: TotalArrayMap[K, V]) =
      (Hash.hash(x.default), Hash.hash(x.keys0), Hash.hash(x.values0)).##

    def eqv(x: TotalArrayMap[K, V], y: TotalArrayMap[K, V]) =
      Eq.eqv(x.default, y.default) && ArrayUtil.eqv(x.keys0, y.keys0) && ArrayUtil.eqv(x.values0, y.values0)
  }

  private final class Combine[@sp(ILD) K: Order, @sp(ILD) V: Eq](
    a: TotalArrayMap[K, V], b: TotalArrayMap[K, V], rd: V, f: (V, V) => V)
    extends BinaryMerge {

    @inline def ak = a.keys0
    @inline def av = a.values0
    @inline def bk = b.keys0
    @inline def bv = b.values0

    val rk = newArray[K](a.size + b.size, ak)
    val rv = newArray[V](a.size + b.size, av)
    var ri = 0

    def compare(ai: Int, bi: Int) = Order.compare(ak(ai), bk(bi))

    def fromA(a0: Int, a1: Int, bi: Int) = {
      var ai = a0
      while(ai < a1) {
        val r = f(av(ai), b.default)
        if(Eq.neqv(r, rd)) {
          rk(ri) = ak(ai)
          rv(ri) = r
          ri += 1
        }
        ai += 1
      }
    }

    def fromB(ai: Int, b0: Int, b1: Int) = {
      var bi = b0
      while(bi < b1) {
        val r = f(a.default, bv(bi))
        if(Eq.neqv(r, rd)) {
          rk(ri) = bk(bi)
          rv(ri) = r
          ri += 1
        }
        bi += 1
      }
    }

    def collision(ai: Int, bi: Int) = {
      val r = f(av(ai), bv(bi))
      if(Eq.neqv(r, rd)) {
        rk(ri) = bk(bi)
        rv(ri) = r
        ri += 1
      }
    }

    merge0(0, ak.length, 0, bk.length)

    def result: TotalArrayMap[K, V] = new TotalArrayMap[K, V](rk.resizeInPlace(ri), rv.resizeInPlace(ri), rd)
  }

  // fast path for combine in case a.default, b.default and r.default is the neutral element
  // in this case we know that we can just copy over elements in a but not in b, or in b but not in a
  // this is especially important when combining a small and a large map
  //
  // for collisions we still have to check if the result is the default. E.g. when adding Int.MinValue + Int.MinValue,
  // you get back 0, the neutral element of addition
  private final class FastCombine[@sp(ILD) K: Order, @sp(ILD) V: Eq](
    a: TotalArrayMap[K, V], b: TotalArrayMap[K, V], rd: V, f: (V, V) => V)
    extends BinaryMerge {

    @inline def ak = a.keys0
    @inline def av = a.values0
    @inline def bk = b.keys0
    @inline def bv = b.values0

    val rk = newArray[K](a.size + b.size, ak)
    val rv = newArray[V](a.size + b.size, av)
    var ri = 0

    def compare(ai: Int, bi: Int) = Order.compare(ak(ai), bk(bi))

    def fromA(a0: Int, a1: Int, bi: Int) = {
      System.arraycopy(ak, a0, rk, ri, a1 - a0)
      System.arraycopy(av, a0, rv, ri, a1 - a0)
      ri += a1 - a0
    }

    def fromB(ai: Int, b0: Int, b1: Int) = {
      System.arraycopy(bk, b0, rk, ri, b1 - b0)
      System.arraycopy(bv, b0, rv, ri, b1 - b0)
      ri += b1 - b0
    }

    def collision(ai: Int, bi: Int) = {
      val r = f(av(ai), bv(bi))
      if(Eq.neqv(r, rd)) {
        rk(ri) = bk(bi)
        rv(ri) = r
        ri += 1
      }
    }

    merge0(0, ak.length, 0, bk.length)

    def result: TotalArrayMap[K, V] = new TotalArrayMap[K, V](rk.resizeInPlace(ri), rv.resizeInPlace(ri), rd)
  }

  private[abc] def compare[@sp(ILD) K: Order, @sp(ILD) V: Order](x: TotalArrayMap[K, V], y: TotalArrayMap[K, V]): Int =
    new Compare[K, V](x, y).merge()

  private final class Compare[@sp(ILD) K: Order, @sp(ILD) V: Order](a: TotalArrayMap[K, V], b: TotalArrayMap[K, V])
    extends BinaryMerge {
    // require(Eq.eqv(a.default, b.default))

    @inline def ak = a.keys0
    @inline def av = a.values0
    @inline def bk = b.keys0
    @inline def bv = b.values0
    var r: Int = 0

    def compare(ai: Int, bi: Int) = Order.compare(ak(ai), bk(bi))

    def fromA(a0: Int, a1: Int, bi: Int) = {
      // merge0 only gets called if a.default === b.default
      // av(a0) must be =!= a.default and thus =!= b.default
      // so this comparison will never return 0, and we can immediately abort
      r = Order.compare(av(a0), b.default)
      throw abort
    }

    def fromB(ai: Int, b0: Int, b1: Int) = {
      // merge0 only gets called if a.default === b.default
      // bv(b0) must be =!= b.default and thus =!= a.default
      // so this comparison will never return 0, and we can immediately abort
      r = Order.compare(a.default, bv(b0))
      throw abort
    }

    def collision(ai: Int, bi: Int) = {
      r = Order.compare(av(ai), bv(bi))
      if(r != 0)
        throw abort
    }

    def merge(): Int = {
      r = Order.compare(a.default, b.default)
      if (r == 0)
        try {
          merge0(0, ak.length, 0, bk.length)
        } catch {
          case x: AbortControl ⇒
        }
      r
    }
  }

  def fromDefault[K, V](default: V): TotalArrayMap[K, V] =
    new TotalArrayMap(ArrayFactory.empty[K], ArrayFactory.empty[V], default)
}
