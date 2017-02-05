package com.rklaehn.abc

import org.scalacheck.Cogen

object cogen {

  implicit def cogenArraySeq[T: Cogen]: Cogen[ArraySeq[T]] =
    Cogen.it[ArraySeq[T], T](_.elements.iterator)

  implicit def cogenArraySet[T: Cogen]: Cogen[ArraySet[T]] =
    Cogen[ArraySeq[T]].contramap[ArraySet[T]](_.asArraySeq)

  implicit def cogenNegatableArraySet[T: Cogen]: Cogen[NegatableArraySet[T]] =
    Cogen[ArraySeq[T]].contramap[NegatableArraySet[T]](_.elements.asArraySeq)

  implicit def cogenTotalArraySeq[T: Cogen]: Cogen[TotalArraySeq[T]] =
    Cogen[ArraySeq[T]].contramap[TotalArraySeq[T]](_.withoutDefault)

  implicit def cogenArrayMap[K: Cogen, V:Cogen]: Cogen[ArrayMap[K, V]] =
    Cogen[(ArraySet[K], ArraySeq[V])].contramap[ArrayMap[K, V]](am => (am.keys, am.values))

  implicit def cogenTotalArrayMap[K: Cogen, V:Cogen]: Cogen[TotalArrayMap[K, V]] =
    Cogen[(ArraySet[K], ArraySeq[V])].contramap[TotalArrayMap[K, V]](am => (am.keys, am.values))
}
