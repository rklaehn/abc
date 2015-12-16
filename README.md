[![Build Status](https://travis-ci.org/rklaehn/abc.png)](https://travis-ci.org/rklaehn/abc)
[![codecov.io](http://codecov.io/github/rklaehn/abc/coverage.svg?branch=master)](http://codecov.io/github/rklaehn/abc?branch=master)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.rklaehn/abc_2.11/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.rklaehn/abc_2.11)

# Array-based collections

Array-based immutable collections for scala. 

## Typeclass-friendly design

These collections *use* [algebra] typeclasses such as [Eq] and [Order], and [cats] typeclasses such as [Show] instead of relying on the equals method of the element objects, which sometimes does not work (e.g. `Array[Byte]`) or does not make sense (`Function1[A, B]`).

They also *provide* typeclass instances for as many [algebra] and [cats] typeclasses as possible.

## Specialization of operations

Most operations are specialized for common types (currently Int, Long, Double), so the specialized instances of typeclasses such as [Order] can be used to avoid boxing.

## Compact in-memory representation

On modern CPUs, cache concerns are *very* important. So a compact in-memory representation is often more 
important for good overall performance than optimal big-O behavior. So in this library, compact in-memory representation
is ***always*** given priority over reference-heavy trees with theoretically optimal Big-O performance.

This yields very good results regarding compactness and performance. The downside is that you have to provide ClassTag instances for almost every operation.

## Bulk operations

The scala collections in the standard library mostly implement collection/collection operations in terms of
collection/element operations. The approach taken in this library is the opposite: focus on collection/collection
operations and to implement collection/element operations in terms of collection/collection operations
whenever possible. E.g. adding an element *e* to a set *a* will be done by merging *a* with a
single-element set created from *e*.

Using flat arrays internally is *very* inefficient when e.g. adding elements one by one to a large collection. But when working with collections in a functional way, this is a pretty rare operation. Usually you apply transformations to the collection as a whole. For that use case, the array-based internal representation is very efficient.

### Compatibility with scala collections

An optional interface to scala collections is provided, but the collections itself are not integrated
into the scala collections hierarchy. They do not even implement methods such as equals and hashcode. ***In fact they will throw an UnsupportedOperationException when you use == or hashCode***
They implement toString on a best-effort basis, but formatting should be done using a Show typeclass from cats.

## Implemented collections

         |          |                  |
---------|----------|------------------|
Sequence | ArraySeq | TotalArraySeq    | 
Set      | ArraySet | NegatableArraySet|
Map      | ArrayMap | TotalArrayMap    |

### ArraySeq[A]

Basically just an array wrapped for immutability. Specialized for fast primitive access

Provided typeclasses:

- [Eq]
- [Hash]
- [Show]
- [Monoid] (empty / concat)
- Foldable

### TotalArraySeq[A]

A wrapped array with a default value, so that the `apply(index: Int)` method is total. Having a total apply function allows to define many more typeclasses

Provided typeclasses:

- Eq
- Hash
- Order
- Semigroup
- Monoid
- Group
- AdditiveSemigroup
- AdditiveMonoid
- AdditiveGroup
- Semiring
- Rig

### ArraySet[A]

A set backed by a sorted array. The internal representation is extremely compact, especially when using primitives. All boolean operations (union, intersect, diff, xor, subsetOf, intersects) are implemented efficiently.

Provided typeclasses:

- Eq
- Hash
- Show
- PartialOrder
- Semiring
- Foldable
- (GenBool once it becomes available)

### NegatableArraySet[K, V]

A set by a sorted array, with an additional flag to allow negation. The additional flag allows implementing the full Bool typeclass. The internal representation is extremely compact, especially when using primitives.

Provided typeclasses:

- Eq
- Bool

### ArrayMap[K, V]

A map backed by a sorted array of keys and a corresponding array of values. The internal representation is extremely compact, especially when using primitives. 

Provided typeclasses:

- Eq
- Hash
- Show
- Monoid
- AdditiveMonoid

### TotalArrayMap[K, V]

A map with default value, so that the apply method is total (hence the name). This map does not have as many operations as ArrayMap[K, V], but you can convert a TotalArrayMap[K, V] back to an ArrayMap[K, V] in O(1).

Provided typeclasses:

- Eq
- Hash
- Show
- Group
- AdditiveGroup
- MultiplicativeGroup
- Semiring
- Rig

### ArrayBiMap[K, V]

Provided typeclasses:

- Hash

### ArrayMultiMap[K, V]

Provided typeclasses:

- Eq
- Show
- Hash

### ArrayBiMultiMap[K, V]

Provided typeclasses:

- Eq

#
<a name="hash"></a>

Unfortunately, [algebra] currently does not contain a typeclass for hashing. The hashing typeclass in the scala library does not follow current conventions for typeclasses. Therefore, `Hash[T]` is currently defined [internally](https://github.com/rklaehn/abc/blob/master/core/src/main/scala/com/rklaehn/abc/Hash.scala).

[algebra]: https://github.com/non/algebra
[Eq]: https://github.com/non/algebra/blob/master/core/src/main/scala/algebra/Eq.scala
[Order]: https://github.com/non/algebra/blob/master/core/src/main/scala/algebra/Order.scala
[Monoid]: https://github.com/non/algebra/blob/master/core/src/main/scala/algebra/Monoid.scala

[cats]: https://github.com/non/cats
[Show]: https://github.com/non/cats/blob/master/core/src/main/scala/cats/Show.scala
[Hash]: #hash
