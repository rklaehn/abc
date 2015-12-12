# Array-based collections

Array-based immutable collections for scala. These collections use spire typeclasses such as Eq and Order instead of relying on the equals method of the element objects, which sometimes does not work (e.g. Array[Byte]).

They also are effectively specialized, so e.g. an ArraySet[Byte] will use an Array[Byte] internally instead of boxing like normal scala collections do.

## Design goals

### Typeclasses

The main purpose of this library is to explore how a collection library could be set up to rely mostly on
typeclasses instead of using inheritance.

An optional interface to scala collections will be provided, but the collections itself are not integrated
into the scala collections hierarchy. They do not even implement methods such as equals and hashcode.
They implement toString on a best-effort basis, but formatting should be done using a Show typeclass.

### Bulk operations

The existing scala collections mostly implement collection/collection operations in terms of
collection/element operations. The approach taken in this library is to focus on collection/collection
operations and to implement collection/element operations in terms of collection/collection operations
whenever possible. E.g. adding an element *e* to a set *a* will be done by merging *a* with a
single-element set created from *e*.

### Compact in memory representation

On modern CPUs, cache concerns are *very* important. So a compact in-memory representation is often more 
important for good overall performance than optimal big-O behavior. So compact in-memory representation
will be given priority over optimal big-O behavior.

## Implemented collections

### ArraySeq[A]

### ArraySet[A]

### ArrayMap[K, V]

### NegatableArraySet[K, V]

### ArrayBiMap[K, V]

### ArrayMultiMap[K, V]

### ArrayBiMultiMap[K, V]
